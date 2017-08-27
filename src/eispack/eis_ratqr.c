/* ratqr.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int ratqr_(integer *n, doublereal *eps1, doublereal *d__, 
	doublereal *e, doublereal *e2, integer *m, doublereal *w, integer *
	ind, doublereal *bd, logical *type__, integer *idef, integer *ierr)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2, d__3;

    /* Local variables */
    integer jdef;
    doublereal f;
    integer i__, j, k;
    doublereal p, q, r__, s, delta;
    integer k1, ii, jj;
    doublereal ep, qp=0.0;
    extern doublereal epslon_(doublereal *);
    doublereal err, tot;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE RATQR, */
/*     NUM. MATH. 11, 264-272(1968) BY REINSCH AND BAUER. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 257-265(1971). */

/*     THIS SUBROUTINE FINDS THE ALGEBRAICALLY SMALLEST OR LARGEST */
/*     EIGENVALUES OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE */
/*     RATIONAL QR METHOD WITH NEWTON CORRECTIONS. */

/*     ON INPUT */

/*        N IS THE ORDER OF THE MATRIX. */

/*        EPS1 IS A THEORETICAL ABSOLUTE ERROR TOLERANCE FOR THE */
/*          COMPUTED EIGENVALUES.  IF THE INPUT EPS1 IS NON-POSITIVE, */
/*          OR INDEED SMALLER THAN ITS DEFAULT VALUE, IT IS RESET */
/*          AT EACH ITERATION TO THE RESPECTIVE DEFAULT VALUE, */
/*          NAMELY, THE PRODUCT OF THE RELATIVE MACHINE PRECISION */
/*          AND THE MAGNITUDE OF THE CURRENT EIGENVALUE ITERATE. */
/*          THE THEORETICAL ABSOLUTE ERROR IN THE K-TH EIGENVALUE */
/*          IS USUALLY NOT GREATER THAN K TIMES EPS1. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX */
/*          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY. */

/*        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E. */
/*          E2(1) IS ARBITRARY. */

/*        M IS THE NUMBER OF EIGENVALUES TO BE FOUND. */

/*        IDEF SHOULD BE SET TO 1 IF THE INPUT MATRIX IS KNOWN TO BE */
/*          POSITIVE DEFINITE, TO -1 IF THE INPUT MATRIX IS KNOWN TO */
/*          BE NEGATIVE DEFINITE, AND TO 0 OTHERWISE. */

/*        TYPE SHOULD BE SET TO .TRUE. IF THE SMALLEST EIGENVALUES */
/*          ARE TO BE FOUND, AND TO .FALSE. IF THE LARGEST EIGENVALUES */
/*          ARE TO BE FOUND. */

/*     ON OUTPUT */

/*        EPS1 IS UNALTERED UNLESS IT HAS BEEN RESET TO ITS */
/*          (LAST) DEFAULT VALUE. */

/*        D AND E ARE UNALTERED (UNLESS W OVERWRITES D). */

/*        ELEMENTS OF E2, CORRESPONDING TO ELEMENTS OF E REGARDED */
/*          AS NEGLIGIBLE, HAVE BEEN REPLACED BY ZERO CAUSING THE */
/*          MATRIX TO SPLIT INTO A DIRECT SUM OF SUBMATRICES. */
/*          E2(1) IS SET TO 0.0D0 IF THE SMALLEST EIGENVALUES HAVE BEEN */
/*          FOUND, AND TO 2.0D0 IF THE LARGEST EIGENVALUES HAVE BEEN */
/*          FOUND.  E2 IS OTHERWISE UNALTERED (UNLESS OVERWRITTEN BY BD). 
*/

/*        W CONTAINS THE M ALGEBRAICALLY SMALLEST EIGENVALUES IN */
/*          ASCENDING ORDER, OR THE M LARGEST EIGENVALUES IN */
/*          DESCENDING ORDER.  IF AN ERROR EXIT IS MADE BECAUSE OF */
/*          AN INCORRECT SPECIFICATION OF IDEF, NO EIGENVALUES */
/*          ARE FOUND.  IF THE NEWTON ITERATES FOR A PARTICULAR */
/*          EIGENVALUE ARE NOT MONOTONE, THE BEST ESTIMATE OBTAINED */
/*          IS RETURNED AND IERR IS SET.  W MAY COINCIDE WITH D. */

/*        IND CONTAINS IN ITS FIRST M POSITIONS THE SUBMATRIX INDICES */
/*          ASSOCIATED WITH THE CORRESPONDING EIGENVALUES IN W -- */
/*          1 FOR EIGENVALUES BELONGING TO THE FIRST SUBMATRIX FROM */
/*          THE TOP, 2 FOR THOSE BELONGING TO THE SECOND SUBMATRIX, ETC.. 
*/

/*        BD CONTAINS REFINED BOUNDS FOR THE THEORETICAL ERRORS OF THE */
/*          CORRESPONDING EIGENVALUES IN W.  THESE BOUNDS ARE USUALLY */
/*          WITHIN THE TOLERANCE SPECIFIED BY EPS1.  BD MAY COINCIDE */
/*          WITH E2. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          6*N+1      IF  IDEF  IS SET TO 1 AND  TYPE  TO .TRUE. */
/*                     WHEN THE MATRIX IS NOT POSITIVE DEFINITE, OR */
/*                     IF  IDEF  IS SET TO -1 AND  TYPE  TO .FALSE. */
/*                     WHEN THE MATRIX IS NOT NEGATIVE DEFINITE, */
/*          5*N+K      IF SUCCESSIVE ITERATES TO THE K-TH EIGENVALUE */
/*                     ARE NOT MONOTONE INCREASING, WHERE K REFERS */
/*                     TO THE LAST SUCH OCCURRENCE. */

/*     NOTE THAT SUBROUTINE TRIDIB IS GENERALLY FASTER AND MORE */
/*     ACCURATE THAN RATQR IF THE EIGENVALUES ARE CLUSTERED. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --bd;
    --ind;
    --w;
    --e2;
    --e;
    --d__;

    /* Function Body */
    *ierr = 0;
    jdef = *idef;
/*     .......... COPY D ARRAY INTO W .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L20: */
	w[i__] = d__[i__];
    }

    if (*type__) {
	goto L40;
    }
    j = 1;
    goto L400;
L40:
    err = 0.;
    s = 0.;
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ENTRIES AND DEFINE */
/*                INITIAL SHIFT FROM LOWER GERSCHGORIN BOUND. */
/*                COPY E2 ARRAY INTO BD .......... */
    tot = w[1];
    q = 0.;
    j = 0;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	p = q;
	if (i__ == 1) {
	    goto L60;
	}
	d__3 = (d__1 = d__[i__], abs(d__1)) + (d__2 = d__[i__ - 1], abs(d__2))
		;
	if (p > epslon_(&d__3)) {
	    goto L80;
	}
L60:
	e2[i__] = 0.;
L80:
	bd[i__] = e2[i__];
/*     .......... COUNT ALSO IF ELEMENT OF E2 HAS UNDERFLOWED ........
.. */
	if (e2[i__] == 0.) {
	    ++j;
	}
	ind[i__] = j;
	q = 0.;
	if (i__ != *n) {
	    q = (d__1 = e[i__ + 1], abs(d__1));
	}
/* Computing MIN */
	d__1 = w[i__] - p - q;
	tot = min(d__1,tot);
/* L100: */
    }

    if (jdef == 1 && tot < 0.) {
	goto L140;
    }

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L110: */
	w[i__] -= tot;
    }

    goto L160;
L140:
    tot = 0.;

L160:
    i__1 = *m;
    for (k = 1; k <= i__1; ++k) {
/*     .......... NEXT QR TRANSFORMATION .......... */
L180:
	tot += s;
	delta = w[*n] - s;
	i__ = *n;
	f = (d__1 = epslon_(&tot), abs(d__1));
	if (*eps1 < f) {
	    *eps1 = f;
	}
	if (delta > *eps1) {
	    goto L190;
	}
	if (delta < -(*eps1)) {
	    goto L1000;
	}
	goto L300;
/*     .......... REPLACE SMALL SUB-DIAGONAL SQUARES BY ZERO */
/*                TO REDUCE THE INCIDENCE OF UNDERFLOWS .......... */
L190:
	if (k == *n) {
	    goto L210;
	}
	k1 = k + 1;
	i__2 = *n;
	for (j = k1; j <= i__2; ++j) {
	    d__2 = w[j] + w[j - 1];
/* Computing 2nd power */
	    d__1 = epslon_(&d__2);
	    if (bd[j] <= d__1 * d__1) {
		bd[j] = 0.;
	    }
/* L200: */
	}

L210:
	f = bd[*n] / delta;
	qp = delta + f;
	p = 1.;
	if (k == *n) {
	    goto L260;
	}
	k1 = *n - k;
/*     .......... FOR I=N-1 STEP -1 UNTIL K DO -- .......... */
	i__2 = k1;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = *n - ii;
	    q = w[i__] - s - f;
	    r__ = q / qp;
	    p = p * r__ + 1.;
	    ep = f * r__;
	    w[i__ + 1] = qp + ep;
	    delta = q - ep;
	    if (delta > *eps1) {
		goto L220;
	    }
	    if (delta < -(*eps1)) {
		goto L1000;
	    }
	    goto L300;
L220:
	    f = bd[i__] / q;
	    qp = delta + f;
	    bd[i__ + 1] = qp * ep;
/* L240: */
	}

L260:
	w[k] = qp;
	s = qp / p;
	if (tot + s > tot) {
	    goto L180;
	}
/*     .......... SET ERROR -- IRREGULAR END OF ITERATION. */
/*                DEFLATE MINIMUM DIAGONAL ELEMENT .......... */
	*ierr = *n * 5 + k;
	s = 0.;
	delta = qp;

	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    if (w[j] > delta) {
		goto L280;
	    }
	    i__ = j;
	    delta = w[j];
L280:
	    ;
	}
/*     .......... CONVERGENCE .......... */
L300:
	if (i__ < *n) {
	    bd[i__ + 1] = bd[i__] * f / qp;
	}
	ii = ind[i__];
	if (i__ == k) {
	    goto L340;
	}
	k1 = i__ - k;
/*     .......... FOR J=I-1 STEP -1 UNTIL K DO -- .......... */
	i__2 = k1;
	for (jj = 1; jj <= i__2; ++jj) {
	    j = i__ - jj;
	    w[j + 1] = w[j] - s;
	    bd[j + 1] = bd[j];
	    ind[j + 1] = ind[j];
/* L320: */
	}

L340:
	w[k] = tot;
	err += abs(delta);
	bd[k] = err;
	ind[k] = ii;
/* L360: */
    }

    if (*type__) {
	goto L1001;
    }
    f = bd[1];
    e2[1] = 2.;
    bd[1] = f;
    j = 2;
/*     .......... NEGATE ELEMENTS OF W FOR LARGEST VALUES .......... */
L400:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L500: */
	w[i__] = -w[i__];
    }

    jdef = -jdef;
    switch (j) {
	case 1:  goto L40;
	case 2:  goto L1001;
    }
/*     .......... SET ERROR -- IDEF SPECIFIED INCORRECTLY .......... */
L1000:
    *ierr = *n * 6 + 1;
L1001:
    return 0;
} /* ratqr_ */

