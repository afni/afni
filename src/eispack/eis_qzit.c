/* qzit.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static doublereal c_b5 = 1.;

/* Subroutine */ int qzit_(integer *nm, integer *n, doublereal *a, doublereal 
	*b, doublereal *eps1, logical *matz, doublereal *z__, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, 
	    i__3;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal epsa, epsb;
    integer i__, j, k, l=0;
    doublereal r__, s, t, anorm, bnorm;
    integer enorn;
    doublereal a1, a2, a3=0.0;
    integer k1, k2, l1;
    doublereal u1, u2, u3, v1, v2, v3, a11, a12, a21, a22, a33, a34, 
	    a43, a44, b11, b12, b22, b33;
    integer na, ld;
    doublereal b34, b44;
    integer en;
    doublereal ep;
    integer ll;
    doublereal sh=0.0;
    extern doublereal epslon_(doublereal *);
    logical notlas;
    integer km1, lm1=0;
    doublereal ani, bni;
    integer ish, itn, its, enm2, lor1;



/*     THIS SUBROUTINE IS THE SECOND STEP OF THE QZ ALGORITHM */
/*     FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS, */
/*     SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART, */
/*     AS MODIFIED IN TECHNICAL NOTE NASA TN D-7305(1973) BY WARD. */

/*     THIS SUBROUTINE ACCEPTS A PAIR OF REAL MATRICES, ONE OF THEM */
/*     IN UPPER HESSENBERG FORM AND THE OTHER IN UPPER TRIANGULAR FORM. */
/*     IT REDUCES THE HESSENBERG MATRIX TO QUASI-TRIANGULAR FORM USING */
/*     ORTHOGONAL TRANSFORMATIONS WHILE MAINTAINING THE TRIANGULAR FORM */
/*     OF THE OTHER MATRIX.  IT IS USUALLY PRECEDED BY  QZHES  AND */
/*     FOLLOWED BY  QZVAL  AND, POSSIBLY,  QZVEC. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRICES. */

/*        A CONTAINS A REAL UPPER HESSENBERG MATRIX. */

/*        B CONTAINS A REAL UPPER TRIANGULAR MATRIX. */

/*        EPS1 IS A TOLERANCE USED TO DETERMINE NEGLIGIBLE ELEMENTS. */
/*          EPS1 = 0.0 (OR NEGATIVE) MAY BE INPUT, IN WHICH CASE AN */
/*          ELEMENT WILL BE NEGLECTED ONLY IF IT IS LESS THAN ROUNDOFF */
/*          ERROR TIMES THE NORM OF ITS MATRIX.  IF THE INPUT EPS1 IS */
/*          POSITIVE, THEN AN ELEMENT WILL BE CONSIDERED NEGLIGIBLE */
/*          IF IT IS LESS THAN EPS1 TIMES THE NORM OF ITS MATRIX.  A */
/*          POSITIVE VALUE OF EPS1 MAY RESULT IN FASTER EXECUTION, */
/*          BUT LESS ACCURATE RESULTS. */

/*        MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS 
*/
/*          ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING */
/*          EIGENVECTORS, AND TO .FALSE. OTHERWISE. */

/*        Z CONTAINS, IF MATZ HAS BEEN SET TO .TRUE., THE */
/*          TRANSFORMATION MATRIX PRODUCED IN THE REDUCTION */
/*          BY  QZHES, IF PERFORMED, OR ELSE THE IDENTITY MATRIX. */
/*          IF MATZ HAS BEEN SET TO .FALSE., Z IS NOT REFERENCED. */

/*     ON OUTPUT */

/*        A HAS BEEN REDUCED TO QUASI-TRIANGULAR FORM.  THE ELEMENTS */
/*          BELOW THE FIRST SUBDIAGONAL ARE STILL ZERO AND NO TWO */
/*          CONSECUTIVE SUBDIAGONAL ELEMENTS ARE NONZERO. */

/*        B IS STILL IN UPPER TRIANGULAR FORM, ALTHOUGH ITS ELEMENTS */
/*          HAVE BEEN ALTERED.  THE LOCATION B(N,1) IS USED TO STORE */
/*          EPS1 TIMES THE NORM OF B FOR LATER USE BY  QZVAL  AND  QZVEC. 
*/

/*        Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS */
/*          (FOR BOTH STEPS) IF MATZ HAS BEEN SET TO .TRUE.. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED */
/*                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    b_dim1 = *nm;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    *ierr = 0;
/*     .......... COMPUTE EPSA,EPSB .......... */
    anorm = 0.;
    bnorm = 0.;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ani = 0.;
	if (i__ != 1) {
	    ani = (d__1 = a[i__ + (i__ - 1) * a_dim1], abs(d__1));
	}
	bni = 0.;

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    ani += (d__1 = a[i__ + j * a_dim1], abs(d__1));
	    bni += (d__1 = b[i__ + j * b_dim1], abs(d__1));
/* L20: */
	}

	if (ani > anorm) {
	    anorm = ani;
	}
	if (bni > bnorm) {
	    bnorm = bni;
	}
/* L30: */
    }

    if (anorm == 0.) {
	anorm = 1.;
    }
    if (bnorm == 0.) {
	bnorm = 1.;
    }
    ep = *eps1;
    if (ep > 0.) {
	goto L50;
    }
/*     .......... USE ROUNDOFF LEVEL IF EPS1 IS ZERO .......... */
    ep = epslon_(&c_b5);
L50:
    epsa = ep * anorm;
    epsb = ep * bnorm;
/*     .......... REDUCE A TO QUASI-TRIANGULAR FORM, WHILE */
/*                KEEPING B TRIANGULAR .......... */
    lor1 = 1;
    enorn = *n;
    en = *n;
    itn = *n * 30;
/*     .......... BEGIN QZ STEP .......... */
L60:
    if (en <= 2) {
	goto L1001;
    }
    if (! (*matz)) {
	enorn = en;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
L70:
    ish = 2;
/*     .......... CHECK FOR CONVERGENCE OR REDUCIBILITY. */
/*                FOR L=EN STEP -1 UNTIL 1 DO -- .......... */
    i__1 = en;
    for (ll = 1; ll <= i__1; ++ll) {
	lm1 = en - ll;
	l = lm1 + 1;
	if (l == 1) {
	    goto L95;
	}
	if ((d__1 = a[l + lm1 * a_dim1], abs(d__1)) <= epsa) {
	    goto L90;
	}
/* L80: */
    }

L90:
    a[l + lm1 * a_dim1] = 0.;
    if (l < na) {
	goto L95;
    }
/*     .......... 1-BY-1 OR 2-BY-2 BLOCK ISOLATED .......... */
    en = lm1;
    goto L60;
/*     .......... CHECK FOR SMALL TOP OF B .......... */
L95:
    ld = l;
L100:
    l1 = l + 1;
    b11 = b[l + l * b_dim1];
    if (abs(b11) > epsb) {
	goto L120;
    }
    b[l + l * b_dim1] = 0.;
    s = (d__1 = a[l + l * a_dim1], abs(d__1)) + (d__2 = a[l1 + l * a_dim1], 
	    abs(d__2));
    u1 = a[l + l * a_dim1] / s;
    u2 = a[l1 + l * a_dim1] / s;
    d__1 = sqrt(u1 * u1 + u2 * u2);
    r__ = d_sign(&d__1, &u1);
    v1 = -(u1 + r__) / r__;
    v2 = -u2 / r__;
    u2 = v2 / v1;

    i__1 = enorn;
    for (j = l; j <= i__1; ++j) {
	t = a[l + j * a_dim1] + u2 * a[l1 + j * a_dim1];
	a[l + j * a_dim1] += t * v1;
	a[l1 + j * a_dim1] += t * v2;
	t = b[l + j * b_dim1] + u2 * b[l1 + j * b_dim1];
	b[l + j * b_dim1] += t * v1;
	b[l1 + j * b_dim1] += t * v2;
/* L110: */
    }

    if (l != 1) {
	a[l + lm1 * a_dim1] = -a[l + lm1 * a_dim1];
    }
    lm1 = l;
    l = l1;
    goto L90;
L120:
    a11 = a[l + l * a_dim1] / b11;
    a21 = a[l1 + l * a_dim1] / b11;
    if (ish == 1) {
	goto L140;
    }
/*     .......... ITERATION STRATEGY .......... */
    if (itn == 0) {
	goto L1000;
    }
    if (its == 10) {
	goto L155;
    }
/*     .......... DETERMINE TYPE OF SHIFT .......... */
    b22 = b[l1 + l1 * b_dim1];
    if (abs(b22) < epsb) {
	b22 = epsb;
    }
    b33 = b[na + na * b_dim1];
    if (abs(b33) < epsb) {
	b33 = epsb;
    }
    b44 = b[en + en * b_dim1];
    if (abs(b44) < epsb) {
	b44 = epsb;
    }
    a33 = a[na + na * a_dim1] / b33;
    a34 = a[na + en * a_dim1] / b44;
    a43 = a[en + na * a_dim1] / b33;
    a44 = a[en + en * a_dim1] / b44;
    b34 = b[na + en * b_dim1] / b44;
    t = (a43 * b34 - a33 - a44) * .5;
    r__ = t * t + a34 * a43 - a33 * a44;
    if (r__ < 0.) {
	goto L150;
    }
/*     .......... DETERMINE SINGLE SHIFT ZEROTH COLUMN OF A .......... */
    ish = 1;
    r__ = sqrt(r__);
    sh = -t + r__;
    s = -t - r__;
    if ((d__1 = s - a44, abs(d__1)) < (d__2 = sh - a44, abs(d__2))) {
	sh = s;
    }
/*     .......... LOOK FOR TWO CONSECUTIVE SMALL */
/*                SUB-DIAGONAL ELEMENTS OF A. */
/*                FOR L=EN-2 STEP -1 UNTIL LD DO -- .......... */
    i__1 = enm2;
    for (ll = ld; ll <= i__1; ++ll) {
	l = enm2 + ld - ll;
	if (l == ld) {
	    goto L140;
	}
	lm1 = l - 1;
	l1 = l + 1;
	t = a[l + l * a_dim1];
	if ((d__1 = b[l + l * b_dim1], abs(d__1)) > epsb) {
	    t -= sh * b[l + l * b_dim1];
	}
	if ((d__1 = a[l + lm1 * a_dim1], abs(d__1)) <= (d__2 = t / a[l1 + l * 
		a_dim1], abs(d__2)) * epsa) {
	    goto L100;
	}
/* L130: */
    }

L140:
    a1 = a11 - sh;
    a2 = a21;
    if (l != ld) {
	a[l + lm1 * a_dim1] = -a[l + lm1 * a_dim1];
    }
    goto L160;
/*     .......... DETERMINE DOUBLE SHIFT ZEROTH COLUMN OF A .......... */
L150:
    a12 = a[l + l1 * a_dim1] / b22;
    a22 = a[l1 + l1 * a_dim1] / b22;
    b12 = b[l + l1 * b_dim1] / b22;
    a1 = ((a33 - a11) * (a44 - a11) - a34 * a43 + a43 * b34 * a11) / a21 + 
	    a12 - a11 * b12;
    a2 = a22 - a11 - a21 * b12 - (a33 - a11) - (a44 - a11) + a43 * b34;
    a3 = a[l1 + 1 + l1 * a_dim1] / b22;
    goto L160;
/*     .......... AD HOC SHIFT .......... */
L155:
    a1 = 0.;
    a2 = 1.;
    a3 = 1.1605;
L160:
    ++its;
    --itn;
    if (! (*matz)) {
	lor1 = ld;
    }
/*     .......... MAIN LOOP .......... */
    i__1 = na;
    for (k = l; k <= i__1; ++k) {
	notlas = k != na && ish == 2;
	k1 = k + 1;
	k2 = k + 2;
/* Computing MAX */
	i__2 = k - 1;
	km1 = max(i__2,l);
/* Computing MIN */
	i__2 = en, i__3 = k1 + ish;
	ll = min(i__2,i__3);
	if (notlas) {
	    goto L190;
	}
/*     .......... ZERO A(K+1,K-1) .......... */
	if (k == l) {
	    goto L170;
	}
	a1 = a[k + km1 * a_dim1];
	a2 = a[k1 + km1 * a_dim1];
L170:
	s = abs(a1) + abs(a2);
	if (s == 0.) {
	    goto L70;
	}
	u1 = a1 / s;
	u2 = a2 / s;
	d__1 = sqrt(u1 * u1 + u2 * u2);
	r__ = d_sign(&d__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	u2 = v2 / v1;

	i__2 = enorn;
	for (j = km1; j <= i__2; ++j) {
	    t = a[k + j * a_dim1] + u2 * a[k1 + j * a_dim1];
	    a[k + j * a_dim1] += t * v1;
	    a[k1 + j * a_dim1] += t * v2;
	    t = b[k + j * b_dim1] + u2 * b[k1 + j * b_dim1];
	    b[k + j * b_dim1] += t * v1;
	    b[k1 + j * b_dim1] += t * v2;
/* L180: */
	}

	if (k != l) {
	    a[k1 + km1 * a_dim1] = 0.;
	}
	goto L240;
/*     .......... ZERO A(K+1,K-1) AND A(K+2,K-1) .......... */
L190:
	if (k == l) {
	    goto L200;
	}
	a1 = a[k + km1 * a_dim1];
	a2 = a[k1 + km1 * a_dim1];
	a3 = a[k2 + km1 * a_dim1];
L200:
	s = abs(a1) + abs(a2) + abs(a3);
	if (s == 0.) {
	    goto L260;
	}
	u1 = a1 / s;
	u2 = a2 / s;
	u3 = a3 / s;
	d__1 = sqrt(u1 * u1 + u2 * u2 + u3 * u3);
	r__ = d_sign(&d__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	v3 = -u3 / r__;
	u2 = v2 / v1;
	u3 = v3 / v1;

	i__2 = enorn;
	for (j = km1; j <= i__2; ++j) {
	    t = a[k + j * a_dim1] + u2 * a[k1 + j * a_dim1] + u3 * a[k2 + j * 
		    a_dim1];
	    a[k + j * a_dim1] += t * v1;
	    a[k1 + j * a_dim1] += t * v2;
	    a[k2 + j * a_dim1] += t * v3;
	    t = b[k + j * b_dim1] + u2 * b[k1 + j * b_dim1] + u3 * b[k2 + j * 
		    b_dim1];
	    b[k + j * b_dim1] += t * v1;
	    b[k1 + j * b_dim1] += t * v2;
	    b[k2 + j * b_dim1] += t * v3;
/* L210: */
	}

	if (k == l) {
	    goto L220;
	}
	a[k1 + km1 * a_dim1] = 0.;
	a[k2 + km1 * a_dim1] = 0.;
/*     .......... ZERO B(K+2,K+1) AND B(K+2,K) .......... */
L220:
	s = (d__1 = b[k2 + k2 * b_dim1], abs(d__1)) + (d__2 = b[k2 + k1 * 
		b_dim1], abs(d__2)) + (d__3 = b[k2 + k * b_dim1], abs(d__3));
	if (s == 0.) {
	    goto L240;
	}
	u1 = b[k2 + k2 * b_dim1] / s;
	u2 = b[k2 + k1 * b_dim1] / s;
	u3 = b[k2 + k * b_dim1] / s;
	d__1 = sqrt(u1 * u1 + u2 * u2 + u3 * u3);
	r__ = d_sign(&d__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	v3 = -u3 / r__;
	u2 = v2 / v1;
	u3 = v3 / v1;

	i__2 = ll;
	for (i__ = lor1; i__ <= i__2; ++i__) {
	    t = a[i__ + k2 * a_dim1] + u2 * a[i__ + k1 * a_dim1] + u3 * a[i__ 
		    + k * a_dim1];
	    a[i__ + k2 * a_dim1] += t * v1;
	    a[i__ + k1 * a_dim1] += t * v2;
	    a[i__ + k * a_dim1] += t * v3;
	    t = b[i__ + k2 * b_dim1] + u2 * b[i__ + k1 * b_dim1] + u3 * b[i__ 
		    + k * b_dim1];
	    b[i__ + k2 * b_dim1] += t * v1;
	    b[i__ + k1 * b_dim1] += t * v2;
	    b[i__ + k * b_dim1] += t * v3;
/* L230: */
	}

	b[k2 + k * b_dim1] = 0.;
	b[k2 + k1 * b_dim1] = 0.;
	if (! (*matz)) {
	    goto L240;
	}

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    t = z__[i__ + k2 * z_dim1] + u2 * z__[i__ + k1 * z_dim1] + u3 * 
		    z__[i__ + k * z_dim1];
	    z__[i__ + k2 * z_dim1] += t * v1;
	    z__[i__ + k1 * z_dim1] += t * v2;
	    z__[i__ + k * z_dim1] += t * v3;
/* L235: */
	}
/*     .......... ZERO B(K+1,K) .......... */
L240:
	s = (d__1 = b[k1 + k1 * b_dim1], abs(d__1)) + (d__2 = b[k1 + k * 
		b_dim1], abs(d__2));
	if (s == 0.) {
	    goto L260;
	}
	u1 = b[k1 + k1 * b_dim1] / s;
	u2 = b[k1 + k * b_dim1] / s;
	d__1 = sqrt(u1 * u1 + u2 * u2);
	r__ = d_sign(&d__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	u2 = v2 / v1;

	i__2 = ll;
	for (i__ = lor1; i__ <= i__2; ++i__) {
	    t = a[i__ + k1 * a_dim1] + u2 * a[i__ + k * a_dim1];
	    a[i__ + k1 * a_dim1] += t * v1;
	    a[i__ + k * a_dim1] += t * v2;
	    t = b[i__ + k1 * b_dim1] + u2 * b[i__ + k * b_dim1];
	    b[i__ + k1 * b_dim1] += t * v1;
	    b[i__ + k * b_dim1] += t * v2;
/* L250: */
	}

	b[k1 + k * b_dim1] = 0.;
	if (! (*matz)) {
	    goto L260;
	}

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    t = z__[i__ + k1 * z_dim1] + u2 * z__[i__ + k * z_dim1];
	    z__[i__ + k1 * z_dim1] += t * v1;
	    z__[i__ + k * z_dim1] += t * v2;
/* L255: */
	}

L260:
	;
    }
/*     .......... END QZ STEP .......... */
    goto L70;
/*     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT */
/*                CONVERGED AFTER 30*N ITERATIONS .......... */
L1000:
    *ierr = en;
/*     .......... SAVE EPSB FOR USE BY QZVAL AND QZVEC .......... */
L1001:
    if (*n > 1) {
	b[*n + b_dim1] = epsb;
    }
    return 0;
} /* qzit_ */

