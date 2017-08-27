/* hqr2.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static doublereal c_b49 = 0.;

/* Subroutine */ int hqr2_(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *h__, doublereal *wr, doublereal *wi, doublereal *z__,
	 integer *ierr)
{
    /* System generated locals */
    integer h_dim1, h_offset, z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    extern /* Subroutine */ int cdiv_(doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *);
    doublereal norm;
    integer i__, j, k, l=0, m=0;
    doublereal p, q, r__=0.0, s=0.0, t, w, x, y;
    integer na, ii, en, jj;
    doublereal ra, sa;
    integer ll, mm, nn;
    doublereal vi, vr, zz;
    logical notlas;
    integer mp2, itn, its, enm2;
    doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR2, */
/*     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971). */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS */
/*     OF A REAL UPPER HESSENBERG MATRIX BY THE QR METHOD.  THE */
/*     EIGENVECTORS OF A REAL GENERAL MATRIX CAN ALSO BE FOUND */
/*     IF  ELMHES  AND  ELTRAN  OR  ORTHES  AND  ORTRAN  HAVE */
/*     BEEN USED TO REDUCE THIS GENERAL MATRIX TO HESSENBERG FORM */
/*     AND TO ACCUMULATE THE SIMILARITY TRANSFORMATIONS. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED, */
/*          SET LOW=1, IGH=N. */

/*        H CONTAINS THE UPPER HESSENBERG MATRIX. */

/*        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED BY  ELTRAN */
/*          AFTER THE REDUCTION BY  ELMHES, OR BY  ORTRAN  AFTER THE */
/*          REDUCTION BY  ORTHES, IF PERFORMED.  IF THE EIGENVECTORS */
/*          OF THE HESSENBERG MATRIX ARE DESIRED, Z MUST CONTAIN THE */
/*          IDENTITY MATRIX. */

/*     ON OUTPUT */

/*        H HAS BEEN DESTROYED. */

/*        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES */
/*          ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS */
/*          OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE */
/*          HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN */
/*          ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT */
/*          FOR INDICES IERR+1,...,N. */

/*        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS. */
/*          IF THE I-TH EIGENVALUE IS REAL, THE I-TH COLUMN OF Z */
/*          CONTAINS ITS EIGENVECTOR.  IF THE I-TH EIGENVALUE IS COMPLEX 
*/
/*          WITH POSITIVE IMAGINARY PART, THE I-TH AND (I+1)-TH */
/*          COLUMNS OF Z CONTAIN THE REAL AND IMAGINARY PARTS OF ITS */
/*          EIGENVECTOR.  THE EIGENVECTORS ARE UNNORMALIZED.  IF AN */
/*          ERROR EXIT IS MADE, NONE OF THE EIGENVECTORS HAS BEEN FOUND. 
*/

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED */
/*                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT. */

/*     CALLS CDIV FOR COMPLEX DIVISION. */

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
    --wi;
    --wr;
    h_dim1 = *nm;
    h_offset = h_dim1 + 1;
    h__ -= h_offset;

    /* Function Body */
    *ierr = 0;
    norm = 0.;
    k = 1;
/*     .......... STORE ROOTS ISOLATED BY BALANC */
/*                AND COMPUTE MATRIX NORM .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
/* L40: */
	    norm += (d__1 = h__[i__ + j * h_dim1], abs(d__1));
	}

	k = i__;
	if (i__ >= *low && i__ <= *igh) {
	    goto L50;
	}
	wr[i__] = h__[i__ + i__ * h_dim1];
	wi[i__] = 0.;
L50:
	;
    }

    en = *igh;
    t = 0.;
    itn = *n * 30;
/*     .......... SEARCH FOR NEXT EIGENVALUES .......... */
L60:
    if (en < *low) {
	goto L340;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT */
/*                FOR L=EN STEP -1 UNTIL LOW DO -- .......... */
L70:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L100;
	}
	s = (d__1 = h__[l - 1 + (l - 1) * h_dim1], abs(d__1)) + (d__2 = h__[l 
		+ l * h_dim1], abs(d__2));
	if (s == 0.) {
	    s = norm;
	}
	tst1 = s;
	tst2 = tst1 + (d__1 = h__[l + (l - 1) * h_dim1], abs(d__1));
	if (tst2 == tst1) {
	    goto L100;
	}
/* L80: */
    }
/*     .......... FORM SHIFT .......... */
L100:
    x = h__[en + en * h_dim1];
    if (l == en) {
	goto L270;
    }
    y = h__[na + na * h_dim1];
    w = h__[en + na * h_dim1] * h__[na + en * h_dim1];
    if (l == na) {
	goto L280;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its != 10 && its != 20) {
	goto L130;
    }
/*     .......... FORM EXCEPTIONAL SHIFT .......... */
    t += x;

    i__1 = en;
    for (i__ = *low; i__ <= i__1; ++i__) {
/* L120: */
	h__[i__ + i__ * h_dim1] -= x;
    }

    s = (d__1 = h__[en + na * h_dim1], abs(d__1)) + (d__2 = h__[na + enm2 * 
	    h_dim1], abs(d__2));
    x = s * .75;
    y = x;
    w = s * -.4375 * s;
L130:
    ++its;
    --itn;
/*     .......... LOOK FOR TWO CONSECUTIVE SMALL */
/*                SUB-DIAGONAL ELEMENTS. */
/*                FOR M=EN-2 STEP -1 UNTIL L DO -- .......... */
    i__1 = enm2;
    for (mm = l; mm <= i__1; ++mm) {
	m = enm2 + l - mm;
	zz = h__[m + m * h_dim1];
	r__ = x - zz;
	s = y - zz;
	p = (r__ * s - w) / h__[m + 1 + m * h_dim1] + h__[m + (m + 1) * 
		h_dim1];
	q = h__[m + 1 + (m + 1) * h_dim1] - zz - r__ - s;
	r__ = h__[m + 2 + (m + 1) * h_dim1];
	s = abs(p) + abs(q) + abs(r__);
	p /= s;
	q /= s;
	r__ /= s;
	if (m == l) {
	    goto L150;
	}
	tst1 = abs(p) * ((d__1 = h__[m - 1 + (m - 1) * h_dim1], abs(d__1)) + 
		abs(zz) + (d__2 = h__[m + 1 + (m + 1) * h_dim1], abs(d__2)));
	tst2 = tst1 + (d__1 = h__[m + (m - 1) * h_dim1], abs(d__1)) * (abs(q) 
		+ abs(r__));
	if (tst2 == tst1) {
	    goto L150;
	}
/* L140: */
    }

L150:
    mp2 = m + 2;

    i__1 = en;
    for (i__ = mp2; i__ <= i__1; ++i__) {
	h__[i__ + (i__ - 2) * h_dim1] = 0.;
	if (i__ == mp2) {
	    goto L160;
	}
	h__[i__ + (i__ - 3) * h_dim1] = 0.;
L160:
	;
    }
/*     .......... DOUBLE QR STEP INVOLVING ROWS L TO EN AND */
/*                COLUMNS M TO EN .......... */
    i__1 = na;
    for (k = m; k <= i__1; ++k) {
	notlas = k != na;
	if (k == m) {
	    goto L170;
	}
	p = h__[k + (k - 1) * h_dim1];
	q = h__[k + 1 + (k - 1) * h_dim1];
	r__ = 0.;
	if (notlas) {
	    r__ = h__[k + 2 + (k - 1) * h_dim1];
	}
	x = abs(p) + abs(q) + abs(r__);
	if (x == 0.) {
	    goto L260;
	}
	p /= x;
	q /= x;
	r__ /= x;
L170:
	d__1 = sqrt(p * p + q * q + r__ * r__);
	s = d_sign(&d__1, &p);
	if (k == m) {
	    goto L180;
	}
	h__[k + (k - 1) * h_dim1] = -s * x;
	goto L190;
L180:
	if (l != m) {
	    h__[k + (k - 1) * h_dim1] = -h__[k + (k - 1) * h_dim1];
	}
L190:
	p += s;
	x = p / s;
	y = q / s;
	zz = r__ / s;
	q /= p;
	r__ /= p;
	if (notlas) {
	    goto L225;
	}
/*     .......... ROW MODIFICATION .......... */
	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    p = h__[k + j * h_dim1] + q * h__[k + 1 + j * h_dim1];
	    h__[k + j * h_dim1] -= p * x;
	    h__[k + 1 + j * h_dim1] -= p * y;
/* L200: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... COLUMN MODIFICATION .......... */
	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    p = x * h__[i__ + k * h_dim1] + y * h__[i__ + (k + 1) * h_dim1];
	    h__[i__ + k * h_dim1] -= p;
	    h__[i__ + (k + 1) * h_dim1] -= p * q;
/* L210: */
	}
/*     .......... ACCUMULATE TRANSFORMATIONS .......... */
	i__2 = *igh;
	for (i__ = *low; i__ <= i__2; ++i__) {
	    p = x * z__[i__ + k * z_dim1] + y * z__[i__ + (k + 1) * z_dim1];
	    z__[i__ + k * z_dim1] -= p;
	    z__[i__ + (k + 1) * z_dim1] -= p * q;
/* L220: */
	}
	goto L255;
L225:
/*     .......... ROW MODIFICATION .......... */
	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    p = h__[k + j * h_dim1] + q * h__[k + 1 + j * h_dim1] + r__ * h__[
		    k + 2 + j * h_dim1];
	    h__[k + j * h_dim1] -= p * x;
	    h__[k + 1 + j * h_dim1] -= p * y;
	    h__[k + 2 + j * h_dim1] -= p * zz;
/* L230: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... COLUMN MODIFICATION .......... */
	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    p = x * h__[i__ + k * h_dim1] + y * h__[i__ + (k + 1) * h_dim1] + 
		    zz * h__[i__ + (k + 2) * h_dim1];
	    h__[i__ + k * h_dim1] -= p;
	    h__[i__ + (k + 1) * h_dim1] -= p * q;
	    h__[i__ + (k + 2) * h_dim1] -= p * r__;
/* L240: */
	}
/*     .......... ACCUMULATE TRANSFORMATIONS .......... */
	i__2 = *igh;
	for (i__ = *low; i__ <= i__2; ++i__) {
	    p = x * z__[i__ + k * z_dim1] + y * z__[i__ + (k + 1) * z_dim1] + 
		    zz * z__[i__ + (k + 2) * z_dim1];
	    z__[i__ + k * z_dim1] -= p;
	    z__[i__ + (k + 1) * z_dim1] -= p * q;
	    z__[i__ + (k + 2) * z_dim1] -= p * r__;
/* L250: */
	}
L255:

L260:
	;
    }

    goto L70;
/*     .......... ONE ROOT FOUND .......... */
L270:
    h__[en + en * h_dim1] = x + t;
    wr[en] = h__[en + en * h_dim1];
    wi[en] = 0.;
    en = na;
    goto L60;
/*     .......... TWO ROOTS FOUND .......... */
L280:
    p = (y - x) / 2.;
    q = p * p + w;
    zz = sqrt((abs(q)));
    h__[en + en * h_dim1] = x + t;
    x = h__[en + en * h_dim1];
    h__[na + na * h_dim1] = y + t;
    if (q < 0.) {
	goto L320;
    }
/*     .......... REAL PAIR .......... */
    zz = p + d_sign(&zz, &p);
    wr[na] = x + zz;
    wr[en] = wr[na];
    if (zz != 0.) {
	wr[en] = x - w / zz;
    }
    wi[na] = 0.;
    wi[en] = 0.;
    x = h__[en + na * h_dim1];
    s = abs(x) + abs(zz);
    p = x / s;
    q = zz / s;
    r__ = sqrt(p * p + q * q);
    p /= r__;
    q /= r__;
/*     .......... ROW MODIFICATION .......... */
    i__1 = *n;
    for (j = na; j <= i__1; ++j) {
	zz = h__[na + j * h_dim1];
	h__[na + j * h_dim1] = q * zz + p * h__[en + j * h_dim1];
	h__[en + j * h_dim1] = q * h__[en + j * h_dim1] - p * zz;
/* L290: */
    }
/*     .......... COLUMN MODIFICATION .......... */
    i__1 = en;
    for (i__ = 1; i__ <= i__1; ++i__) {
	zz = h__[i__ + na * h_dim1];
	h__[i__ + na * h_dim1] = q * zz + p * h__[i__ + en * h_dim1];
	h__[i__ + en * h_dim1] = q * h__[i__ + en * h_dim1] - p * zz;
/* L300: */
    }
/*     .......... ACCUMULATE TRANSFORMATIONS .......... */
    i__1 = *igh;
    for (i__ = *low; i__ <= i__1; ++i__) {
	zz = z__[i__ + na * z_dim1];
	z__[i__ + na * z_dim1] = q * zz + p * z__[i__ + en * z_dim1];
	z__[i__ + en * z_dim1] = q * z__[i__ + en * z_dim1] - p * zz;
/* L310: */
    }

    goto L330;
/*     .......... COMPLEX PAIR .......... */
L320:
    wr[na] = x + p;
    wr[en] = x + p;
    wi[na] = zz;
    wi[en] = -zz;
L330:
    en = enm2;
    goto L60;
/*     .......... ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND */
/*                VECTORS OF UPPER TRIANGULAR FORM .......... */
L340:
    if (norm == 0.) {
	goto L1001;
    }
/*     .......... FOR EN=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (nn = 1; nn <= i__1; ++nn) {
	en = *n + 1 - nn;
	p = wr[en];
	q = wi[en];
	na = en - 1;
	if (q < 0.) {
	    goto L710;
	} else if (q == 0) {
	    goto L600;
	} else {
	    goto L800;
	}
/*     .......... REAL VECTOR .......... */
L600:
	m = en;
	h__[en + en * h_dim1] = 1.;
	if (na == 0) {
	    goto L800;
	}
/*     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = na;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = en - ii;
	    w = h__[i__ + i__ * h_dim1] - p;
	    r__ = 0.;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
/* L610: */
		r__ += h__[i__ + j * h_dim1] * h__[j + en * h_dim1];
	    }

	    if (wi[i__] >= 0.) {
		goto L630;
	    }
	    zz = w;
	    s = r__;
	    goto L700;
L630:
	    m = i__;
	    if (wi[i__] != 0.) {
		goto L640;
	    }
	    t = w;
	    if (t != 0.) {
		goto L635;
	    }
	    tst1 = norm;
	    t = tst1;
L632:
	    t *= .01;
	    tst2 = norm + t;
	    if (tst2 > tst1) {
		goto L632;
	    }
L635:
	    h__[i__ + en * h_dim1] = -r__ / t;
	    goto L680;
/*     .......... SOLVE REAL EQUATIONS .......... */
L640:
	    x = h__[i__ + (i__ + 1) * h_dim1];
	    y = h__[i__ + 1 + i__ * h_dim1];
	    q = (wr[i__] - p) * (wr[i__] - p) + wi[i__] * wi[i__];
	    t = (x * s - zz * r__) / q;
	    h__[i__ + en * h_dim1] = t;
	    if (abs(x) <= abs(zz)) {
		goto L650;
	    }
	    h__[i__ + 1 + en * h_dim1] = (-r__ - w * t) / x;
	    goto L680;
L650:
	    h__[i__ + 1 + en * h_dim1] = (-s - y * t) / zz;

/*     .......... OVERFLOW CONTROL .......... */
L680:
	    t = (d__1 = h__[i__ + en * h_dim1], abs(d__1));
	    if (t == 0.) {
		goto L700;
	    }
	    tst1 = t;
	    tst2 = tst1 + 1. / tst1;
	    if (tst2 > tst1) {
		goto L700;
	    }
	    i__3 = en;
	    for (j = i__; j <= i__3; ++j) {
		h__[j + en * h_dim1] /= t;
/* L690: */
	    }

L700:
	    ;
	}
/*     .......... END REAL VECTOR .......... */
	goto L800;
/*     .......... COMPLEX VECTOR .......... */
L710:
	m = na;
/*     .......... LAST VECTOR COMPONENT CHOSEN IMAGINARY SO THAT */
/*                EIGENVECTOR MATRIX IS TRIANGULAR .......... */
	if ((d__1 = h__[en + na * h_dim1], abs(d__1)) <= (d__2 = h__[na + en *
		 h_dim1], abs(d__2))) {
	    goto L720;
	}
	h__[na + na * h_dim1] = q / h__[en + na * h_dim1];
	h__[na + en * h_dim1] = -(h__[en + en * h_dim1] - p) / h__[en + na * 
		h_dim1];
	goto L730;
L720:
	d__1 = -h__[na + en * h_dim1];
	d__2 = h__[na + na * h_dim1] - p;
	cdiv_(&c_b49, &d__1, &d__2, &q, &h__[na + na * h_dim1], &h__[na + en *
		 h_dim1]);
L730:
	h__[en + na * h_dim1] = 0.;
	h__[en + en * h_dim1] = 1.;
	enm2 = na - 1;
	if (enm2 == 0) {
	    goto L800;
	}
/*     .......... FOR I=EN-2 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = enm2;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = na - ii;
	    w = h__[i__ + i__ * h_dim1] - p;
	    ra = 0.;
	    sa = 0.;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
		ra += h__[i__ + j * h_dim1] * h__[j + na * h_dim1];
		sa += h__[i__ + j * h_dim1] * h__[j + en * h_dim1];
/* L760: */
	    }

	    if (wi[i__] >= 0.) {
		goto L770;
	    }
	    zz = w;
	    r__ = ra;
	    s = sa;
	    goto L795;
L770:
	    m = i__;
	    if (wi[i__] != 0.) {
		goto L780;
	    }
	    d__1 = -ra;
	    d__2 = -sa;
	    cdiv_(&d__1, &d__2, &w, &q, &h__[i__ + na * h_dim1], &h__[i__ + 
		    en * h_dim1]);
	    goto L790;
/*     .......... SOLVE COMPLEX EQUATIONS .......... */
L780:
	    x = h__[i__ + (i__ + 1) * h_dim1];
	    y = h__[i__ + 1 + i__ * h_dim1];
	    vr = (wr[i__] - p) * (wr[i__] - p) + wi[i__] * wi[i__] - q * q;
	    vi = (wr[i__] - p) * 2. * q;
	    if (vr != 0. || vi != 0.) {
		goto L784;
	    }
	    tst1 = norm * (abs(w) + abs(q) + abs(x) + abs(y) + abs(zz));
	    vr = tst1;
L783:
	    vr *= .01;
	    tst2 = tst1 + vr;
	    if (tst2 > tst1) {
		goto L783;
	    }
L784:
	    d__1 = x * r__ - zz * ra + q * sa;
	    d__2 = x * s - zz * sa - q * ra;
	    cdiv_(&d__1, &d__2, &vr, &vi, &h__[i__ + na * h_dim1], &h__[i__ + 
		    en * h_dim1]);
	    if (abs(x) <= abs(zz) + abs(q)) {
		goto L785;
	    }
	    h__[i__ + 1 + na * h_dim1] = (-ra - w * h__[i__ + na * h_dim1] + 
		    q * h__[i__ + en * h_dim1]) / x;
	    h__[i__ + 1 + en * h_dim1] = (-sa - w * h__[i__ + en * h_dim1] - 
		    q * h__[i__ + na * h_dim1]) / x;
	    goto L790;
L785:
	    d__1 = -r__ - y * h__[i__ + na * h_dim1];
	    d__2 = -s - y * h__[i__ + en * h_dim1];
	    cdiv_(&d__1, &d__2, &zz, &q, &h__[i__ + 1 + na * h_dim1], &h__[
		    i__ + 1 + en * h_dim1]);

/*     .......... OVERFLOW CONTROL .......... */
L790:
/* Computing MAX */
	    d__3 = (d__1 = h__[i__ + na * h_dim1], abs(d__1)), d__4 = (d__2 = 
		    h__[i__ + en * h_dim1], abs(d__2));
	    t = max(d__3,d__4);
	    if (t == 0.) {
		goto L795;
	    }
	    tst1 = t;
	    tst2 = tst1 + 1. / tst1;
	    if (tst2 > tst1) {
		goto L795;
	    }
	    i__3 = en;
	    for (j = i__; j <= i__3; ++j) {
		h__[j + na * h_dim1] /= t;
		h__[j + en * h_dim1] /= t;
/* L792: */
	    }

L795:
	    ;
	}
/*     .......... END COMPLEX VECTOR .......... */
L800:
	;
    }
/*     .......... END BACK SUBSTITUTION. */
/*                VECTORS OF ISOLATED ROOTS .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L840;
	}

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
/* L820: */
	    z__[i__ + j * z_dim1] = h__[i__ + j * h_dim1];
	}

L840:
	;
    }
/*     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE */
/*                VECTORS OF ORIGINAL FULL MATRIX. */
/*                FOR J=N STEP -1 UNTIL LOW DO -- .......... */
    i__1 = *n;
    for (jj = *low; jj <= i__1; ++jj) {
	j = *n + *low - jj;
	m = min(j,*igh);

	i__2 = *igh;
	for (i__ = *low; i__ <= i__2; ++i__) {
	    zz = 0.;

	    i__3 = m;
	    for (k = *low; k <= i__3; ++k) {
/* L860: */
		zz += z__[i__ + k * z_dim1] * h__[k + j * h_dim1];
	    }

	    z__[i__ + j * z_dim1] = zz;
/* L880: */
	}
    }

    goto L1001;
/*     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT */
/*                CONVERGED AFTER 30*N ITERATIONS .......... */
L1000:
    *ierr = en;
L1001:
    return 0;
} /* hqr2_ */

