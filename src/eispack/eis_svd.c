/* svd.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b47 = 1.;

/* Subroutine */ int svd_(integer *nm, integer *m, integer *n, doublereal *a, 
	doublereal *w, logical *matu, doublereal *u, logical *matv, 
	doublereal *v, integer *ierr, doublereal *rv1)
{
    /* System generated locals */
    integer a_dim1, a_offset, u_dim1, u_offset, v_dim1, v_offset, i__1, i__2, 
	    i__3;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal c__, f, g, h__;
    static integer i__, j, k, l;
    static doublereal s, x, y, z__, scale;
    static integer i1, k1, l1, ii, kk, ll, mn;
    extern doublereal pythag_(doublereal *, doublereal *);
    static integer its;
    static doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE SVD, */
/*     NUM. MATH. 14, 403-420(1970) BY GOLUB AND REINSCH. */
/*     HANDBOOK FOR AUTO. COMP., VOL II-LINEAR ALGEBRA, 134-151(1971). */

/*     THIS SUBROUTINE DETERMINES THE SINGULAR VALUE DECOMPOSITION */
/*          T */
/*     A=USV  OF A REAL M BY N RECTANGULAR MATRIX.  HOUSEHOLDER */
/*     BIDIAGONALIZATION AND A VARIANT OF THE QR ALGORITHM ARE USED. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT.  NOTE THAT NM MUST BE AT LEAST */
/*          AS LARGE AS THE MAXIMUM OF M AND N. */

/*        M IS THE NUMBER OF ROWS OF A (AND U). */

/*        N IS THE NUMBER OF COLUMNS OF A (AND U) AND THE ORDER OF V. */

/*        A CONTAINS THE RECTANGULAR INPUT MATRIX TO BE DECOMPOSED. */

/*        MATU SHOULD BE SET TO .TRUE. IF THE U MATRIX IN THE */
/*          DECOMPOSITION IS DESIRED, AND TO .FALSE. OTHERWISE. */

/*        MATV SHOULD BE SET TO .TRUE. IF THE V MATRIX IN THE */
/*          DECOMPOSITION IS DESIRED, AND TO .FALSE. OTHERWISE. */

/*     ON OUTPUT */

/*        A IS UNALTERED (UNLESS OVERWRITTEN BY U OR V). */

/*        W CONTAINS THE N (NON-NEGATIVE) SINGULAR VALUES OF A (THE */
/*          DIAGONAL ELEMENTS OF S).  THEY ARE UNORDERED.  IF AN */
/*          ERROR EXIT IS MADE, THE SINGULAR VALUES SHOULD BE CORRECT */
/*          FOR INDICES IERR+1,IERR+2,...,N. */

/*        U CONTAINS THE MATRIX U (ORTHOGONAL COLUMN VECTORS) OF THE */
/*          DECOMPOSITION IF MATU HAS BEEN SET TO .TRUE.  OTHERWISE */
/*          U IS USED AS A TEMPORARY ARRAY.  U MAY COINCIDE WITH A. */
/*          IF AN ERROR EXIT IS MADE, THE COLUMNS OF U CORRESPONDING */
/*          TO INDICES OF CORRECT SINGULAR VALUES SHOULD BE CORRECT. */

/*        V CONTAINS THE MATRIX V (ORTHOGONAL) OF THE DECOMPOSITION IF */
/*          MATV HAS BEEN SET TO .TRUE.  OTHERWISE V IS NOT REFERENCED. */
/*          V MAY ALSO COINCIDE WITH A IF U IS NOT NEEDED.  IF AN ERROR */
/*          EXIT IS MADE, THE COLUMNS OF V CORRESPONDING TO INDICES OF */
/*          CORRECT SINGULAR VALUES SHOULD BE CORRECT. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          K          IF THE K-TH SINGULAR VALUE HAS NOT BEEN */
/*                     DETERMINED AFTER 30 ITERATIONS. */

/*        RV1 IS A TEMPORARY STORAGE ARRAY. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --rv1;
    v_dim1 = *nm;
    v_offset = v_dim1 + 1;
    v -= v_offset;
    u_dim1 = *nm;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    --w;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    *ierr = 0;

    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    u[i__ + j * u_dim1] = a[i__ + j * a_dim1];
/* L100: */
	}
    }
/*     .......... HOUSEHOLDER REDUCTION TO BIDIAGONAL FORM .......... */
    g = 0.;
    scale = 0.;
    x = 0.;

    i__2 = *n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	l = i__ + 1;
	rv1[i__] = scale * g;
	g = 0.;
	s = 0.;
	scale = 0.;
	if (i__ > *m) {
	    goto L210;
	}

	i__1 = *m;
	for (k = i__; k <= i__1; ++k) {
/* L120: */
	    scale += (d__1 = u[k + i__ * u_dim1], abs(d__1));
	}

	if (scale == 0.) {
	    goto L210;
	}

	i__1 = *m;
	for (k = i__; k <= i__1; ++k) {
	    u[k + i__ * u_dim1] /= scale;
/* Computing 2nd power */
	    d__1 = u[k + i__ * u_dim1];
	    s += d__1 * d__1;
/* L130: */
	}

	f = u[i__ + i__ * u_dim1];
	d__1 = sqrt(s);
	g = -d_sign(&d__1, &f);
	h__ = f * g - s;
	u[i__ + i__ * u_dim1] = f - g;
	if (i__ == *n) {
	    goto L190;
	}

	i__1 = *n;
	for (j = l; j <= i__1; ++j) {
	    s = 0.;

	    i__3 = *m;
	    for (k = i__; k <= i__3; ++k) {
/* L140: */
		s += u[k + i__ * u_dim1] * u[k + j * u_dim1];
	    }

	    f = s / h__;

	    i__3 = *m;
	    for (k = i__; k <= i__3; ++k) {
		u[k + j * u_dim1] += f * u[k + i__ * u_dim1];
/* L150: */
	    }
	}

L190:
	i__3 = *m;
	for (k = i__; k <= i__3; ++k) {
/* L200: */
	    u[k + i__ * u_dim1] = scale * u[k + i__ * u_dim1];
	}

L210:
	w[i__] = scale * g;
	g = 0.;
	s = 0.;
	scale = 0.;
	if (i__ > *m || i__ == *n) {
	    goto L290;
	}

	i__3 = *n;
	for (k = l; k <= i__3; ++k) {
/* L220: */
	    scale += (d__1 = u[i__ + k * u_dim1], abs(d__1));
	}

	if (scale == 0.) {
	    goto L290;
	}

	i__3 = *n;
	for (k = l; k <= i__3; ++k) {
	    u[i__ + k * u_dim1] /= scale;
/* Computing 2nd power */
	    d__1 = u[i__ + k * u_dim1];
	    s += d__1 * d__1;
/* L230: */
	}

	f = u[i__ + l * u_dim1];
	d__1 = sqrt(s);
	g = -d_sign(&d__1, &f);
	h__ = f * g - s;
	u[i__ + l * u_dim1] = f - g;

	i__3 = *n;
	for (k = l; k <= i__3; ++k) {
/* L240: */
	    rv1[k] = u[i__ + k * u_dim1] / h__;
	}

	if (i__ == *m) {
	    goto L270;
	}

	i__3 = *m;
	for (j = l; j <= i__3; ++j) {
	    s = 0.;

	    i__1 = *n;
	    for (k = l; k <= i__1; ++k) {
/* L250: */
		s += u[j + k * u_dim1] * u[i__ + k * u_dim1];
	    }

	    i__1 = *n;
	    for (k = l; k <= i__1; ++k) {
		u[j + k * u_dim1] += s * rv1[k];
/* L260: */
	    }
	}

L270:
	i__1 = *n;
	for (k = l; k <= i__1; ++k) {
/* L280: */
	    u[i__ + k * u_dim1] = scale * u[i__ + k * u_dim1];
	}

L290:
/* Computing MAX */
	d__3 = x, d__4 = (d__1 = w[i__], abs(d__1)) + (d__2 = rv1[i__], abs(
		d__2));
	x = max(d__3,d__4);
/* L300: */
    }
/*     .......... ACCUMULATION OF RIGHT-HAND TRANSFORMATIONS .......... */
    if (! (*matv)) {
	goto L410;
    }
/*     .......... FOR I=N STEP -1 UNTIL 1 DO -- .......... */
    i__2 = *n;
    for (ii = 1; ii <= i__2; ++ii) {
	i__ = *n + 1 - ii;
	if (i__ == *n) {
	    goto L390;
	}
	if (g == 0.) {
	    goto L360;
	}

	i__1 = *n;
	for (j = l; j <= i__1; ++j) {
/*     .......... DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ......
.... */
/* L320: */
	    v[j + i__ * v_dim1] = u[i__ + j * u_dim1] / u[i__ + l * u_dim1] / 
		    g;
	}

	i__1 = *n;
	for (j = l; j <= i__1; ++j) {
	    s = 0.;

	    i__3 = *n;
	    for (k = l; k <= i__3; ++k) {
/* L340: */
		s += u[i__ + k * u_dim1] * v[k + j * v_dim1];
	    }

	    i__3 = *n;
	    for (k = l; k <= i__3; ++k) {
		v[k + j * v_dim1] += s * v[k + i__ * v_dim1];
/* L350: */
	    }
	}

L360:
	i__3 = *n;
	for (j = l; j <= i__3; ++j) {
	    v[i__ + j * v_dim1] = 0.;
	    v[j + i__ * v_dim1] = 0.;
/* L380: */
	}

L390:
	v[i__ + i__ * v_dim1] = 1.;
	g = rv1[i__];
	l = i__;
/* L400: */
    }
/*     .......... ACCUMULATION OF LEFT-HAND TRANSFORMATIONS .......... */
L410:
    if (! (*matu)) {
	goto L510;
    }
/*     ..........FOR I=MIN(M,N) STEP -1 UNTIL 1 DO -- .......... */
    mn = *n;
    if (*m < *n) {
	mn = *m;
    }

    i__2 = mn;
    for (ii = 1; ii <= i__2; ++ii) {
	i__ = mn + 1 - ii;
	l = i__ + 1;
	g = w[i__];
	if (i__ == *n) {
	    goto L430;
	}

	i__3 = *n;
	for (j = l; j <= i__3; ++j) {
/* L420: */
	    u[i__ + j * u_dim1] = 0.;
	}

L430:
	if (g == 0.) {
	    goto L475;
	}
	if (i__ == mn) {
	    goto L460;
	}

	i__3 = *n;
	for (j = l; j <= i__3; ++j) {
	    s = 0.;

	    i__1 = *m;
	    for (k = l; k <= i__1; ++k) {
/* L440: */
		s += u[k + i__ * u_dim1] * u[k + j * u_dim1];
	    }
/*     .......... DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ......
.... */
	    f = s / u[i__ + i__ * u_dim1] / g;

	    i__1 = *m;
	    for (k = i__; k <= i__1; ++k) {
		u[k + j * u_dim1] += f * u[k + i__ * u_dim1];
/* L450: */
	    }
	}

L460:
	i__1 = *m;
	for (j = i__; j <= i__1; ++j) {
/* L470: */
	    u[j + i__ * u_dim1] /= g;
	}

	goto L490;

L475:
	i__1 = *m;
	for (j = i__; j <= i__1; ++j) {
/* L480: */
	    u[j + i__ * u_dim1] = 0.;
	}

L490:
	u[i__ + i__ * u_dim1] += 1.;
/* L500: */
    }
/*     .......... DIAGONALIZATION OF THE BIDIAGONAL FORM .......... */
L510:
    tst1 = x;
/*     .......... FOR K=N STEP -1 UNTIL 1 DO -- .......... */
    i__2 = *n;
    for (kk = 1; kk <= i__2; ++kk) {
	k1 = *n - kk;
	k = k1 + 1;
	its = 0;
/*     .......... TEST FOR SPLITTING. */
/*                FOR L=K STEP -1 UNTIL 1 DO -- .......... */
L520:
	i__1 = k;
	for (ll = 1; ll <= i__1; ++ll) {
	    l1 = k - ll;
	    l = l1 + 1;
	    tst2 = tst1 + (d__1 = rv1[l], abs(d__1));
	    if (tst2 == tst1) {
		goto L565;
	    }
/*     .......... RV1(1) IS ALWAYS ZERO, SO THERE IS NO EXIT */
/*                THROUGH THE BOTTOM OF THE LOOP .......... */
	    tst2 = tst1 + (d__1 = w[l1], abs(d__1));
	    if (tst2 == tst1) {
		goto L540;
	    }
/* L530: */
	}
/*     .......... CANCELLATION OF RV1(L) IF L GREATER THAN 1 .........
. */
L540:
	c__ = 0.;
	s = 1.;

	i__1 = k;
	for (i__ = l; i__ <= i__1; ++i__) {
	    f = s * rv1[i__];
	    rv1[i__] = c__ * rv1[i__];
	    tst2 = tst1 + abs(f);
	    if (tst2 == tst1) {
		goto L565;
	    }
	    g = w[i__];
	    h__ = pythag_(&f, &g);
	    w[i__] = h__;
	    c__ = g / h__;
	    s = -f / h__;
	    if (! (*matu)) {
		goto L560;
	    }

	    i__3 = *m;
	    for (j = 1; j <= i__3; ++j) {
		y = u[j + l1 * u_dim1];
		z__ = u[j + i__ * u_dim1];
		u[j + l1 * u_dim1] = y * c__ + z__ * s;
		u[j + i__ * u_dim1] = -y * s + z__ * c__;
/* L550: */
	    }

L560:
	    ;
	}
/*     .......... TEST FOR CONVERGENCE .......... */
L565:
	z__ = w[k];
	if (l == k) {
	    goto L650;
	}
/*     .......... SHIFT FROM BOTTOM 2 BY 2 MINOR .......... */
	if (its == 30) {
	    goto L1000;
	}
	++its;
	x = w[l];
	y = w[k1];
	g = rv1[k1];
	h__ = rv1[k];
	f = ((g + z__) / h__ * ((g - z__) / y) + y / h__ - h__ / y) * .5;
	g = pythag_(&f, &c_b47);
	f = x - z__ / x * z__ + h__ / x * (y / (f + d_sign(&g, &f)) - h__);
/*     .......... NEXT QR TRANSFORMATION .......... */
	c__ = 1.;
	s = 1.;

	i__1 = k1;
	for (i1 = l; i1 <= i__1; ++i1) {
	    i__ = i1 + 1;
	    g = rv1[i__];
	    y = w[i__];
	    h__ = s * g;
	    g = c__ * g;
	    z__ = pythag_(&f, &h__);
	    rv1[i1] = z__;
	    c__ = f / z__;
	    s = h__ / z__;
	    f = x * c__ + g * s;
	    g = -x * s + g * c__;
	    h__ = y * s;
	    y *= c__;
	    if (! (*matv)) {
		goto L575;
	    }

	    i__3 = *n;
	    for (j = 1; j <= i__3; ++j) {
		x = v[j + i1 * v_dim1];
		z__ = v[j + i__ * v_dim1];
		v[j + i1 * v_dim1] = x * c__ + z__ * s;
		v[j + i__ * v_dim1] = -x * s + z__ * c__;
/* L570: */
	    }

L575:
	    z__ = pythag_(&f, &h__);
	    w[i1] = z__;
/*     .......... ROTATION CAN BE ARBITRARY IF Z IS ZERO .........
. */
	    if (z__ == 0.) {
		goto L580;
	    }
	    c__ = f / z__;
	    s = h__ / z__;
L580:
	    f = c__ * g + s * y;
	    x = -s * g + c__ * y;
	    if (! (*matu)) {
		goto L600;
	    }

	    i__3 = *m;
	    for (j = 1; j <= i__3; ++j) {
		y = u[j + i1 * u_dim1];
		z__ = u[j + i__ * u_dim1];
		u[j + i1 * u_dim1] = y * c__ + z__ * s;
		u[j + i__ * u_dim1] = -y * s + z__ * c__;
/* L590: */
	    }

L600:
	    ;
	}

	rv1[l] = 0.;
	rv1[k] = f;
	w[k] = x;
	goto L520;
/*     .......... CONVERGENCE .......... */
L650:
	if (z__ >= 0.) {
	    goto L700;
	}
/*     .......... W(K) IS MADE NON-NEGATIVE .......... */
	w[k] = -z__;
	if (! (*matv)) {
	    goto L700;
	}

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
/* L690: */
	    v[j + k * v_dim1] = -v[j + k * v_dim1];
	}

L700:
	;
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO A */
/*                SINGULAR VALUE AFTER 30 ITERATIONS .......... */
L1000:
    *ierr = k;
L1001:
    return 0;
} /* svd_ */

