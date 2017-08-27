/* minfit.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static doublereal c_b39 = 1.;

/* Subroutine */ int minfit_(integer *nm, integer *m, integer *n, doublereal *
	a, doublereal *w, integer *ip, doublereal *b, integer *ierr, 
	doublereal *rv1)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal c__, f, g, h__;
    integer i__, j, k, l=0;
    doublereal s, x, y, z__, scale;
    integer i1, k1, l1=0, m1, ii, kk, ll;
    extern doublereal pythag_(doublereal *, doublereal *);
    integer its;
    doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE MINFIT, */
/*     NUM. MATH. 14, 403-420(1970) BY GOLUB AND REINSCH. */
/*     HANDBOOK FOR AUTO. COMP., VOL II-LINEAR ALGEBRA, 134-151(1971). */

/*     THIS SUBROUTINE DETERMINES, TOWARDS THE SOLUTION OF THE LINEAR */
/*                                                        T */
/*     SYSTEM AX=B, THE SINGULAR VALUE DECOMPOSITION A=USV  OF A REAL */
/*                                         T */
/*     M BY N RECTANGULAR MATRIX, FORMING U B RATHER THAN U.  HOUSEHOLDER 
*/
/*     BIDIAGONALIZATION AND A VARIANT OF THE QR ALGORITHM ARE USED. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT.  NOTE THAT NM MUST BE AT LEAST */
/*          AS LARGE AS THE MAXIMUM OF M AND N. */

/*        M IS THE NUMBER OF ROWS OF A AND B. */

/*        N IS THE NUMBER OF COLUMNS OF A AND THE ORDER OF V. */

/*        A CONTAINS THE RECTANGULAR COEFFICIENT MATRIX OF THE SYSTEM. */

/*        IP IS THE NUMBER OF COLUMNS OF B.  IP CAN BE ZERO. */

/*        B CONTAINS THE CONSTANT COLUMN MATRIX OF THE SYSTEM */
/*          IF IP IS NOT ZERO.  OTHERWISE B IS NOT REFERENCED. */

/*     ON OUTPUT */

/*        A HAS BEEN OVERWRITTEN BY THE MATRIX V (ORTHOGONAL) OF THE */
/*          DECOMPOSITION IN ITS FIRST N ROWS AND COLUMNS.  IF AN */
/*          ERROR EXIT IS MADE, THE COLUMNS OF V CORRESPONDING TO */
/*          INDICES OF CORRECT SINGULAR VALUES SHOULD BE CORRECT. */

/*        W CONTAINS THE N (NON-NEGATIVE) SINGULAR VALUES OF A (THE */
/*          DIAGONAL ELEMENTS OF S).  THEY ARE UNORDERED.  IF AN */
/*          ERROR EXIT IS MADE, THE SINGULAR VALUES SHOULD BE CORRECT */
/*          FOR INDICES IERR+1,IERR+2,...,N. */

/*                                   T */
/*        B HAS BEEN OVERWRITTEN BY U B.  IF AN ERROR EXIT IS MADE, */
/*                       T */
/*          THE ROWS OF U B CORRESPONDING TO INDICES OF CORRECT */
/*          SINGULAR VALUES SHOULD BE CORRECT. */

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
    --w;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *nm;
    b_offset = b_dim1 + 1;
    b -= b_offset;

    /* Function Body */
    *ierr = 0;
/*     .......... HOUSEHOLDER REDUCTION TO BIDIAGONAL FORM .......... */
    g = 0.;
    scale = 0.;
    x = 0.;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	l = i__ + 1;
	rv1[i__] = scale * g;
	g = 0.;
	s = 0.;
	scale = 0.;
	if (i__ > *m) {
	    goto L210;
	}

	i__2 = *m;
	for (k = i__; k <= i__2; ++k) {
/* L120: */
	    scale += (d__1 = a[k + i__ * a_dim1], abs(d__1));
	}

	if (scale == 0.) {
	    goto L210;
	}

	i__2 = *m;
	for (k = i__; k <= i__2; ++k) {
	    a[k + i__ * a_dim1] /= scale;
/* Computing 2nd power */
	    d__1 = a[k + i__ * a_dim1];
	    s += d__1 * d__1;
/* L130: */
	}

	f = a[i__ + i__ * a_dim1];
	d__1 = sqrt(s);
	g = -d_sign(&d__1, &f);
	h__ = f * g - s;
	a[i__ + i__ * a_dim1] = f - g;
	if (i__ == *n) {
	    goto L160;
	}

	i__2 = *n;
	for (j = l; j <= i__2; ++j) {
	    s = 0.;

	    i__3 = *m;
	    for (k = i__; k <= i__3; ++k) {
/* L140: */
		s += a[k + i__ * a_dim1] * a[k + j * a_dim1];
	    }

	    f = s / h__;

	    i__3 = *m;
	    for (k = i__; k <= i__3; ++k) {
		a[k + j * a_dim1] += f * a[k + i__ * a_dim1];
/* L150: */
	    }
	}

L160:
	if (*ip == 0) {
	    goto L190;
	}

	i__3 = *ip;
	for (j = 1; j <= i__3; ++j) {
	    s = 0.;

	    i__2 = *m;
	    for (k = i__; k <= i__2; ++k) {
/* L170: */
		s += a[k + i__ * a_dim1] * b[k + j * b_dim1];
	    }

	    f = s / h__;

	    i__2 = *m;
	    for (k = i__; k <= i__2; ++k) {
		b[k + j * b_dim1] += f * a[k + i__ * a_dim1];
/* L180: */
	    }
	}

L190:
	i__2 = *m;
	for (k = i__; k <= i__2; ++k) {
/* L200: */
	    a[k + i__ * a_dim1] = scale * a[k + i__ * a_dim1];
	}

L210:
	w[i__] = scale * g;
	g = 0.;
	s = 0.;
	scale = 0.;
	if (i__ > *m || i__ == *n) {
	    goto L290;
	}

	i__2 = *n;
	for (k = l; k <= i__2; ++k) {
/* L220: */
	    scale += (d__1 = a[i__ + k * a_dim1], abs(d__1));
	}

	if (scale == 0.) {
	    goto L290;
	}

	i__2 = *n;
	for (k = l; k <= i__2; ++k) {
	    a[i__ + k * a_dim1] /= scale;
/* Computing 2nd power */
	    d__1 = a[i__ + k * a_dim1];
	    s += d__1 * d__1;
/* L230: */
	}

	f = a[i__ + l * a_dim1];
	d__1 = sqrt(s);
	g = -d_sign(&d__1, &f);
	h__ = f * g - s;
	a[i__ + l * a_dim1] = f - g;

	i__2 = *n;
	for (k = l; k <= i__2; ++k) {
/* L240: */
	    rv1[k] = a[i__ + k * a_dim1] / h__;
	}

	if (i__ == *m) {
	    goto L270;
	}

	i__2 = *m;
	for (j = l; j <= i__2; ++j) {
	    s = 0.;

	    i__3 = *n;
	    for (k = l; k <= i__3; ++k) {
/* L250: */
		s += a[j + k * a_dim1] * a[i__ + k * a_dim1];
	    }

	    i__3 = *n;
	    for (k = l; k <= i__3; ++k) {
		a[j + k * a_dim1] += s * rv1[k];
/* L260: */
	    }
	}

L270:
	i__3 = *n;
	for (k = l; k <= i__3; ++k) {
/* L280: */
	    a[i__ + k * a_dim1] = scale * a[i__ + k * a_dim1];
	}

L290:
/* Computing MAX */
	d__3 = x, d__4 = (d__1 = w[i__], abs(d__1)) + (d__2 = rv1[i__], abs(
		d__2));
	x = max(d__3,d__4);
/* L300: */
    }
/*     .......... ACCUMULATION OF RIGHT-HAND TRANSFORMATIONS. */
/*                FOR I=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = *n + 1 - ii;
	if (i__ == *n) {
	    goto L390;
	}
	if (g == 0.) {
	    goto L360;
	}

	i__3 = *n;
	for (j = l; j <= i__3; ++j) {
/*     .......... DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ......
.... */
/* L320: */
	    a[j + i__ * a_dim1] = a[i__ + j * a_dim1] / a[i__ + l * a_dim1] / 
		    g;
	}

	i__3 = *n;
	for (j = l; j <= i__3; ++j) {
	    s = 0.;

	    i__2 = *n;
	    for (k = l; k <= i__2; ++k) {
/* L340: */
		s += a[i__ + k * a_dim1] * a[k + j * a_dim1];
	    }

	    i__2 = *n;
	    for (k = l; k <= i__2; ++k) {
		a[k + j * a_dim1] += s * a[k + i__ * a_dim1];
/* L350: */
	    }
	}

L360:
	i__2 = *n;
	for (j = l; j <= i__2; ++j) {
	    a[i__ + j * a_dim1] = 0.;
	    a[j + i__ * a_dim1] = 0.;
/* L380: */
	}

L390:
	a[i__ + i__ * a_dim1] = 1.;
	g = rv1[i__];
	l = i__;
/* L400: */
    }

    if (*m >= *n || *ip == 0) {
	goto L510;
    }
    m1 = *m + 1;

    i__1 = *n;
    for (i__ = m1; i__ <= i__1; ++i__) {

	i__2 = *ip;
	for (j = 1; j <= i__2; ++j) {
	    b[i__ + j * b_dim1] = 0.;
/* L500: */
	}
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
	    if (*ip == 0) {
		goto L560;
	    }

	    i__3 = *ip;
	    for (j = 1; j <= i__3; ++j) {
		y = b[l1 + j * b_dim1];
		z__ = b[i__ + j * b_dim1];
		b[l1 + j * b_dim1] = y * c__ + z__ * s;
		b[i__ + j * b_dim1] = -y * s + z__ * c__;
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
	g = pythag_(&f, &c_b39);
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

	    i__3 = *n;
	    for (j = 1; j <= i__3; ++j) {
		x = a[j + i1 * a_dim1];
		z__ = a[j + i__ * a_dim1];
		a[j + i1 * a_dim1] = x * c__ + z__ * s;
		a[j + i__ * a_dim1] = -x * s + z__ * c__;
/* L570: */
	    }

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
	    if (*ip == 0) {
		goto L600;
	    }

	    i__3 = *ip;
	    for (j = 1; j <= i__3; ++j) {
		y = b[i1 + j * b_dim1];
		z__ = b[i__ + j * b_dim1];
		b[i1 + j * b_dim1] = y * c__ + z__ * s;
		b[i__ + j * b_dim1] = -y * s + z__ * c__;
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

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
/* L690: */
	    a[j + k * a_dim1] = -a[j + k * a_dim1];
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
} /* minfit_ */

