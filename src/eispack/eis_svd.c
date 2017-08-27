/* svd.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static doublereal c_b47 = 1.;

#ifndef FUNCNAME
#define FUNCNAME svd_
#endif

/* Subroutine */ int FUNCNAME(integer *m, integer *n, integer *lda, doublereal *a,
	 doublereal *w, logical *matu, integer *ldu, doublereal *u, logical *
	matv, integer *ldv, doublereal *v, integer *ierr, doublereal *rv1)
{
    /* System generated locals */
    integer a_dim1, a_offset, u_dim1, u_offset, v_dim1, v_offset, i__1, i__2, 
	    i__3;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal c__, f, g, h__;
    integer i__, j, k, l=0;
    doublereal s, x, y, z__, scale;
    integer i1, k1, l1=0, ii, kk, ll, mn;
    extern doublereal pythag_(doublereal *, doublereal *);
    integer its;
    doublereal tst1, tst2;



/*     this subroutine is a translation of the algol procedure svd, */
/*     num. math. 14, 403-420(1970) by golub and reinsch. */
/*     handbook for auto. comp., vol ii-linear algebra, 134-151(1971). */

/*     this subroutine determines the singular value decomposition */
/*          t */
/*     a=usv  of a real m by n rectangular matrix.  householder */
/*     bidiagonalization and a variant of the qr algorithm are used. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement.  note that nm must be at least */
/*          as large as the maximum of m and n. */

/*        m is the number of rows of a (and u). */

/*        n is the number of columns of a, u, and v */

/*        a contains the rectangular input matrix to be decomposed. */

/*        matu should be set to .true. if the u matrix in the */
/*          decomposition is desired, and to .false. otherwise. */

/*        matv should be set to .true. if the v matrix in the */
/*          decomposition is desired, and to .false. otherwise. */

/*        lda, ldu, ldv: are the leading dimensions of matrices */
/*          a, u, and v (respectively);  must have */
/*           lda >= m ; ldu >= m ; ldv >= n */

/*     on output */

/*        a is unaltered (unless overwritten by u or v). */

/*        w contains the n (non-negative) singular values of a (the */
/*          diagonal elements of s).  they are unordered.  if an */
/*          error exit is made, the singular values should be correct */
/*          for indices ierr+1,ierr+2,...,n. */

/*        u contains the matrix u (orthogonal column vectors) of the */
/*          decomposition if matu has been set to .true.  otherwise */
/*          u is used as a temporary array.  u may coincide with a. */
/*          if an error exit is made, the columns of u corresponding */
/*          to indices of correct singular values should be correct. */

/*        v contains the matrix v (orthogonal) of the decomposition if */
/*          matv has been set to .true.  otherwise v is not referenced. */
/*          v may also coincide with a if u is not needed.  if an error */
/*          exit is made, the columns of v corresponding to indices of */
/*          correct singular values should be correct. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          k          if the k-th singular value has not been */
/*                     determined after 30 iterations. */

/*        rv1 is a temporary storage array. */

/*     calls pythag for  dsqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory 
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --rv1;
    --w;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    u_dim1 = *ldu;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    v_dim1 = *ldv;
    v_offset = v_dim1 + 1;
    if( v != (doublereal *)0 ) v -= v_offset;

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
/*     .......... householder reduction to bidiagonal form .......... */
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
/*     .......... accumulation of right-hand transformations .......... */
    if (! (*matv)) {
	goto L410;
    }
/*     .......... for i=n step -1 until 1 do -- .......... */
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
/*     .......... double division avoids possible underflow ......
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
/*     .......... accumulation of left-hand transformations .......... */
L410:
    if (! (*matu)) {
	goto L510;
    }
/*     ..........for i=min(m,n) step -1 until 1 do -- .......... */
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
/*     .......... double division avoids possible underflow ......
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
/*     .......... diagonalization of the bidiagonal form .......... */
L510:
    tst1 = x;
/*     .......... for k=n step -1 until 1 do -- .......... */
    i__2 = *n;
    for (kk = 1; kk <= i__2; ++kk) {
	k1 = *n - kk;
	k = k1 + 1;
	its = 0;
/*     .......... test for splitting. */
/*                for l=k step -1 until 1 do -- .......... */
L520:
	i__1 = k;
	for (ll = 1; ll <= i__1; ++ll) {
	    l1 = k - ll;
	    l = l1 + 1;
	    tst2 = tst1 + (d__1 = rv1[l], abs(d__1));
	    if (tst2 == tst1) {
		goto L565;
	    }
/*     .......... rv1(1) is always zero, so there is no exit */
/*                through the bottom of the loop .......... */
	    tst2 = tst1 + (d__1 = w[l1], abs(d__1));
	    if (tst2 == tst1) {
		goto L540;
	    }
/* L530: */
	}
/*     .......... cancellation of rv1(l) if l greater than 1 .........
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
/*     .......... test for convergence .......... */
L565:
	z__ = w[k];
	if (l == k) {
	    goto L650;
	}
/*     .......... shift from bottom 2 by 2 minor .......... */
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
/*     .......... next qr transformation .......... */
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
/*     .......... rotation can be arbitrary if z is zero .........
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
/*     .......... convergence .......... */
L650:
	if (z__ >= 0.) {
	    goto L700;
	}
/*     .......... w(k) is made non-negative .......... */
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
/*     .......... set error -- no convergence to a */
/*                singular value after 30 iterations .......... */
L1000:
    *ierr = k;
L1001:
    return 0;
} /* svd_ */

