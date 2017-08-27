/* bqr.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static doublereal c_b8 = 1.;

/* Subroutine */ int bqr_(integer *nm, integer *n, integer *mb, doublereal *a,
	 doublereal *t, doublereal *r__, integer *ierr, integer *nv, 
	doublereal *rv)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *), sqrt(doublereal);

    /* Local variables */
    doublereal f, g;
    integer i__, j, k, l, m;
    doublereal q, s, scale;
    integer imult, m1, m2, m3, m4, m21, m31, ii, ik, jk, kj, jm, kk, 
	    km, ll, mk, mn, ni, mz;
    extern doublereal pythag_(doublereal *, doublereal *);
    integer kj1, its;
    doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE BQR, */
/*     NUM. MATH. 16, 85-92(1970) BY MARTIN, REINSCH, AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL II-LINEAR ALGEBRA, 266-272(1971). */

/*     THIS SUBROUTINE FINDS THE EIGENVALUE OF SMALLEST (USUALLY) */
/*     MAGNITUDE OF A REAL SYMMETRIC BAND MATRIX USING THE */
/*     QR ALGORITHM WITH SHIFTS OF ORIGIN.  CONSECUTIVE CALLS */
/*     CAN BE MADE TO FIND FURTHER EIGENVALUES. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        MB IS THE (HALF) BAND WIDTH OF THE MATRIX, DEFINED AS THE */
/*          NUMBER OF ADJACENT DIAGONALS, INCLUDING THE PRINCIPAL */
/*          DIAGONAL, REQUIRED TO SPECIFY THE NON-ZERO PORTION OF THE */
/*          LOWER TRIANGLE OF THE MATRIX. */

/*        A CONTAINS THE LOWER TRIANGLE OF THE SYMMETRIC BAND INPUT */
/*          MATRIX STORED AS AN N BY MB ARRAY.  ITS LOWEST SUBDIAGONAL */
/*          IS STORED IN THE LAST N+1-MB POSITIONS OF THE FIRST COLUMN, */
/*          ITS NEXT SUBDIAGONAL IN THE LAST N+2-MB POSITIONS OF THE */
/*          SECOND COLUMN, FURTHER SUBDIAGONALS SIMILARLY, AND FINALLY */
/*          ITS PRINCIPAL DIAGONAL IN THE N POSITIONS OF THE LAST COLUMN. 
*/
/*          CONTENTS OF STORAGES NOT PART OF THE MATRIX ARE ARBITRARY. */
/*          ON A SUBSEQUENT CALL, ITS OUTPUT CONTENTS FROM THE PREVIOUS */
/*          CALL SHOULD BE PASSED. */

/*        T SPECIFIES THE SHIFT (OF EIGENVALUES) APPLIED TO THE DIAGONAL 
*/
/*          OF A IN FORMING THE INPUT MATRIX. WHAT IS ACTUALLY DETERMINED 
*/
/*          IS THE EIGENVALUE OF A+TI (I IS THE IDENTITY MATRIX) NEAREST 
*/
/*          TO T.  ON A SUBSEQUENT CALL, THE OUTPUT VALUE OF T FROM THE */
/*          PREVIOUS CALL SHOULD BE PASSED IF THE NEXT NEAREST EIGENVALUE 
*/
/*          IS SOUGHT. */

/*        R SHOULD BE SPECIFIED AS ZERO ON THE FIRST CALL, AND AS ITS */
/*          OUTPUT VALUE FROM THE PREVIOUS CALL ON A SUBSEQUENT CALL. */
/*          IT IS USED TO DETERMINE WHEN THE LAST ROW AND COLUMN OF */
/*          THE TRANSFORMED BAND MATRIX CAN BE REGARDED AS NEGLIGIBLE. */

/*        NV MUST BE SET TO THE DIMENSION OF THE ARRAY PARAMETER RV */
/*          AS DECLARED IN THE CALLING PROGRAM DIMENSION STATEMENT. */

/*     ON OUTPUT */

/*        A CONTAINS THE TRANSFORMED BAND MATRIX.  THE MATRIX A+TI */
/*          DERIVED FROM THE OUTPUT PARAMETERS IS SIMILAR TO THE */
/*          INPUT A+TI TO WITHIN ROUNDING ERRORS.  ITS LAST ROW AND */
/*          COLUMN ARE NULL (IF IERR IS ZERO). */

/*        T CONTAINS THE COMPUTED EIGENVALUE OF A+TI (IF IERR IS ZERO). */

/*        R CONTAINS THE MAXIMUM OF ITS INPUT VALUE AND THE NORM OF THE */
/*          LAST COLUMN OF THE INPUT MATRIX A. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          N          IF THE EIGENVALUE HAS NOT BEEN */
/*                     DETERMINED AFTER 30 ITERATIONS. */

/*        RV IS A TEMPORARY STORAGE ARRAY OF DIMENSION AT LEAST */
/*          (2*MB**2+4*MB-3).  THE FIRST (3*MB-2) LOCATIONS CORRESPOND */
/*          TO THE ALGOL ARRAY B, THE NEXT (2*MB-1) LOCATIONS CORRESPOND 
*/
/*          TO THE ALGOL ARRAY H, AND THE FINAL (2*MB**2-MB) LOCATIONS */
/*          CORRESPOND TO THE MB BY (2*MB-1) ALGOL ARRAY U. */

/*     NOTE. FOR A SUBSEQUENT CALL, N SHOULD BE REPLACED BY N-1, BUT */
/*     MB SHOULD NOT BE ALTERED EVEN WHEN IT EXCEEDS THE CURRENT N. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --rv;

    /* Function Body */
    *ierr = 0;
    m1 = min(*mb,*n);
    m = m1 - 1;
    m2 = m + m;
    m21 = m2 + 1;
    m3 = m21 + m;
    m31 = m3 + 1;
    m4 = m31 + m2;
    mn = m + *n;
    mz = *mb - m1;
    its = 0;
/*     .......... TEST FOR CONVERGENCE .......... */
L40:
    g = a[*n + *mb * a_dim1];
    if (m == 0) {
	goto L360;
    }
    f = 0.;

    i__1 = m;
    for (k = 1; k <= i__1; ++k) {
	mk = k + mz;
	f += (d__1 = a[*n + mk * a_dim1], abs(d__1));
/* L50: */
    }

    if (its == 0 && f > *r__) {
	*r__ = f;
    }
    tst1 = *r__;
    tst2 = tst1 + f;
    if (tst2 <= tst1) {
	goto L360;
    }
    if (its == 30) {
	goto L1000;
    }
    ++its;
/*     .......... FORM SHIFT FROM BOTTOM 2 BY 2 MINOR .......... */
    if (f > *r__ * .25 && its < 5) {
	goto L90;
    }
    f = a[*n + (*mb - 1) * a_dim1];
    if (f == 0.) {
	goto L70;
    }
    q = (a[*n - 1 + *mb * a_dim1] - g) / (f * 2.);
    s = pythag_(&q, &c_b8);
    g -= f / (q + d_sign(&s, &q));
L70:
    *t += g;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L80: */
	a[i__ + *mb * a_dim1] -= g;
    }

L90:
    i__1 = m4;
    for (k = m31; k <= i__1; ++k) {
/* L100: */
	rv[k] = 0.;
    }

    i__1 = mn;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = ii - m;
	ni = *n - ii;
	if (ni < 0) {
	    goto L230;
	}
/*     .......... FORM COLUMN OF SHIFTED MATRIX A-G*I .......... */
/* Computing MAX */
	i__2 = 1, i__3 = 2 - i__;
	l = max(i__2,i__3);

	i__2 = m3;
	for (k = 1; k <= i__2; ++k) {
/* L110: */
	    rv[k] = 0.;
	}

	i__2 = m1;
	for (k = l; k <= i__2; ++k) {
	    km = k + m;
	    mk = k + mz;
	    rv[km] = a[ii + mk * a_dim1];
/* L120: */
	}

	ll = min(m,ni);
	if (ll == 0) {
	    goto L135;
	}

	i__2 = ll;
	for (k = 1; k <= i__2; ++k) {
	    km = k + m21;
	    ik = ii + k;
	    mk = *mb - k;
	    rv[km] = a[ik + mk * a_dim1];
/* L130: */
	}
/*     .......... PRE-MULTIPLY WITH HOUSEHOLDER REFLECTIONS ..........
 */
L135:
	ll = m2;
	imult = 0;
/*     .......... MULTIPLICATION PROCEDURE .......... */
L140:
	kj = m4 - m1;

	i__2 = ll;
	for (j = 1; j <= i__2; ++j) {
	    kj += m1;
	    jm = j + m3;
	    if (rv[jm] == 0.) {
		goto L170;
	    }
	    f = 0.;

	    i__3 = m1;
	    for (k = 1; k <= i__3; ++k) {
		++kj;
		jk = j + k - 1;
		f += rv[kj] * rv[jk];
/* L150: */
	    }

	    f /= rv[jm];
	    kj -= m1;

	    i__3 = m1;
	    for (k = 1; k <= i__3; ++k) {
		++kj;
		jk = j + k - 1;
		rv[jk] -= rv[kj] * f;
/* L160: */
	    }

	    kj -= m1;
L170:
	    ;
	}

	if (imult != 0) {
	    goto L280;
	}
/*     .......... HOUSEHOLDER REFLECTION .......... */
	f = rv[m21];
	s = 0.;
	rv[m4] = 0.;
	scale = 0.;

	i__2 = m3;
	for (k = m21; k <= i__2; ++k) {
/* L180: */
	    scale += (d__1 = rv[k], abs(d__1));
	}

	if (scale == 0.) {
	    goto L210;
	}

	i__2 = m3;
	for (k = m21; k <= i__2; ++k) {
/* L190: */
/* Computing 2nd power */
	    d__1 = rv[k] / scale;
	    s += d__1 * d__1;
	}

	s = scale * scale * s;
	d__1 = sqrt(s);
	g = -d_sign(&d__1, &f);
	rv[m21] = g;
	rv[m4] = s - f * g;
	kj = m4 + m2 * m1 + 1;
	rv[kj] = f - g;

	i__2 = m1;
	for (k = 2; k <= i__2; ++k) {
	    ++kj;
	    km = k + m2;
	    rv[kj] = rv[km];
/* L200: */
	}
/*     .......... SAVE COLUMN OF TRIANGULAR FACTOR R .......... */
L210:
	i__2 = m1;
	for (k = l; k <= i__2; ++k) {
	    km = k + m;
	    mk = k + mz;
	    a[ii + mk * a_dim1] = rv[km];
/* L220: */
	}

L230:
/* Computing MAX */
	i__2 = 1, i__3 = m1 + 1 - i__;
	l = max(i__2,i__3);
	if (i__ <= 0) {
	    goto L300;
	}
/*     .......... PERFORM ADDITIONAL STEPS .......... */
	i__2 = m21;
	for (k = 1; k <= i__2; ++k) {
/* L240: */
	    rv[k] = 0.;
	}

/* Computing MIN */
	i__2 = m1, i__3 = ni + m1;
	ll = min(i__2,i__3);
/*     .......... GET ROW OF TRIANGULAR FACTOR R .......... */
	i__2 = ll;
	for (kk = 1; kk <= i__2; ++kk) {
	    k = kk - 1;
	    km = k + m1;
	    ik = i__ + k;
	    mk = *mb - k;
	    rv[km] = a[ik + mk * a_dim1];
/* L250: */
	}
/*     .......... POST-MULTIPLY WITH HOUSEHOLDER REFLECTIONS .........
. */
	ll = m1;
	imult = 1;
	goto L140;
/*     .......... STORE COLUMN OF NEW A MATRIX .......... */
L280:
	i__2 = m1;
	for (k = l; k <= i__2; ++k) {
	    mk = k + mz;
	    a[i__ + mk * a_dim1] = rv[k];
/* L290: */
	}
/*     .......... UPDATE HOUSEHOLDER REFLECTIONS .......... */
L300:
	if (l > 1) {
	    --l;
	}
	kj1 = m4 + l * m1;

	i__2 = m2;
	for (j = l; j <= i__2; ++j) {
	    jm = j + m3;
	    rv[jm] = rv[jm + 1];

	    i__3 = m1;
	    for (k = 1; k <= i__3; ++k) {
		++kj1;
		kj = kj1 - m1;
		rv[kj] = rv[kj1];
/* L320: */
	    }
	}

/* L350: */
    }

    goto L40;
/*     .......... CONVERGENCE .......... */
L360:
    *t += g;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L380: */
	a[i__ + *mb * a_dim1] -= g;
    }

    i__1 = m1;
    for (k = 1; k <= i__1; ++k) {
	mk = k + mz;
	a[*n + mk * a_dim1] = 0.;
/* L400: */
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO */
/*                EIGENVALUE AFTER 30 ITERATIONS .......... */
L1000:
    *ierr = *n;
L1001:
    return 0;
} /* bqr_ */

