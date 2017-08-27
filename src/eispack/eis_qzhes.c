/* qzhes.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int qzhes_(integer *nm, integer *n, doublereal *a, 
	doublereal *b, logical *matz, doublereal *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, 
	    i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    integer i__, j, k, l;
    doublereal r__, s, t;
    integer l1;
    doublereal u1, u2, v1, v2;
    integer lb, nk1, nm1, nm2;
    doublereal rho;



/*     THIS SUBROUTINE IS THE FIRST STEP OF THE QZ ALGORITHM */
/*     FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS, */
/*     SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART. */

/*     THIS SUBROUTINE ACCEPTS A PAIR OF REAL GENERAL MATRICES AND */
/*     REDUCES ONE OF THEM TO UPPER HESSENBERG FORM AND THE OTHER */
/*     TO UPPER TRIANGULAR FORM USING ORTHOGONAL TRANSFORMATIONS. */
/*     IT IS USUALLY FOLLOWED BY  QZIT,  QZVAL  AND, POSSIBLY,  QZVEC. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRICES. */

/*        A CONTAINS A REAL GENERAL MATRIX. */

/*        B CONTAINS A REAL GENERAL MATRIX. */

/*        MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS 
*/
/*          ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING */
/*          EIGENVECTORS, AND TO .FALSE. OTHERWISE. */

/*     ON OUTPUT */

/*        A HAS BEEN REDUCED TO UPPER HESSENBERG FORM.  THE ELEMENTS */
/*          BELOW THE FIRST SUBDIAGONAL HAVE BEEN SET TO ZERO. */

/*        B HAS BEEN REDUCED TO UPPER TRIANGULAR FORM.  THE ELEMENTS */
/*          BELOW THE MAIN DIAGONAL HAVE BEEN SET TO ZERO. */

/*        Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS IF */
/*          MATZ HAS BEEN SET TO .TRUE.  OTHERWISE, Z IS NOT REFERENCED. 
*/

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

/*     .......... INITIALIZE Z .......... */
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
    if (! (*matz)) {
	goto L10;
    }

    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    z__[i__ + j * z_dim1] = 0.;
/* L2: */
	}

	z__[j + j * z_dim1] = 1.;
/* L3: */
    }
/*     .......... REDUCE B TO UPPER TRIANGULAR FORM .......... */
L10:
    if (*n <= 1) {
	goto L170;
    }
    nm1 = *n - 1;

    i__1 = nm1;
    for (l = 1; l <= i__1; ++l) {
	l1 = l + 1;
	s = 0.;

	i__2 = *n;
	for (i__ = l1; i__ <= i__2; ++i__) {
	    s += (d__1 = b[i__ + l * b_dim1], abs(d__1));
/* L20: */
	}

	if (s == 0.) {
	    goto L100;
	}
	s += (d__1 = b[l + l * b_dim1], abs(d__1));
	r__ = 0.;

	i__2 = *n;
	for (i__ = l; i__ <= i__2; ++i__) {
	    b[i__ + l * b_dim1] /= s;
/* Computing 2nd power */
	    d__1 = b[i__ + l * b_dim1];
	    r__ += d__1 * d__1;
/* L25: */
	}

	d__1 = sqrt(r__);
	r__ = d_sign(&d__1, &b[l + l * b_dim1]);
	b[l + l * b_dim1] += r__;
	rho = r__ * b[l + l * b_dim1];

	i__2 = *n;
	for (j = l1; j <= i__2; ++j) {
	    t = 0.;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		t += b[i__ + l * b_dim1] * b[i__ + j * b_dim1];
/* L30: */
	    }

	    t = -t / rho;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		b[i__ + j * b_dim1] += t * b[i__ + l * b_dim1];
/* L40: */
	    }

/* L50: */
	}

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    t = 0.;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		t += b[i__ + l * b_dim1] * a[i__ + j * a_dim1];
/* L60: */
	    }

	    t = -t / rho;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		a[i__ + j * a_dim1] += t * b[i__ + l * b_dim1];
/* L70: */
	    }

/* L80: */
	}

	b[l + l * b_dim1] = -s * r__;

	i__2 = *n;
	for (i__ = l1; i__ <= i__2; ++i__) {
	    b[i__ + l * b_dim1] = 0.;
/* L90: */
	}

L100:
	;
    }
/*     .......... REDUCE A TO UPPER HESSENBERG FORM, WHILE */
/*                KEEPING B TRIANGULAR .......... */
    if (*n == 2) {
	goto L170;
    }
    nm2 = *n - 2;

    i__1 = nm2;
    for (k = 1; k <= i__1; ++k) {
	nk1 = nm1 - k;
/*     .......... FOR L=N-1 STEP -1 UNTIL K+1 DO -- .......... */
	i__2 = nk1;
	for (lb = 1; lb <= i__2; ++lb) {
	    l = *n - lb;
	    l1 = l + 1;
/*     .......... ZERO A(L+1,K) .......... */
	    s = (d__1 = a[l + k * a_dim1], abs(d__1)) + (d__2 = a[l1 + k * 
		    a_dim1], abs(d__2));
	    if (s == 0.) {
		goto L150;
	    }
	    u1 = a[l + k * a_dim1] / s;
	    u2 = a[l1 + k * a_dim1] / s;
	    d__1 = sqrt(u1 * u1 + u2 * u2);
	    r__ = d_sign(&d__1, &u1);
	    v1 = -(u1 + r__) / r__;
	    v2 = -u2 / r__;
	    u2 = v2 / v1;

	    i__3 = *n;
	    for (j = k; j <= i__3; ++j) {
		t = a[l + j * a_dim1] + u2 * a[l1 + j * a_dim1];
		a[l + j * a_dim1] += t * v1;
		a[l1 + j * a_dim1] += t * v2;
/* L110: */
	    }

	    a[l1 + k * a_dim1] = 0.;

	    i__3 = *n;
	    for (j = l; j <= i__3; ++j) {
		t = b[l + j * b_dim1] + u2 * b[l1 + j * b_dim1];
		b[l + j * b_dim1] += t * v1;
		b[l1 + j * b_dim1] += t * v2;
/* L120: */
	    }
/*     .......... ZERO B(L+1,L) .......... */
	    s = (d__1 = b[l1 + l1 * b_dim1], abs(d__1)) + (d__2 = b[l1 + l * 
		    b_dim1], abs(d__2));
	    if (s == 0.) {
		goto L150;
	    }
	    u1 = b[l1 + l1 * b_dim1] / s;
	    u2 = b[l1 + l * b_dim1] / s;
	    d__1 = sqrt(u1 * u1 + u2 * u2);
	    r__ = d_sign(&d__1, &u1);
	    v1 = -(u1 + r__) / r__;
	    v2 = -u2 / r__;
	    u2 = v2 / v1;

	    i__3 = l1;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		t = b[i__ + l1 * b_dim1] + u2 * b[i__ + l * b_dim1];
		b[i__ + l1 * b_dim1] += t * v1;
		b[i__ + l * b_dim1] += t * v2;
/* L130: */
	    }

	    b[l1 + l * b_dim1] = 0.;

	    i__3 = *n;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		t = a[i__ + l1 * a_dim1] + u2 * a[i__ + l * a_dim1];
		a[i__ + l1 * a_dim1] += t * v1;
		a[i__ + l * a_dim1] += t * v2;
/* L140: */
	    }

	    if (! (*matz)) {
		goto L150;
	    }

	    i__3 = *n;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		t = z__[i__ + l1 * z_dim1] + u2 * z__[i__ + l * z_dim1];
		z__[i__ + l1 * z_dim1] += t * v1;
		z__[i__ + l * z_dim1] += t * v2;
/* L145: */
	    }

L150:
	    ;
	}

/* L160: */
    }

L170:
    return 0;
} /* qzhes_ */

