/* qzvec.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int qzvec_(integer *nm, integer *n, doublereal *a, 
	doublereal *b, doublereal *alfr, doublereal *alfi, doublereal *beta, 
	doublereal *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, 
	    i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal alfm, almi, betm, epsb, almr, d__;
    integer i__, j, k, m;
    doublereal q, r__=0.0, s=0.0, t, w, x=0.0, y, t1, t2, w1, x1=0.0, z1=0.0, di;
    integer na, ii, en, jj;
    doublereal ra, dr, sa;
    integer nn;
    doublereal ti, rr, tr, zz=0.0;
    integer isw, enm2;



/*     THIS SUBROUTINE IS THE OPTIONAL FOURTH STEP OF THE QZ ALGORITHM */
/*     FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS, */
/*     SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART. */

/*     THIS SUBROUTINE ACCEPTS A PAIR OF REAL MATRICES, ONE OF THEM IN */
/*     QUASI-TRIANGULAR FORM (IN WHICH EACH 2-BY-2 BLOCK CORRESPONDS TO */
/*     A PAIR OF COMPLEX EIGENVALUES) AND THE OTHER IN UPPER TRIANGULAR */
/*     FORM.  IT COMPUTES THE EIGENVECTORS OF THE TRIANGULAR PROBLEM AND 
*/
/*     TRANSFORMS THE RESULTS BACK TO THE ORIGINAL COORDINATE SYSTEM. */
/*     IT IS USUALLY PRECEDED BY  QZHES,  QZIT, AND  QZVAL. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRICES. */

/*        A CONTAINS A REAL UPPER QUASI-TRIANGULAR MATRIX. */

/*        B CONTAINS A REAL UPPER TRIANGULAR MATRIX.  IN ADDITION, */
/*          LOCATION B(N,1) CONTAINS THE TOLERANCE QUANTITY (EPSB) */
/*          COMPUTED AND SAVED IN  QZIT. */

/*        ALFR, ALFI, AND BETA  ARE VECTORS WITH COMPONENTS WHOSE */
/*          RATIOS ((ALFR+I*ALFI)/BETA) ARE THE GENERALIZED */
/*          EIGENVALUES.  THEY ARE USUALLY OBTAINED FROM  QZVAL. */

/*        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE */
/*          REDUCTIONS BY  QZHES,  QZIT, AND  QZVAL, IF PERFORMED. */
/*          IF THE EIGENVECTORS OF THE TRIANGULAR PROBLEM ARE */
/*          DESIRED, Z MUST CONTAIN THE IDENTITY MATRIX. */

/*     ON OUTPUT */

/*        A IS UNALTERED.  ITS SUBDIAGONAL ELEMENTS PROVIDE INFORMATION */
/*           ABOUT THE STORAGE OF THE COMPLEX EIGENVECTORS. */

/*        B HAS BEEN DESTROYED. */

/*        ALFR, ALFI, AND BETA ARE UNALTERED. */

/*        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS. */
/*          IF ALFI(I) .EQ. 0.0, THE I-TH EIGENVALUE IS REAL AND */
/*            THE I-TH COLUMN OF Z CONTAINS ITS EIGENVECTOR. */
/*          IF ALFI(I) .NE. 0.0, THE I-TH EIGENVALUE IS COMPLEX. */
/*            IF ALFI(I) .GT. 0.0, THE EIGENVALUE IS THE FIRST OF */
/*              A COMPLEX PAIR AND THE I-TH AND (I+1)-TH COLUMNS */
/*              OF Z CONTAIN ITS EIGENVECTOR. */
/*            IF ALFI(I) .LT. 0.0, THE EIGENVALUE IS THE SECOND OF */
/*              A COMPLEX PAIR AND THE (I-1)-TH AND I-TH COLUMNS */
/*              OF Z CONTAIN THE CONJUGATE OF ITS EIGENVECTOR. */
/*          EACH EIGENVECTOR IS NORMALIZED SO THAT THE MODULUS */
/*          OF ITS LARGEST COMPONENT IS 1.0 . */

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
    --beta;
    --alfi;
    --alfr;
    b_dim1 = *nm;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    epsb = b[*n + b_dim1];
    isw = 1;
/*     .......... FOR EN=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (nn = 1; nn <= i__1; ++nn) {
	en = *n + 1 - nn;
	na = en - 1;
	if (isw == 2) {
	    goto L795;
	}
	if (alfi[en] != 0.) {
	    goto L710;
	}
/*     .......... REAL VECTOR .......... */
	m = en;
	b[en + en * b_dim1] = 1.;
	if (na == 0) {
	    goto L800;
	}
	alfm = alfr[m];
	betm = beta[m];
/*     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = na;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = en - ii;
	    w = betm * a[i__ + i__ * a_dim1] - alfm * b[i__ + i__ * b_dim1];
	    r__ = 0.;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
/* L610: */
		r__ += (betm * a[i__ + j * a_dim1] - alfm * b[i__ + j * 
			b_dim1]) * b[j + en * b_dim1];
	    }

	    if (i__ == 1 || isw == 2) {
		goto L630;
	    }
	    if (betm * a[i__ + (i__ - 1) * a_dim1] == 0.) {
		goto L630;
	    }
	    zz = w;
	    s = r__;
	    goto L690;
L630:
	    m = i__;
	    if (isw == 2) {
		goto L640;
	    }
/*     .......... REAL 1-BY-1 BLOCK .......... */
	    t = w;
	    if (w == 0.) {
		t = epsb;
	    }
	    b[i__ + en * b_dim1] = -r__ / t;
	    goto L700;
/*     .......... REAL 2-BY-2 BLOCK .......... */
L640:
	    x = betm * a[i__ + (i__ + 1) * a_dim1] - alfm * b[i__ + (i__ + 1) 
		    * b_dim1];
	    y = betm * a[i__ + 1 + i__ * a_dim1];
	    q = w * zz - x * y;
	    t = (x * s - zz * r__) / q;
	    b[i__ + en * b_dim1] = t;
	    if (abs(x) <= abs(zz)) {
		goto L650;
	    }
	    b[i__ + 1 + en * b_dim1] = (-r__ - w * t) / x;
	    goto L690;
L650:
	    b[i__ + 1 + en * b_dim1] = (-s - y * t) / zz;
L690:
	    isw = 3 - isw;
L700:
	    ;
	}
/*     .......... END REAL VECTOR .......... */
	goto L800;
/*     .......... COMPLEX VECTOR .......... */
L710:
	m = na;
	almr = alfr[m];
	almi = alfi[m];
	betm = beta[m];
/*     .......... LAST VECTOR COMPONENT CHOSEN IMAGINARY SO THAT */
/*                EIGENVECTOR MATRIX IS TRIANGULAR .......... */
	y = betm * a[en + na * a_dim1];
	b[na + na * b_dim1] = -almi * b[en + en * b_dim1] / y;
	b[na + en * b_dim1] = (almr * b[en + en * b_dim1] - betm * a[en + en *
		 a_dim1]) / y;
	b[en + na * b_dim1] = 0.;
	b[en + en * b_dim1] = 1.;
	enm2 = na - 1;
	if (enm2 == 0) {
	    goto L795;
	}
/*     .......... FOR I=EN-2 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = enm2;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = na - ii;
	    w = betm * a[i__ + i__ * a_dim1] - almr * b[i__ + i__ * b_dim1];
	    w1 = -almi * b[i__ + i__ * b_dim1];
	    ra = 0.;
	    sa = 0.;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
		x = betm * a[i__ + j * a_dim1] - almr * b[i__ + j * b_dim1];
		x1 = -almi * b[i__ + j * b_dim1];
		ra = ra + x * b[j + na * b_dim1] - x1 * b[j + en * b_dim1];
		sa = sa + x * b[j + en * b_dim1] + x1 * b[j + na * b_dim1];
/* L760: */
	    }

	    if (i__ == 1 || isw == 2) {
		goto L770;
	    }
	    if (betm * a[i__ + (i__ - 1) * a_dim1] == 0.) {
		goto L770;
	    }
	    zz = w;
	    z1 = w1;
	    r__ = ra;
	    s = sa;
	    isw = 2;
	    goto L790;
L770:
	    m = i__;
	    if (isw == 2) {
		goto L780;
	    }
/*     .......... COMPLEX 1-BY-1 BLOCK .......... */
	    tr = -ra;
	    ti = -sa;
L773:
	    dr = w;
	    di = w1;
/*     .......... COMPLEX DIVIDE (T1,T2) = (TR,TI) / (DR,DI) .....
..... */
L775:
	    if (abs(di) > abs(dr)) {
		goto L777;
	    }
	    rr = di / dr;
	    d__ = dr + di * rr;
	    t1 = (tr + ti * rr) / d__;
	    t2 = (ti - tr * rr) / d__;
	    switch (isw) {
		case 1:  goto L787;
		case 2:  goto L782;
	    }
L777:
	    rr = dr / di;
	    d__ = dr * rr + di;
	    t1 = (tr * rr + ti) / d__;
	    t2 = (ti * rr - tr) / d__;
	    switch (isw) {
		case 1:  goto L787;
		case 2:  goto L782;
	    }
/*     .......... COMPLEX 2-BY-2 BLOCK .......... */
L780:
	    x = betm * a[i__ + (i__ + 1) * a_dim1] - almr * b[i__ + (i__ + 1) 
		    * b_dim1];
	    x1 = -almi * b[i__ + (i__ + 1) * b_dim1];
	    y = betm * a[i__ + 1 + i__ * a_dim1];
	    tr = y * ra - w * r__ + w1 * s;
	    ti = y * sa - w * s - w1 * r__;
	    dr = w * zz - w1 * z1 - x * y;
	    di = w * z1 + w1 * zz - x1 * y;
	    if (dr == 0. && di == 0.) {
		dr = epsb;
	    }
	    goto L775;
L782:
	    b[i__ + 1 + na * b_dim1] = t1;
	    b[i__ + 1 + en * b_dim1] = t2;
	    isw = 1;
	    if (abs(y) > abs(w) + abs(w1)) {
		goto L785;
	    }
	    tr = -ra - x * b[i__ + 1 + na * b_dim1] + x1 * b[i__ + 1 + en * 
		    b_dim1];
	    ti = -sa - x * b[i__ + 1 + en * b_dim1] - x1 * b[i__ + 1 + na * 
		    b_dim1];
	    goto L773;
L785:
	    t1 = (-r__ - zz * b[i__ + 1 + na * b_dim1] + z1 * b[i__ + 1 + en *
		     b_dim1]) / y;
	    t2 = (-s - zz * b[i__ + 1 + en * b_dim1] - z1 * b[i__ + 1 + na * 
		    b_dim1]) / y;
L787:
	    b[i__ + na * b_dim1] = t1;
	    b[i__ + en * b_dim1] = t2;
L790:
	    ;
	}
/*     .......... END COMPLEX VECTOR .......... */
L795:
	isw = 3 - isw;
L800:
	;
    }
/*     .......... END BACK SUBSTITUTION. */
/*                TRANSFORM TO ORIGINAL COORDINATE SYSTEM. */
/*                FOR J=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (jj = 1; jj <= i__1; ++jj) {
	j = *n + 1 - jj;

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    zz = 0.;

	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
/* L860: */
		zz += z__[i__ + k * z_dim1] * b[k + j * b_dim1];
	    }

	    z__[i__ + j * z_dim1] = zz;
/* L880: */
	}
    }
/*     .......... NORMALIZE SO THAT MODULUS OF LARGEST */
/*                COMPONENT OF EACH VECTOR IS 1. */
/*                (ISW IS 1 INITIALLY FROM BEFORE) .......... */
    i__2 = *n;
    for (j = 1; j <= i__2; ++j) {
	d__ = 0.;
	if (isw == 2) {
	    goto L920;
	}
	if (alfi[j] != 0.) {
	    goto L945;
	}

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if ((d__1 = z__[i__ + j * z_dim1], abs(d__1)) > d__) {
		d__ = (d__2 = z__[i__ + j * z_dim1], abs(d__2));
	    }
/* L890: */
	}

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* L900: */
	    z__[i__ + j * z_dim1] /= d__;
	}

	goto L950;

L920:
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    r__ = (d__1 = z__[i__ + (j - 1) * z_dim1], abs(d__1)) + (d__2 = 
		    z__[i__ + j * z_dim1], abs(d__2));
	    if (r__ != 0.) {
/* Computing 2nd power */
		d__1 = z__[i__ + (j - 1) * z_dim1] / r__;
/* Computing 2nd power */
		d__2 = z__[i__ + j * z_dim1] / r__;
		r__ *= sqrt(d__1 * d__1 + d__2 * d__2);
	    }
	    if (r__ > d__) {
		d__ = r__;
	    }
/* L930: */
	}

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    z__[i__ + (j - 1) * z_dim1] /= d__;
	    z__[i__ + j * z_dim1] /= d__;
/* L940: */
	}

L945:
	isw = 3 - isw;
L950:
	;
    }

    return 0;
} /* qzvec_ */

