/* qzval.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int qzval_(integer *nm, integer *n, doublereal *a, 
	doublereal *b, doublereal *alfr, doublereal *alfi, doublereal *beta, 
	logical *matz, doublereal *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal epsb, c__, d__, e=0.0;
    integer i__, j;
    doublereal r__, s, t, a1, a2, u1, u2, v1, v2, a11, a12, a21, a22, 
	    b11, b12, b22, di, ei;
    integer na;
    doublereal an=0.0, bn;
    integer en;
    doublereal cq, dr;
    integer nn;
    doublereal cz, ti, tr, a1i, a2i, a11i, a12i, a22i, a11r, a12r, 
	    a22r, sqi, ssi;
    integer isw;
    doublereal sqr, szi, ssr, szr;



/*     THIS SUBROUTINE IS THE THIRD STEP OF THE QZ ALGORITHM */
/*     FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS, */
/*     SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART. */

/*     THIS SUBROUTINE ACCEPTS A PAIR OF REAL MATRICES, ONE OF THEM */
/*     IN QUASI-TRIANGULAR FORM AND THE OTHER IN UPPER TRIANGULAR FORM. */
/*     IT REDUCES THE QUASI-TRIANGULAR MATRIX FURTHER, SO THAT ANY */
/*     REMAINING 2-BY-2 BLOCKS CORRESPOND TO PAIRS OF COMPLEX */
/*     EIGENVALUES, AND RETURNS QUANTITIES WHOSE RATIOS GIVE THE */
/*     GENERALIZED EIGENVALUES.  IT IS USUALLY PRECEDED BY  QZHES */
/*     AND  QZIT  AND MAY BE FOLLOWED BY  QZVEC. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRICES. */

/*        A CONTAINS A REAL UPPER QUASI-TRIANGULAR MATRIX. */

/*        B CONTAINS A REAL UPPER TRIANGULAR MATRIX.  IN ADDITION, */
/*          LOCATION B(N,1) CONTAINS THE TOLERANCE QUANTITY (EPSB) */
/*          COMPUTED AND SAVED IN  QZIT. */

/*        MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS 
*/
/*          ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING */
/*          EIGENVECTORS, AND TO .FALSE. OTHERWISE. */

/*        Z CONTAINS, IF MATZ HAS BEEN SET TO .TRUE., THE */
/*          TRANSFORMATION MATRIX PRODUCED IN THE REDUCTIONS BY QZHES */
/*          AND QZIT, IF PERFORMED, OR ELSE THE IDENTITY MATRIX. */
/*          IF MATZ HAS BEEN SET TO .FALSE., Z IS NOT REFERENCED. */

/*     ON OUTPUT */

/*        A HAS BEEN REDUCED FURTHER TO A QUASI-TRIANGULAR MATRIX */
/*          IN WHICH ALL NONZERO SUBDIAGONAL ELEMENTS CORRESPOND TO */
/*          PAIRS OF COMPLEX EIGENVALUES. */

/*        B IS STILL IN UPPER TRIANGULAR FORM, ALTHOUGH ITS ELEMENTS */
/*          HAVE BEEN ALTERED.  B(N,1) IS UNALTERED. */

/*        ALFR AND ALFI CONTAIN THE REAL AND IMAGINARY PARTS OF THE */
/*          DIAGONAL ELEMENTS OF THE TRIANGULAR MATRIX THAT WOULD BE */
/*          OBTAINED IF A WERE REDUCED COMPLETELY TO TRIANGULAR FORM */
/*          BY UNITARY TRANSFORMATIONS.  NON-ZERO VALUES OF ALFI OCCUR */
/*          IN PAIRS, THE FIRST MEMBER POSITIVE AND THE SECOND NEGATIVE. 
*/

/*        BETA CONTAINS THE DIAGONAL ELEMENTS OF THE CORRESPONDING B, */
/*          NORMALIZED TO BE REAL AND NON-NEGATIVE.  THE GENERALIZED */
/*          EIGENVALUES ARE THEN THE RATIOS ((ALFR+I*ALFI)/BETA). */

/*        Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS */
/*          (FOR ALL THREE STEPS) IF MATZ HAS BEEN SET TO .TRUE. */

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
/*     .......... FIND EIGENVALUES OF QUASI-TRIANGULAR MATRICES. */
/*                FOR EN=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (nn = 1; nn <= i__1; ++nn) {
	en = *n + 1 - nn;
	na = en - 1;
	if (isw == 2) {
	    goto L505;
	}
	if (en == 1) {
	    goto L410;
	}
	if (a[en + na * a_dim1] != 0.) {
	    goto L420;
	}
/*     .......... 1-BY-1 BLOCK, ONE REAL ROOT .......... */
L410:
	alfr[en] = a[en + en * a_dim1];
	if (b[en + en * b_dim1] < 0.) {
	    alfr[en] = -alfr[en];
	}
	beta[en] = (d__1 = b[en + en * b_dim1], abs(d__1));
	alfi[en] = 0.;
	goto L510;
/*     .......... 2-BY-2 BLOCK .......... */
L420:
	if ((d__1 = b[na + na * b_dim1], abs(d__1)) <= epsb) {
	    goto L455;
	}
	if ((d__1 = b[en + en * b_dim1], abs(d__1)) > epsb) {
	    goto L430;
	}
	a1 = a[en + en * a_dim1];
	a2 = a[en + na * a_dim1];
	bn = 0.;
	goto L435;
L430:
	an = (d__1 = a[na + na * a_dim1], abs(d__1)) + (d__2 = a[na + en * 
		a_dim1], abs(d__2)) + (d__3 = a[en + na * a_dim1], abs(d__3)) 
		+ (d__4 = a[en + en * a_dim1], abs(d__4));
	bn = (d__1 = b[na + na * b_dim1], abs(d__1)) + (d__2 = b[na + en * 
		b_dim1], abs(d__2)) + (d__3 = b[en + en * b_dim1], abs(d__3));
	a11 = a[na + na * a_dim1] / an;
	a12 = a[na + en * a_dim1] / an;
	a21 = a[en + na * a_dim1] / an;
	a22 = a[en + en * a_dim1] / an;
	b11 = b[na + na * b_dim1] / bn;
	b12 = b[na + en * b_dim1] / bn;
	b22 = b[en + en * b_dim1] / bn;
	e = a11 / b11;
	ei = a22 / b22;
	s = a21 / (b11 * b22);
	t = (a22 - e * b22) / b22;
	if (abs(e) <= abs(ei)) {
	    goto L431;
	}
	e = ei;
	t = (a11 - e * b11) / b11;
L431:
	c__ = (t - s * b12) * .5;
	d__ = c__ * c__ + s * (a12 - e * b12);
	if (d__ < 0.) {
	    goto L480;
	}
/*     .......... TWO REAL ROOTS. */
/*                ZERO BOTH A(EN,NA) AND B(EN,NA) .......... */
	d__1 = sqrt(d__);
	e += c__ + d_sign(&d__1, &c__);
	a11 -= e * b11;
	a12 -= e * b12;
	a22 -= e * b22;
	if (abs(a11) + abs(a12) < abs(a21) + abs(a22)) {
	    goto L432;
	}
	a1 = a12;
	a2 = a11;
	goto L435;
L432:
	a1 = a22;
	a2 = a21;
/*     .......... CHOOSE AND APPLY REAL Z .......... */
L435:
	s = abs(a1) + abs(a2);
	u1 = a1 / s;
	u2 = a2 / s;
	d__1 = sqrt(u1 * u1 + u2 * u2);
	r__ = d_sign(&d__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	u2 = v2 / v1;

	i__2 = en;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    t = a[i__ + en * a_dim1] + u2 * a[i__ + na * a_dim1];
	    a[i__ + en * a_dim1] += t * v1;
	    a[i__ + na * a_dim1] += t * v2;
	    t = b[i__ + en * b_dim1] + u2 * b[i__ + na * b_dim1];
	    b[i__ + en * b_dim1] += t * v1;
	    b[i__ + na * b_dim1] += t * v2;
/* L440: */
	}

	if (! (*matz)) {
	    goto L450;
	}

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    t = z__[i__ + en * z_dim1] + u2 * z__[i__ + na * z_dim1];
	    z__[i__ + en * z_dim1] += t * v1;
	    z__[i__ + na * z_dim1] += t * v2;
/* L445: */
	}

L450:
	if (bn == 0.) {
	    goto L475;
	}
	if (an < abs(e) * bn) {
	    goto L455;
	}
	a1 = b[na + na * b_dim1];
	a2 = b[en + na * b_dim1];
	goto L460;
L455:
	a1 = a[na + na * a_dim1];
	a2 = a[en + na * a_dim1];
/*     .......... CHOOSE AND APPLY REAL Q .......... */
L460:
	s = abs(a1) + abs(a2);
	if (s == 0.) {
	    goto L475;
	}
	u1 = a1 / s;
	u2 = a2 / s;
	d__1 = sqrt(u1 * u1 + u2 * u2);
	r__ = d_sign(&d__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	u2 = v2 / v1;

	i__2 = *n;
	for (j = na; j <= i__2; ++j) {
	    t = a[na + j * a_dim1] + u2 * a[en + j * a_dim1];
	    a[na + j * a_dim1] += t * v1;
	    a[en + j * a_dim1] += t * v2;
	    t = b[na + j * b_dim1] + u2 * b[en + j * b_dim1];
	    b[na + j * b_dim1] += t * v1;
	    b[en + j * b_dim1] += t * v2;
/* L470: */
	}

L475:
	a[en + na * a_dim1] = 0.;
	b[en + na * b_dim1] = 0.;
	alfr[na] = a[na + na * a_dim1];
	alfr[en] = a[en + en * a_dim1];
	if (b[na + na * b_dim1] < 0.) {
	    alfr[na] = -alfr[na];
	}
	if (b[en + en * b_dim1] < 0.) {
	    alfr[en] = -alfr[en];
	}
	beta[na] = (d__1 = b[na + na * b_dim1], abs(d__1));
	beta[en] = (d__1 = b[en + en * b_dim1], abs(d__1));
	alfi[en] = 0.;
	alfi[na] = 0.;
	goto L505;
/*     .......... TWO COMPLEX ROOTS .......... */
L480:
	e += c__;
	ei = sqrt(-d__);
	a11r = a11 - e * b11;
	a11i = ei * b11;
	a12r = a12 - e * b12;
	a12i = ei * b12;
	a22r = a22 - e * b22;
	a22i = ei * b22;
	if (abs(a11r) + abs(a11i) + abs(a12r) + abs(a12i) < abs(a21) + abs(
		a22r) + abs(a22i)) {
	    goto L482;
	}
	a1 = a12r;
	a1i = a12i;
	a2 = -a11r;
	a2i = -a11i;
	goto L485;
L482:
	a1 = a22r;
	a1i = a22i;
	a2 = -a21;
	a2i = 0.;
/*     .......... CHOOSE COMPLEX Z .......... */
L485:
	cz = sqrt(a1 * a1 + a1i * a1i);
	if (cz == 0.) {
	    goto L487;
	}
	szr = (a1 * a2 + a1i * a2i) / cz;
	szi = (a1 * a2i - a1i * a2) / cz;
	r__ = sqrt(cz * cz + szr * szr + szi * szi);
	cz /= r__;
	szr /= r__;
	szi /= r__;
	goto L490;
L487:
	szr = 1.;
	szi = 0.;
L490:
	if (an < (abs(e) + ei) * bn) {
	    goto L492;
	}
	a1 = cz * b11 + szr * b12;
	a1i = szi * b12;
	a2 = szr * b22;
	a2i = szi * b22;
	goto L495;
L492:
	a1 = cz * a11 + szr * a12;
	a1i = szi * a12;
	a2 = cz * a21 + szr * a22;
	a2i = szi * a22;
/*     .......... CHOOSE COMPLEX Q .......... */
L495:
	cq = sqrt(a1 * a1 + a1i * a1i);
	if (cq == 0.) {
	    goto L497;
	}
	sqr = (a1 * a2 + a1i * a2i) / cq;
	sqi = (a1 * a2i - a1i * a2) / cq;
	r__ = sqrt(cq * cq + sqr * sqr + sqi * sqi);
	cq /= r__;
	sqr /= r__;
	sqi /= r__;
	goto L500;
L497:
	sqr = 1.;
	sqi = 0.;
/*     .......... COMPUTE DIAGONAL ELEMENTS THAT WOULD RESULT */
/*                IF TRANSFORMATIONS WERE APPLIED .......... */
L500:
	ssr = sqr * szr + sqi * szi;
	ssi = sqr * szi - sqi * szr;
	i__ = 1;
	tr = cq * cz * a11 + cq * szr * a12 + sqr * cz * a21 + ssr * a22;
	ti = cq * szi * a12 - sqi * cz * a21 + ssi * a22;
	dr = cq * cz * b11 + cq * szr * b12 + ssr * b22;
	di = cq * szi * b12 + ssi * b22;
	goto L503;
L502:
	i__ = 2;
	tr = ssr * a11 - sqr * cz * a12 - cq * szr * a21 + cq * cz * a22;
	ti = -ssi * a11 - sqi * cz * a12 + cq * szi * a21;
	dr = ssr * b11 - sqr * cz * b12 + cq * cz * b22;
	di = -ssi * b11 - sqi * cz * b12;
L503:
	t = ti * dr - tr * di;
	j = na;
	if (t < 0.) {
	    j = en;
	}
	r__ = sqrt(dr * dr + di * di);
	beta[j] = bn * r__;
	alfr[j] = an * (tr * dr + ti * di) / r__;
	alfi[j] = an * t / r__;
	if (i__ == 1) {
	    goto L502;
	}
L505:
	isw = 3 - isw;
L510:
	;
    }
    b[*n + b_dim1] = epsb;

    return 0;
} /* qzval_ */

