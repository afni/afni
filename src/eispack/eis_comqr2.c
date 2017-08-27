/* comqr2.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int comqr2_(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *ortr, doublereal *orti, doublereal *hr, doublereal *
	hi, doublereal *wr, doublereal *wi, doublereal *zr, doublereal *zi, 
	integer *ierr)
{
    /* System generated locals */
    integer hr_dim1, hr_offset, hi_dim1, hi_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3, d__4;

    /* Local variables */
    integer iend;
    extern /* Subroutine */ int cdiv_(doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *);
    doublereal norm;
    integer i__, j, k, l=0, m, ii, en, jj, ll, nn;
    doublereal si, ti, xi, yi, sr, tr, xr, yr;
    extern doublereal pythag_(doublereal *, doublereal *);
    extern /* Subroutine */ int csroot_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    integer ip1, lp1, itn, its;
    doublereal zzi, zzr;
    integer enm1;
    doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF A UNITARY ANALOGUE OF THE */
/*     ALGOL PROCEDURE  COMLR2, NUM. MATH. 16, 181-204(1970) BY PETERS */
/*     AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971). */
/*     THE UNITARY ANALOGUE SUBSTITUTES THE QR ALGORITHM OF FRANCIS */
/*     (COMP. JOUR. 4, 332-345(1962)) FOR THE LR ALGORITHM. */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS */
/*     OF A COMPLEX UPPER HESSENBERG MATRIX BY THE QR */
/*     METHOD.  THE EIGENVECTORS OF A COMPLEX GENERAL MATRIX */
/*     CAN ALSO BE FOUND IF  CORTH  HAS BEEN USED TO REDUCE */
/*     THIS GENERAL MATRIX TO HESSENBERG FORM. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED, */
/*          SET LOW=1, IGH=N. */

/*        ORTR AND ORTI CONTAIN INFORMATION ABOUT THE UNITARY TRANS- */
/*          FORMATIONS USED IN THE REDUCTION BY  CORTH, IF PERFORMED. */
/*          ONLY ELEMENTS LOW THROUGH IGH ARE USED.  IF THE EIGENVECTORS 
*/
/*          OF THE HESSENBERG MATRIX ARE DESIRED, SET ORTR(J) AND */
/*          ORTI(J) TO 0.0D0 FOR THESE ELEMENTS. */

/*        HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX. */
/*          THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN FURTHER */
/*          INFORMATION ABOUT THE TRANSFORMATIONS WHICH WERE USED IN THE 
*/
/*          REDUCTION BY  CORTH, IF PERFORMED.  IF THE EIGENVECTORS OF */
/*          THE HESSENBERG MATRIX ARE DESIRED, THESE ELEMENTS MAY BE */
/*          ARBITRARY. */

/*     ON OUTPUT */

/*        ORTR, ORTI, AND THE UPPER HESSENBERG PORTIONS OF HR AND HI */
/*          HAVE BEEN DESTROYED. */

/*        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE EIGENVALUES.  IF AN ERROR */
/*          EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT */
/*          FOR INDICES IERR+1,...,N. */

/*        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE EIGENVECTORS.  THE EIGENVECTORS */
/*          ARE UNNORMALIZED.  IF AN ERROR EXIT IS MADE, NONE OF */
/*          THE EIGENVECTORS HAS BEEN FOUND. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED */
/*                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT. */

/*     CALLS CDIV FOR COMPLEX DIVISION. */
/*     CALLS CSROOT FOR COMPLEX SQUARE ROOT. */
/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;
    --wi;
    --wr;
    hi_dim1 = *nm;
    hi_offset = hi_dim1 + 1;
    hi -= hi_offset;
    hr_dim1 = *nm;
    hr_offset = hr_dim1 + 1;
    hr -= hr_offset;
    --orti;
    --ortr;

    /* Function Body */
    *ierr = 0;
/*     .......... INITIALIZE EIGENVECTOR MATRIX .......... */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    zr[i__ + j * zr_dim1] = 0.;
	    zi[i__ + j * zi_dim1] = 0.;
/* L100: */
	}
	zr[j + j * zr_dim1] = 1.;
/* L101: */
    }
/*     .......... FORM THE MATRIX OF ACCUMULATED TRANSFORMATIONS */
/*                FROM THE INFORMATION LEFT BY CORTH .......... */
    iend = *igh - *low - 1;
    if (iend < 0) {
	goto L180;
    } else if (iend == 0) {
	goto L150;
    } else {
	goto L105;
    }
/*     .......... FOR I=IGH-1 STEP -1 UNTIL LOW+1 DO -- .......... */
L105:
    i__1 = iend;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = *igh - ii;
	if (ortr[i__] == 0. && orti[i__] == 0.) {
	    goto L140;
	}
	if (hr[i__ + (i__ - 1) * hr_dim1] == 0. && hi[i__ + (i__ - 1) * 
		hi_dim1] == 0.) {
	    goto L140;
	}
/*     .......... NORM BELOW IS NEGATIVE OF H FORMED IN CORTH ........
.. */
	norm = hr[i__ + (i__ - 1) * hr_dim1] * ortr[i__] + hi[i__ + (i__ - 1) 
		* hi_dim1] * orti[i__];
	ip1 = i__ + 1;

	i__2 = *igh;
	for (k = ip1; k <= i__2; ++k) {
	    ortr[k] = hr[k + (i__ - 1) * hr_dim1];
	    orti[k] = hi[k + (i__ - 1) * hi_dim1];
/* L110: */
	}

	i__2 = *igh;
	for (j = i__; j <= i__2; ++j) {
	    sr = 0.;
	    si = 0.;

	    i__3 = *igh;
	    for (k = i__; k <= i__3; ++k) {
		sr = sr + ortr[k] * zr[k + j * zr_dim1] + orti[k] * zi[k + j *
			 zi_dim1];
		si = si + ortr[k] * zi[k + j * zi_dim1] - orti[k] * zr[k + j *
			 zr_dim1];
/* L115: */
	    }

	    sr /= norm;
	    si /= norm;

	    i__3 = *igh;
	    for (k = i__; k <= i__3; ++k) {
		zr[k + j * zr_dim1] = zr[k + j * zr_dim1] + sr * ortr[k] - si 
			* orti[k];
		zi[k + j * zi_dim1] = zi[k + j * zi_dim1] + sr * orti[k] + si 
			* ortr[k];
/* L120: */
	    }

/* L130: */
	}

L140:
	;
    }
/*     .......... CREATE REAL SUBDIAGONAL ELEMENTS .......... */
L150:
    l = *low + 1;

    i__1 = *igh;
    for (i__ = l; i__ <= i__1; ++i__) {
/* Computing MIN */
	i__2 = i__ + 1;
	ll = min(i__2,*igh);
	if (hi[i__ + (i__ - 1) * hi_dim1] == 0.) {
	    goto L170;
	}
	norm = pythag_(&hr[i__ + (i__ - 1) * hr_dim1], &hi[i__ + (i__ - 1) * 
		hi_dim1]);
	yr = hr[i__ + (i__ - 1) * hr_dim1] / norm;
	yi = hi[i__ + (i__ - 1) * hi_dim1] / norm;
	hr[i__ + (i__ - 1) * hr_dim1] = norm;
	hi[i__ + (i__ - 1) * hi_dim1] = 0.;

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    si = yr * hi[i__ + j * hi_dim1] - yi * hr[i__ + j * hr_dim1];
	    hr[i__ + j * hr_dim1] = yr * hr[i__ + j * hr_dim1] + yi * hi[i__ 
		    + j * hi_dim1];
	    hi[i__ + j * hi_dim1] = si;
/* L155: */
	}

	i__2 = ll;
	for (j = 1; j <= i__2; ++j) {
	    si = yr * hi[j + i__ * hi_dim1] + yi * hr[j + i__ * hr_dim1];
	    hr[j + i__ * hr_dim1] = yr * hr[j + i__ * hr_dim1] - yi * hi[j + 
		    i__ * hi_dim1];
	    hi[j + i__ * hi_dim1] = si;
/* L160: */
	}

	i__2 = *igh;
	for (j = *low; j <= i__2; ++j) {
	    si = yr * zi[j + i__ * zi_dim1] + yi * zr[j + i__ * zr_dim1];
	    zr[j + i__ * zr_dim1] = yr * zr[j + i__ * zr_dim1] - yi * zi[j + 
		    i__ * zi_dim1];
	    zi[j + i__ * zi_dim1] = si;
/* L165: */
	}

L170:
	;
    }
/*     .......... STORE ROOTS ISOLATED BY CBAL .......... */
L180:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L200;
	}
	wr[i__] = hr[i__ + i__ * hr_dim1];
	wi[i__] = hi[i__ + i__ * hi_dim1];
L200:
	;
    }

    en = *igh;
    tr = 0.;
    ti = 0.;
    itn = *n * 30;
/*     .......... SEARCH FOR NEXT EIGENVALUE .......... */
L220:
    if (en < *low) {
	goto L680;
    }
    its = 0;
    enm1 = en - 1;
/*     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT */
/*                FOR L=EN STEP -1 UNTIL LOW DO -- .......... */
L240:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L300;
	}
	tst1 = (d__1 = hr[l - 1 + (l - 1) * hr_dim1], abs(d__1)) + (d__2 = hi[
		l - 1 + (l - 1) * hi_dim1], abs(d__2)) + (d__3 = hr[l + l * 
		hr_dim1], abs(d__3)) + (d__4 = hi[l + l * hi_dim1], abs(d__4))
		;
	tst2 = tst1 + (d__1 = hr[l + (l - 1) * hr_dim1], abs(d__1));
	if (tst2 == tst1) {
	    goto L300;
	}
/* L260: */
    }
/*     .......... FORM SHIFT .......... */
L300:
    if (l == en) {
	goto L660;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its == 10 || its == 20) {
	goto L320;
    }
    sr = hr[en + en * hr_dim1];
    si = hi[en + en * hi_dim1];
    xr = hr[enm1 + en * hr_dim1] * hr[en + enm1 * hr_dim1];
    xi = hi[enm1 + en * hi_dim1] * hr[en + enm1 * hr_dim1];
    if (xr == 0. && xi == 0.) {
	goto L340;
    }
    yr = (hr[enm1 + enm1 * hr_dim1] - sr) / 2.;
    yi = (hi[enm1 + enm1 * hi_dim1] - si) / 2.;
/* Computing 2nd power */
    d__2 = yr;
/* Computing 2nd power */
    d__3 = yi;
    d__1 = d__2 * d__2 - d__3 * d__3 + xr;
    d__4 = yr * 2. * yi + xi;
    csroot_(&d__1, &d__4, &zzr, &zzi);
    if (yr * zzr + yi * zzi >= 0.) {
	goto L310;
    }
    zzr = -zzr;
    zzi = -zzi;
L310:
    d__1 = yr + zzr;
    d__2 = yi + zzi;
    cdiv_(&xr, &xi, &d__1, &d__2, &xr, &xi);
    sr -= xr;
    si -= xi;
    goto L340;
/*     .......... FORM EXCEPTIONAL SHIFT .......... */
L320:
    sr = (d__1 = hr[en + enm1 * hr_dim1], abs(d__1)) + (d__2 = hr[enm1 + (en 
	    - 2) * hr_dim1], abs(d__2));
    si = 0.;

L340:
    i__1 = en;
    for (i__ = *low; i__ <= i__1; ++i__) {
	hr[i__ + i__ * hr_dim1] -= sr;
	hi[i__ + i__ * hi_dim1] -= si;
/* L360: */
    }

    tr += sr;
    ti += si;
    ++its;
    --itn;
/*     .......... REDUCE TO TRIANGLE (ROWS) .......... */
    lp1 = l + 1;

    i__1 = en;
    for (i__ = lp1; i__ <= i__1; ++i__) {
	sr = hr[i__ + (i__ - 1) * hr_dim1];
	hr[i__ + (i__ - 1) * hr_dim1] = 0.;
	d__1 = pythag_(&hr[i__ - 1 + (i__ - 1) * hr_dim1], &hi[i__ - 1 + (i__ 
		- 1) * hi_dim1]);
	norm = pythag_(&d__1, &sr);
	xr = hr[i__ - 1 + (i__ - 1) * hr_dim1] / norm;
	wr[i__ - 1] = xr;
	xi = hi[i__ - 1 + (i__ - 1) * hi_dim1] / norm;
	wi[i__ - 1] = xi;
	hr[i__ - 1 + (i__ - 1) * hr_dim1] = norm;
	hi[i__ - 1 + (i__ - 1) * hi_dim1] = 0.;
	hi[i__ + (i__ - 1) * hi_dim1] = sr / norm;

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    yr = hr[i__ - 1 + j * hr_dim1];
	    yi = hi[i__ - 1 + j * hi_dim1];
	    zzr = hr[i__ + j * hr_dim1];
	    zzi = hi[i__ + j * hi_dim1];
	    hr[i__ - 1 + j * hr_dim1] = xr * yr + xi * yi + hi[i__ + (i__ - 1)
		     * hi_dim1] * zzr;
	    hi[i__ - 1 + j * hi_dim1] = xr * yi - xi * yr + hi[i__ + (i__ - 1)
		     * hi_dim1] * zzi;
	    hr[i__ + j * hr_dim1] = xr * zzr - xi * zzi - hi[i__ + (i__ - 1) *
		     hi_dim1] * yr;
	    hi[i__ + j * hi_dim1] = xr * zzi + xi * zzr - hi[i__ + (i__ - 1) *
		     hi_dim1] * yi;
/* L490: */
	}

/* L500: */
    }

    si = hi[en + en * hi_dim1];
    if (si == 0.) {
	goto L540;
    }
    norm = pythag_(&hr[en + en * hr_dim1], &si);
    sr = hr[en + en * hr_dim1] / norm;
    si /= norm;
    hr[en + en * hr_dim1] = norm;
    hi[en + en * hi_dim1] = 0.;
    if (en == *n) {
	goto L540;
    }
    ip1 = en + 1;

    i__1 = *n;
    for (j = ip1; j <= i__1; ++j) {
	yr = hr[en + j * hr_dim1];
	yi = hi[en + j * hi_dim1];
	hr[en + j * hr_dim1] = sr * yr + si * yi;
	hi[en + j * hi_dim1] = sr * yi - si * yr;
/* L520: */
    }
/*     .......... INVERSE OPERATION (COLUMNS) .......... */
L540:
    i__1 = en;
    for (j = lp1; j <= i__1; ++j) {
	xr = wr[j - 1];
	xi = wi[j - 1];

	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    yr = hr[i__ + (j - 1) * hr_dim1];
	    yi = 0.;
	    zzr = hr[i__ + j * hr_dim1];
	    zzi = hi[i__ + j * hi_dim1];
	    if (i__ == j) {
		goto L560;
	    }
	    yi = hi[i__ + (j - 1) * hi_dim1];
	    hi[i__ + (j - 1) * hi_dim1] = xr * yi + xi * yr + hi[j + (j - 1) *
		     hi_dim1] * zzi;
L560:
	    hr[i__ + (j - 1) * hr_dim1] = xr * yr - xi * yi + hi[j + (j - 1) *
		     hi_dim1] * zzr;
	    hr[i__ + j * hr_dim1] = xr * zzr + xi * zzi - hi[j + (j - 1) * 
		    hi_dim1] * yr;
	    hi[i__ + j * hi_dim1] = xr * zzi - xi * zzr - hi[j + (j - 1) * 
		    hi_dim1] * yi;
/* L580: */
	}

	i__2 = *igh;
	for (i__ = *low; i__ <= i__2; ++i__) {
	    yr = zr[i__ + (j - 1) * zr_dim1];
	    yi = zi[i__ + (j - 1) * zi_dim1];
	    zzr = zr[i__ + j * zr_dim1];
	    zzi = zi[i__ + j * zi_dim1];
	    zr[i__ + (j - 1) * zr_dim1] = xr * yr - xi * yi + hi[j + (j - 1) *
		     hi_dim1] * zzr;
	    zi[i__ + (j - 1) * zi_dim1] = xr * yi + xi * yr + hi[j + (j - 1) *
		     hi_dim1] * zzi;
	    zr[i__ + j * zr_dim1] = xr * zzr + xi * zzi - hi[j + (j - 1) * 
		    hi_dim1] * yr;
	    zi[i__ + j * zi_dim1] = xr * zzi - xi * zzr - hi[j + (j - 1) * 
		    hi_dim1] * yi;
/* L590: */
	}

/* L600: */
    }

    if (si == 0.) {
	goto L240;
    }

    i__1 = en;
    for (i__ = 1; i__ <= i__1; ++i__) {
	yr = hr[i__ + en * hr_dim1];
	yi = hi[i__ + en * hi_dim1];
	hr[i__ + en * hr_dim1] = sr * yr - si * yi;
	hi[i__ + en * hi_dim1] = sr * yi + si * yr;
/* L630: */
    }

    i__1 = *igh;
    for (i__ = *low; i__ <= i__1; ++i__) {
	yr = zr[i__ + en * zr_dim1];
	yi = zi[i__ + en * zi_dim1];
	zr[i__ + en * zr_dim1] = sr * yr - si * yi;
	zi[i__ + en * zi_dim1] = sr * yi + si * yr;
/* L640: */
    }

    goto L240;
/*     .......... A ROOT FOUND .......... */
L660:
    hr[en + en * hr_dim1] += tr;
    wr[en] = hr[en + en * hr_dim1];
    hi[en + en * hi_dim1] += ti;
    wi[en] = hi[en + en * hi_dim1];
    en = enm1;
    goto L220;
/*     .......... ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND */
/*                VECTORS OF UPPER TRIANGULAR FORM .......... */
L680:
    norm = 0.;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    tr = (d__1 = hr[i__ + j * hr_dim1], abs(d__1)) + (d__2 = hi[i__ + 
		    j * hi_dim1], abs(d__2));
	    if (tr > norm) {
		norm = tr;
	    }
/* L720: */
	}
    }

    if (*n == 1 || norm == 0.) {
	goto L1001;
    }
/*     .......... FOR EN=N STEP -1 UNTIL 2 DO -- .......... */
    i__2 = *n;
    for (nn = 2; nn <= i__2; ++nn) {
	en = *n + 2 - nn;
	xr = wr[en];
	xi = wi[en];
	hr[en + en * hr_dim1] = 1.;
	hi[en + en * hi_dim1] = 0.;
	enm1 = en - 1;
/*     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- .......... */
	i__1 = enm1;
	for (ii = 1; ii <= i__1; ++ii) {
	    i__ = en - ii;
	    zzr = 0.;
	    zzi = 0.;
	    ip1 = i__ + 1;

	    i__3 = en;
	    for (j = ip1; j <= i__3; ++j) {
		zzr = zzr + hr[i__ + j * hr_dim1] * hr[j + en * hr_dim1] - hi[
			i__ + j * hi_dim1] * hi[j + en * hi_dim1];
		zzi = zzi + hr[i__ + j * hr_dim1] * hi[j + en * hi_dim1] + hi[
			i__ + j * hi_dim1] * hr[j + en * hr_dim1];
/* L740: */
	    }

	    yr = xr - wr[i__];
	    yi = xi - wi[i__];
	    if (yr != 0. || yi != 0.) {
		goto L765;
	    }
	    tst1 = norm;
	    yr = tst1;
L760:
	    yr *= .01;
	    tst2 = norm + yr;
	    if (tst2 > tst1) {
		goto L760;
	    }
L765:
	    cdiv_(&zzr, &zzi, &yr, &yi, &hr[i__ + en * hr_dim1], &hi[i__ + en 
		    * hi_dim1]);
/*     .......... OVERFLOW CONTROL .......... */
	    tr = (d__1 = hr[i__ + en * hr_dim1], abs(d__1)) + (d__2 = hi[i__ 
		    + en * hi_dim1], abs(d__2));
	    if (tr == 0.) {
		goto L780;
	    }
	    tst1 = tr;
	    tst2 = tst1 + 1. / tst1;
	    if (tst2 > tst1) {
		goto L780;
	    }
	    i__3 = en;
	    for (j = i__; j <= i__3; ++j) {
		hr[j + en * hr_dim1] /= tr;
		hi[j + en * hi_dim1] /= tr;
/* L770: */
	    }

L780:
	    ;
	}

/* L800: */
    }
/*     .......... END BACKSUBSTITUTION .......... */
    enm1 = *n - 1;
/*     .......... VECTORS OF ISOLATED ROOTS .......... */
    i__2 = enm1;
    for (i__ = 1; i__ <= i__2; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L840;
	}
	ip1 = i__ + 1;

	i__1 = *n;
	for (j = ip1; j <= i__1; ++j) {
	    zr[i__ + j * zr_dim1] = hr[i__ + j * hr_dim1];
	    zi[i__ + j * zi_dim1] = hi[i__ + j * hi_dim1];
/* L820: */
	}

L840:
	;
    }
/*     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE */
/*                VECTORS OF ORIGINAL FULL MATRIX. */
/*                FOR J=N STEP -1 UNTIL LOW+1 DO -- .......... */
    i__2 = enm1;
    for (jj = *low; jj <= i__2; ++jj) {
	j = *n + *low - jj;
	m = min(j,*igh);

	i__1 = *igh;
	for (i__ = *low; i__ <= i__1; ++i__) {
	    zzr = 0.;
	    zzi = 0.;

	    i__3 = m;
	    for (k = *low; k <= i__3; ++k) {
		zzr = zzr + zr[i__ + k * zr_dim1] * hr[k + j * hr_dim1] - zi[
			i__ + k * zi_dim1] * hi[k + j * hi_dim1];
		zzi = zzi + zr[i__ + k * zr_dim1] * hi[k + j * hi_dim1] + zi[
			i__ + k * zi_dim1] * hr[k + j * hr_dim1];
/* L860: */
	    }

	    zr[i__ + j * zr_dim1] = zzr;
	    zi[i__ + j * zi_dim1] = zzi;
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
} /* comqr2_ */

