/* comlr2.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int comlr2_(integer *nm, integer *n, integer *low, integer *
	igh, integer *int__, doublereal *hr, doublereal *hi, doublereal *wr, 
	doublereal *wi, doublereal *zr, doublereal *zi, integer *ierr)
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
    integer i__, j, k, l=0, m=0, ii, en, jj, ll, mm, nn;
    doublereal si, ti, xi, yi, sr, tr, xr, yr;
    integer im1;
    extern /* Subroutine */ int csroot_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    integer ip1, mp1, itn, its;
    doublereal zzi, zzr;
    integer enm1;
    doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE COMLR2, */
/*     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971). */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS */
/*     OF A COMPLEX UPPER HESSENBERG MATRIX BY THE MODIFIED LR */
/*     METHOD.  THE EIGENVECTORS OF A COMPLEX GENERAL MATRIX */
/*     CAN ALSO BE FOUND IF  COMHES  HAS BEEN USED TO REDUCE */
/*     THIS GENERAL MATRIX TO HESSENBERG FORM. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED, */
/*          SET LOW=1, IGH=N. */

/*        INT CONTAINS INFORMATION ON THE ROWS AND COLUMNS INTERCHANGED */
/*          IN THE REDUCTION BY  COMHES, IF PERFORMED.  ONLY ELEMENTS */
/*          LOW THROUGH IGH ARE USED.  IF THE EIGENVECTORS OF THE HESSEN- 
*/
/*          BERG MATRIX ARE DESIRED, SET INT(J)=J FOR THESE ELEMENTS. */

/*        HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX. */
/*          THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN THE */
/*          MULTIPLIERS WHICH WERE USED IN THE REDUCTION BY  COMHES, */
/*          IF PERFORMED.  IF THE EIGENVECTORS OF THE HESSENBERG */
/*          MATRIX ARE DESIRED, THESE ELEMENTS MUST BE SET TO ZERO. */

/*     ON OUTPUT */

/*        THE UPPER HESSENBERG PORTIONS OF HR AND HI HAVE BEEN */
/*          DESTROYED, BUT THE LOCATION HR(1,1) CONTAINS THE NORM */
/*          OF THE TRIANGULARIZED MATRIX. */

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
    --int__;

    /* Function Body */
    *ierr = 0;
/*     .......... INITIALIZE EIGENVECTOR MATRIX .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    zr[i__ + j * zr_dim1] = 0.;
	    zi[i__ + j * zi_dim1] = 0.;
	    if (i__ == j) {
		zr[i__ + j * zr_dim1] = 1.;
	    }
/* L100: */
	}
    }
/*     .......... FORM THE MATRIX OF ACCUMULATED TRANSFORMATIONS */
/*                FROM THE INFORMATION LEFT BY COMHES .......... */
    iend = *igh - *low - 1;
    if (iend <= 0) {
	goto L180;
    }
/*     .......... FOR I=IGH-1 STEP -1 UNTIL LOW+1 DO -- .......... */
    i__2 = iend;
    for (ii = 1; ii <= i__2; ++ii) {
	i__ = *igh - ii;
	ip1 = i__ + 1;

	i__1 = *igh;
	for (k = ip1; k <= i__1; ++k) {
	    zr[k + i__ * zr_dim1] = hr[k + (i__ - 1) * hr_dim1];
	    zi[k + i__ * zi_dim1] = hi[k + (i__ - 1) * hi_dim1];
/* L120: */
	}

	j = int__[i__];
	if (i__ == j) {
	    goto L160;
	}

	i__1 = *igh;
	for (k = i__; k <= i__1; ++k) {
	    zr[i__ + k * zr_dim1] = zr[j + k * zr_dim1];
	    zi[i__ + k * zi_dim1] = zi[j + k * zi_dim1];
	    zr[j + k * zr_dim1] = 0.;
	    zi[j + k * zi_dim1] = 0.;
/* L140: */
	}

	zr[j + i__ * zr_dim1] = 1.;
L160:
	;
    }
/*     .......... STORE ROOTS ISOLATED BY CBAL .......... */
L180:
    i__2 = *n;
    for (i__ = 1; i__ <= i__2; ++i__) {
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
    i__2 = en;
    for (ll = *low; ll <= i__2; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L300;
	}
	tst1 = (d__1 = hr[l - 1 + (l - 1) * hr_dim1], abs(d__1)) + (d__2 = hi[
		l - 1 + (l - 1) * hi_dim1], abs(d__2)) + (d__3 = hr[l + l * 
		hr_dim1], abs(d__3)) + (d__4 = hi[l + l * hi_dim1], abs(d__4))
		;
	tst2 = tst1 + (d__1 = hr[l + (l - 1) * hr_dim1], abs(d__1)) + (d__2 = 
		hi[l + (l - 1) * hi_dim1], abs(d__2));
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
    xr = hr[enm1 + en * hr_dim1] * hr[en + enm1 * hr_dim1] - hi[enm1 + en * 
	    hi_dim1] * hi[en + enm1 * hi_dim1];
    xi = hr[enm1 + en * hr_dim1] * hi[en + enm1 * hi_dim1] + hi[enm1 + en * 
	    hi_dim1] * hr[en + enm1 * hr_dim1];
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
    si = (d__1 = hi[en + enm1 * hi_dim1], abs(d__1)) + (d__2 = hi[enm1 + (en 
	    - 2) * hi_dim1], abs(d__2));

L340:
    i__2 = en;
    for (i__ = *low; i__ <= i__2; ++i__) {
	hr[i__ + i__ * hr_dim1] -= sr;
	hi[i__ + i__ * hi_dim1] -= si;
/* L360: */
    }

    tr += sr;
    ti += si;
    ++its;
    --itn;
/*     .......... LOOK FOR TWO CONSECUTIVE SMALL */
/*                SUB-DIAGONAL ELEMENTS .......... */
    xr = (d__1 = hr[enm1 + enm1 * hr_dim1], abs(d__1)) + (d__2 = hi[enm1 + 
	    enm1 * hi_dim1], abs(d__2));
    yr = (d__1 = hr[en + enm1 * hr_dim1], abs(d__1)) + (d__2 = hi[en + enm1 * 
	    hi_dim1], abs(d__2));
    zzr = (d__1 = hr[en + en * hr_dim1], abs(d__1)) + (d__2 = hi[en + en * 
	    hi_dim1], abs(d__2));
/*     .......... FOR M=EN-1 STEP -1 UNTIL L DO -- .......... */
    i__2 = enm1;
    for (mm = l; mm <= i__2; ++mm) {
	m = enm1 + l - mm;
	if (m == l) {
	    goto L420;
	}
	yi = yr;
	yr = (d__1 = hr[m + (m - 1) * hr_dim1], abs(d__1)) + (d__2 = hi[m + (
		m - 1) * hi_dim1], abs(d__2));
	xi = zzr;
	zzr = xr;
	xr = (d__1 = hr[m - 1 + (m - 1) * hr_dim1], abs(d__1)) + (d__2 = hi[m 
		- 1 + (m - 1) * hi_dim1], abs(d__2));
	tst1 = zzr / yi * (zzr + xr + xi);
	tst2 = tst1 + yr;
	if (tst2 == tst1) {
	    goto L420;
	}
/* L380: */
    }
/*     .......... TRIANGULAR DECOMPOSITION H=L*R .......... */
L420:
    mp1 = m + 1;

    i__2 = en;
    for (i__ = mp1; i__ <= i__2; ++i__) {
	im1 = i__ - 1;
	xr = hr[im1 + im1 * hr_dim1];
	xi = hi[im1 + im1 * hi_dim1];
	yr = hr[i__ + im1 * hr_dim1];
	yi = hi[i__ + im1 * hi_dim1];
	if (abs(xr) + abs(xi) >= abs(yr) + abs(yi)) {
	    goto L460;
	}
/*     .......... INTERCHANGE ROWS OF HR AND HI .......... */
	i__1 = *n;
	for (j = im1; j <= i__1; ++j) {
	    zzr = hr[im1 + j * hr_dim1];
	    hr[im1 + j * hr_dim1] = hr[i__ + j * hr_dim1];
	    hr[i__ + j * hr_dim1] = zzr;
	    zzi = hi[im1 + j * hi_dim1];
	    hi[im1 + j * hi_dim1] = hi[i__ + j * hi_dim1];
	    hi[i__ + j * hi_dim1] = zzi;
/* L440: */
	}

	cdiv_(&xr, &xi, &yr, &yi, &zzr, &zzi);
	wr[i__] = 1.;
	goto L480;
L460:
	cdiv_(&yr, &yi, &xr, &xi, &zzr, &zzi);
	wr[i__] = -1.;
L480:
	hr[i__ + im1 * hr_dim1] = zzr;
	hi[i__ + im1 * hi_dim1] = zzi;

	i__1 = *n;
	for (j = i__; j <= i__1; ++j) {
	    hr[i__ + j * hr_dim1] = hr[i__ + j * hr_dim1] - zzr * hr[im1 + j *
		     hr_dim1] + zzi * hi[im1 + j * hi_dim1];
	    hi[i__ + j * hi_dim1] = hi[i__ + j * hi_dim1] - zzr * hi[im1 + j *
		     hi_dim1] - zzi * hr[im1 + j * hr_dim1];
/* L500: */
	}

/* L520: */
    }
/*     .......... COMPOSITION R*L=H .......... */
    i__2 = en;
    for (j = mp1; j <= i__2; ++j) {
	xr = hr[j + (j - 1) * hr_dim1];
	xi = hi[j + (j - 1) * hi_dim1];
	hr[j + (j - 1) * hr_dim1] = 0.;
	hi[j + (j - 1) * hi_dim1] = 0.;
/*     .......... INTERCHANGE COLUMNS OF HR, HI, ZR, AND ZI, */
/*                IF NECESSARY .......... */
	if (wr[j] <= 0.) {
	    goto L580;
	}

	i__1 = j;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    zzr = hr[i__ + (j - 1) * hr_dim1];
	    hr[i__ + (j - 1) * hr_dim1] = hr[i__ + j * hr_dim1];
	    hr[i__ + j * hr_dim1] = zzr;
	    zzi = hi[i__ + (j - 1) * hi_dim1];
	    hi[i__ + (j - 1) * hi_dim1] = hi[i__ + j * hi_dim1];
	    hi[i__ + j * hi_dim1] = zzi;
/* L540: */
	}

	i__1 = *igh;
	for (i__ = *low; i__ <= i__1; ++i__) {
	    zzr = zr[i__ + (j - 1) * zr_dim1];
	    zr[i__ + (j - 1) * zr_dim1] = zr[i__ + j * zr_dim1];
	    zr[i__ + j * zr_dim1] = zzr;
	    zzi = zi[i__ + (j - 1) * zi_dim1];
	    zi[i__ + (j - 1) * zi_dim1] = zi[i__ + j * zi_dim1];
	    zi[i__ + j * zi_dim1] = zzi;
/* L560: */
	}

L580:
	i__1 = j;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    hr[i__ + (j - 1) * hr_dim1] = hr[i__ + (j - 1) * hr_dim1] + xr * 
		    hr[i__ + j * hr_dim1] - xi * hi[i__ + j * hi_dim1];
	    hi[i__ + (j - 1) * hi_dim1] = hi[i__ + (j - 1) * hi_dim1] + xr * 
		    hi[i__ + j * hi_dim1] + xi * hr[i__ + j * hr_dim1];
/* L600: */
	}
/*     .......... ACCUMULATE TRANSFORMATIONS .......... */
	i__1 = *igh;
	for (i__ = *low; i__ <= i__1; ++i__) {
	    zr[i__ + (j - 1) * zr_dim1] = zr[i__ + (j - 1) * zr_dim1] + xr * 
		    zr[i__ + j * zr_dim1] - xi * zi[i__ + j * zi_dim1];
	    zi[i__ + (j - 1) * zi_dim1] = zi[i__ + (j - 1) * zi_dim1] + xr * 
		    zi[i__ + j * zi_dim1] + xi * zr[i__ + j * zr_dim1];
/* L620: */
	}

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

    i__2 = *n;
    for (i__ = 1; i__ <= i__2; ++i__) {

	i__1 = *n;
	for (j = i__; j <= i__1; ++j) {
	    tr = (d__1 = hr[i__ + j * hr_dim1], abs(d__1)) + (d__2 = hi[i__ + 
		    j * hi_dim1], abs(d__2));
	    if (tr > norm) {
		norm = tr;
	    }
/* L720: */
	}
    }

    hr[hr_dim1 + 1] = norm;
    if (*n == 1 || norm == 0.) {
	goto L1001;
    }
/*     .......... FOR EN=N STEP -1 UNTIL 2 DO -- .......... */
    i__1 = *n;
    for (nn = 2; nn <= i__1; ++nn) {
	en = *n + 2 - nn;
	xr = wr[en];
	xi = wi[en];
	hr[en + en * hr_dim1] = 1.;
	hi[en + en * hi_dim1] = 0.;
	enm1 = en - 1;
/*     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = enm1;
	for (ii = 1; ii <= i__2; ++ii) {
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
    i__1 = enm1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L840;
	}
	ip1 = i__ + 1;

	i__2 = *n;
	for (j = ip1; j <= i__2; ++j) {
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
    i__1 = enm1;
    for (jj = *low; jj <= i__1; ++jj) {
	j = *n + *low - jj;
	m = min(j,*igh);

	i__2 = *igh;
	for (i__ = *low; i__ <= i__2; ++i__) {
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
} /* comlr2_ */

