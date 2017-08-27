/* comlr.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int comlr_(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *hr, doublereal *hi, doublereal *wr, doublereal *wi, 
	integer *ierr)
{
    /* System generated locals */
    integer hr_dim1, hr_offset, hi_dim1, hi_offset, i__1, i__2;
    doublereal d__1, d__2, d__3, d__4;

    /* Local variables */
    extern /* Subroutine */ int cdiv_(doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *);
    integer i__, j, l=0, m=0, en, ll, mm;
    doublereal si, ti, xi, yi, sr, tr, xr, yr;
    integer im1;
    extern /* Subroutine */ int csroot_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    integer mp1, itn, its;
    doublereal zzi, zzr;
    integer enm1;
    doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE COMLR, */
/*     NUM. MATH. 12, 369-376(1968) BY MARTIN AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 396-403(1971). */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES OF A COMPLEX */
/*     UPPER HESSENBERG MATRIX BY THE MODIFIED LR METHOD. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED, */
/*          SET LOW=1, IGH=N. */

/*        HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX. */
/*          THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN THE */
/*          MULTIPLIERS WHICH WERE USED IN THE REDUCTION BY  COMHES, */
/*          IF PERFORMED. */

/*     ON OUTPUT */

/*        THE UPPER HESSENBERG PORTIONS OF HR AND HI HAVE BEEN */
/*          DESTROYED.  THEREFORE, THEY MUST BE SAVED BEFORE */
/*          CALLING  COMLR  IF SUBSEQUENT CALCULATION OF */
/*          EIGENVECTORS IS TO BE PERFORMED. */

/*        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE EIGENVALUES.  IF AN ERROR */
/*          EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT */
/*          FOR INDICES IERR+1,...,N. */

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
    --wi;
    --wr;
    hi_dim1 = *nm;
    hi_offset = hi_dim1 + 1;
    hi -= hi_offset;
    hr_dim1 = *nm;
    hr_offset = hr_dim1 + 1;
    hr -= hr_offset;

    /* Function Body */
    *ierr = 0;
/*     .......... STORE ROOTS ISOLATED BY CBAL .......... */
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
	goto L1001;
    }
    its = 0;
    enm1 = en - 1;
/*     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT */
/*                FOR L=EN STEP -1 UNTIL LOW D0 -- .......... */
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
/*     .......... LOOK FOR TWO CONSECUTIVE SMALL */
/*                SUB-DIAGONAL ELEMENTS .......... */
    xr = (d__1 = hr[enm1 + enm1 * hr_dim1], abs(d__1)) + (d__2 = hi[enm1 + 
	    enm1 * hi_dim1], abs(d__2));
    yr = (d__1 = hr[en + enm1 * hr_dim1], abs(d__1)) + (d__2 = hi[en + enm1 * 
	    hi_dim1], abs(d__2));
    zzr = (d__1 = hr[en + en * hr_dim1], abs(d__1)) + (d__2 = hi[en + en * 
	    hi_dim1], abs(d__2));
/*     .......... FOR M=EN-1 STEP -1 UNTIL L DO -- .......... */
    i__1 = enm1;
    for (mm = l; mm <= i__1; ++mm) {
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

    i__1 = en;
    for (i__ = mp1; i__ <= i__1; ++i__) {
	im1 = i__ - 1;
	xr = hr[im1 + im1 * hr_dim1];
	xi = hi[im1 + im1 * hi_dim1];
	yr = hr[i__ + im1 * hr_dim1];
	yi = hi[i__ + im1 * hi_dim1];
	if (abs(xr) + abs(xi) >= abs(yr) + abs(yi)) {
	    goto L460;
	}
/*     .......... INTERCHANGE ROWS OF HR AND HI .......... */
	i__2 = en;
	for (j = im1; j <= i__2; ++j) {
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

	i__2 = en;
	for (j = i__; j <= i__2; ++j) {
	    hr[i__ + j * hr_dim1] = hr[i__ + j * hr_dim1] - zzr * hr[im1 + j *
		     hr_dim1] + zzi * hi[im1 + j * hi_dim1];
	    hi[i__ + j * hi_dim1] = hi[i__ + j * hi_dim1] - zzr * hi[im1 + j *
		     hi_dim1] - zzi * hr[im1 + j * hr_dim1];
/* L500: */
	}

/* L520: */
    }
/*     .......... COMPOSITION R*L=H .......... */
    i__1 = en;
    for (j = mp1; j <= i__1; ++j) {
	xr = hr[j + (j - 1) * hr_dim1];
	xi = hi[j + (j - 1) * hi_dim1];
	hr[j + (j - 1) * hr_dim1] = 0.;
	hi[j + (j - 1) * hi_dim1] = 0.;
/*     .......... INTERCHANGE COLUMNS OF HR AND HI, */
/*                IF NECESSARY .......... */
	if (wr[j] <= 0.) {
	    goto L580;
	}

	i__2 = j;
	for (i__ = l; i__ <= i__2; ++i__) {
	    zzr = hr[i__ + (j - 1) * hr_dim1];
	    hr[i__ + (j - 1) * hr_dim1] = hr[i__ + j * hr_dim1];
	    hr[i__ + j * hr_dim1] = zzr;
	    zzi = hi[i__ + (j - 1) * hi_dim1];
	    hi[i__ + (j - 1) * hi_dim1] = hi[i__ + j * hi_dim1];
	    hi[i__ + j * hi_dim1] = zzi;
/* L540: */
	}

L580:
	i__2 = j;
	for (i__ = l; i__ <= i__2; ++i__) {
	    hr[i__ + (j - 1) * hr_dim1] = hr[i__ + (j - 1) * hr_dim1] + xr * 
		    hr[i__ + j * hr_dim1] - xi * hi[i__ + j * hi_dim1];
	    hi[i__ + (j - 1) * hi_dim1] = hi[i__ + (j - 1) * hi_dim1] + xr * 
		    hi[i__ + j * hi_dim1] + xi * hr[i__ + j * hr_dim1];
/* L600: */
	}

/* L640: */
    }

    goto L240;
/*     .......... A ROOT FOUND .......... */
L660:
    wr[en] = hr[en + en * hr_dim1] + tr;
    wi[en] = hi[en + en * hi_dim1] + ti;
    en = enm1;
    goto L220;
/*     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT */
/*                CONVERGED AFTER 30*N ITERATIONS .......... */
L1000:
    *ierr = en;
L1001:
    return 0;
} /* comlr_ */

