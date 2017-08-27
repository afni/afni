/* invit.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int invit_(integer *nm, integer *n, doublereal *a, 
	doublereal *wr, doublereal *wi, logical *select, integer *mm, integer 
	*m, doublereal *z__, integer *ierr, doublereal *rm1, doublereal *rv1, 
	doublereal *rv2)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, rm1_dim1, rm1_offset, i__1, 
	    i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    extern /* Subroutine */ int cdiv_(doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *);
    doublereal norm;
    integer i__, j=0, k, l, s;
    doublereal t, w, x, y;
    integer n1;
    doublereal normv;
    integer ii;
    doublereal ilambd;
    integer ip, mp, ns=0, uk;
    doublereal rlambd;
    extern doublereal pythag_(doublereal *, doublereal *), epslon_(doublereal 
	    *);
    integer km1, ip1;
    doublereal growto=0.0, ukroot=0.0;
    integer its;
    doublereal eps3=0.0;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE INVIT */
/*     BY PETERS AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971). */

/*     THIS SUBROUTINE FINDS THOSE EIGENVECTORS OF A REAL UPPER */
/*     HESSENBERG MATRIX CORRESPONDING TO SPECIFIED EIGENVALUES, */
/*     USING INVERSE ITERATION. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        A CONTAINS THE HESSENBERG MATRIX. */

/*        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS, RESPECTIVELY, */
/*          OF THE EIGENVALUES OF THE MATRIX.  THE EIGENVALUES MUST BE */
/*          STORED IN A MANNER IDENTICAL TO THAT OF SUBROUTINE  HQR, */
/*          WHICH RECOGNIZES POSSIBLE SPLITTING OF THE MATRIX. */

/*        SELECT SPECIFIES THE EIGENVECTORS TO BE FOUND. THE */
/*          EIGENVECTOR CORRESPONDING TO THE J-TH EIGENVALUE IS */
/*          SPECIFIED BY SETTING SELECT(J) TO .TRUE.. */

/*        MM SHOULD BE SET TO AN UPPER BOUND FOR THE NUMBER OF */
/*          COLUMNS REQUIRED TO STORE THE EIGENVECTORS TO BE FOUND. */
/*          NOTE THAT TWO COLUMNS ARE REQUIRED TO STORE THE */
/*          EIGENVECTOR CORRESPONDING TO A COMPLEX EIGENVALUE. */

/*     ON OUTPUT */

/*        A AND WI ARE UNALTERED. */

/*        WR MAY HAVE BEEN ALTERED SINCE CLOSE EIGENVALUES ARE PERTURBED 
*/
/*          SLIGHTLY IN SEARCHING FOR INDEPENDENT EIGENVECTORS. */

/*        SELECT MAY HAVE BEEN ALTERED.  IF THE ELEMENTS CORRESPONDING */
/*          TO A PAIR OF CONJUGATE COMPLEX EIGENVALUES WERE EACH */
/*          INITIALLY SET TO .TRUE., THE PROGRAM RESETS THE SECOND OF */
/*          THE TWO ELEMENTS TO .FALSE.. */

/*        M IS THE NUMBER OF COLUMNS ACTUALLY USED TO STORE */
/*          THE EIGENVECTORS. */

/*        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS. */
/*          IF THE NEXT SELECTED EIGENVALUE IS REAL, THE NEXT COLUMN */
/*          OF Z CONTAINS ITS EIGENVECTOR.  IF THE EIGENVALUE IS */
/*          COMPLEX, THE NEXT TWO COLUMNS OF Z CONTAIN THE REAL AND */
/*          IMAGINARY PARTS OF ITS EIGENVECTOR.  THE EIGENVECTORS ARE */
/*          NORMALIZED SO THAT THE COMPONENT OF LARGEST MAGNITUDE IS 1. */
/*          ANY VECTOR WHICH FAILS THE ACCEPTANCE TEST IS SET TO ZERO. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          -(2*N+1)   IF MORE THAN MM COLUMNS OF Z ARE NECESSARY */
/*                     TO STORE THE EIGENVECTORS CORRESPONDING TO */
/*                     THE SPECIFIED EIGENVALUES. */
/*          -K         IF THE ITERATION CORRESPONDING TO THE K-TH */
/*                     VALUE FAILS, */
/*          -(N+K)     IF BOTH ERROR SITUATIONS OCCUR. */

/*        RM1, RV1, AND RV2 ARE TEMPORARY STORAGE ARRAYS.  NOTE THAT RM1 
*/
/*          IS SQUARE OF DIMENSION N BY N AND, AUGMENTED BY TWO COLUMNS */
/*          OF Z, IS THE TRANSPOSE OF THE CORRESPONDING ALGOL B ARRAY. */

/*     THE ALGOL PROCEDURE GUESSVEC APPEARS IN INVIT IN LINE. */

/*     CALLS CDIV FOR COMPLEX DIVISION. */
/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --rv2;
    --rv1;
    rm1_dim1 = *n;
    rm1_offset = rm1_dim1 + 1;
    rm1 -= rm1_offset;
    --select;
    --wi;
    --wr;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;

    /* Function Body */
    *ierr = 0;
    uk = 0;
    s = 1;
/*     .......... IP = 0, REAL EIGENVALUE */
/*                     1, FIRST OF CONJUGATE COMPLEX PAIR */
/*                    -1, SECOND OF CONJUGATE COMPLEX PAIR .......... */
    ip = 0;
    n1 = *n - 1;

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	if (wi[k] == 0. || ip < 0) {
	    goto L100;
	}
	ip = 1;
	if (select[k] && select[k + 1]) {
	    select[k + 1] = FALSE_;
	}
L100:
	if (! select[k]) {
	    goto L960;
	}
	if (wi[k] != 0.) {
	    ++s;
	}
	if (s > *mm) {
	    goto L1000;
	}
	if (uk >= k) {
	    goto L200;
	}
/*     .......... CHECK FOR POSSIBLE SPLITTING .......... */
	i__2 = *n;
	for (uk = k; uk <= i__2; ++uk) {
	    if (uk == *n) {
		goto L140;
	    }
	    if (a[uk + 1 + uk * a_dim1] == 0.) {
		goto L140;
	    }
/* L120: */
	}
/*     .......... COMPUTE INFINITY NORM OF LEADING UK BY UK */
/*                (HESSENBERG) MATRIX .......... */
L140:
	norm = 0.;
	mp = 1;

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    x = 0.;

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
/* L160: */
		x += (d__1 = a[i__ + j * a_dim1], abs(d__1));
	    }

	    if (x > norm) {
		norm = x;
	    }
	    mp = i__;
/* L180: */
	}
/*     .......... EPS3 REPLACES ZERO PIVOT IN DECOMPOSITION */
/*                AND CLOSE ROOTS ARE MODIFIED BY EPS3 .......... */
	if (norm == 0.) {
	    norm = 1.;
	}
	eps3 = epslon_(&norm);
/*     .......... GROWTO IS THE CRITERION FOR THE GROWTH .......... */
	ukroot = (doublereal) uk;
	ukroot = sqrt(ukroot);
	growto = .1 / ukroot;
L200:
	rlambd = wr[k];
	ilambd = wi[k];
	if (k == 1) {
	    goto L280;
	}
	km1 = k - 1;
	goto L240;
/*     .......... PERTURB EIGENVALUE IF IT IS CLOSE */
/*                TO ANY PREVIOUS EIGENVALUE .......... */
L220:
	rlambd += eps3;
/*     .......... FOR I=K-1 STEP -1 UNTIL 1 DO -- .......... */
L240:
	i__2 = km1;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = k - ii;
	    if (select[i__] && (d__1 = wr[i__] - rlambd, abs(d__1)) < eps3 && 
		    (d__2 = wi[i__] - ilambd, abs(d__2)) < eps3) {
		goto L220;
	    }
/* L260: */
	}

	wr[k] = rlambd;
/*     .......... PERTURB CONJUGATE EIGENVALUE TO MATCH .......... */
	ip1 = k + ip;
	wr[ip1] = rlambd;
/*     .......... FORM UPPER HESSENBERG A-RLAMBD*I (TRANSPOSED) */
/*                AND INITIAL REAL VECTOR .......... */
L280:
	mp = 1;

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
/* L300: */
		rm1[j + i__ * rm1_dim1] = a[i__ + j * a_dim1];
	    }

	    rm1[i__ + i__ * rm1_dim1] -= rlambd;
	    mp = i__;
	    rv1[i__] = eps3;
/* L320: */
	}

	its = 0;
	if (ilambd != 0.) {
	    goto L520;
	}
/*     .......... REAL EIGENVALUE. */
/*                TRIANGULAR DECOMPOSITION WITH INTERCHANGES, */
/*                REPLACING ZERO PIVOTS BY EPS3 .......... */
	if (uk == 1) {
	    goto L420;
	}

	i__2 = uk;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    mp = i__ - 1;
	    if ((d__1 = rm1[mp + i__ * rm1_dim1], abs(d__1)) <= (d__2 = rm1[
		    mp + mp * rm1_dim1], abs(d__2))) {
		goto L360;
	    }

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
		y = rm1[j + i__ * rm1_dim1];
		rm1[j + i__ * rm1_dim1] = rm1[j + mp * rm1_dim1];
		rm1[j + mp * rm1_dim1] = y;
/* L340: */
	    }

L360:
	    if (rm1[mp + mp * rm1_dim1] == 0.) {
		rm1[mp + mp * rm1_dim1] = eps3;
	    }
	    x = rm1[mp + i__ * rm1_dim1] / rm1[mp + mp * rm1_dim1];
	    if (x == 0.) {
		goto L400;
	    }

	    i__3 = uk;
	    for (j = i__; j <= i__3; ++j) {
/* L380: */
		rm1[j + i__ * rm1_dim1] -= x * rm1[j + mp * rm1_dim1];
	    }

L400:
	    ;
	}

L420:
	if (rm1[uk + uk * rm1_dim1] == 0.) {
	    rm1[uk + uk * rm1_dim1] = eps3;
	}
/*     .......... BACK SUBSTITUTION FOR REAL VECTOR */
/*                FOR I=UK STEP -1 UNTIL 1 DO -- .......... */
L440:
	i__2 = uk;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = uk + 1 - ii;
	    y = rv1[i__];
	    if (i__ == uk) {
		goto L480;
	    }
	    ip1 = i__ + 1;

	    i__3 = uk;
	    for (j = ip1; j <= i__3; ++j) {
/* L460: */
		y -= rm1[j + i__ * rm1_dim1] * rv1[j];
	    }

L480:
	    rv1[i__] = y / rm1[i__ + i__ * rm1_dim1];
/* L500: */
	}

	goto L740;
/*     .......... COMPLEX EIGENVALUE. */
/*                TRIANGULAR DECOMPOSITION WITH INTERCHANGES, */
/*                REPLACING ZERO PIVOTS BY EPS3.  STORE IMAGINARY */
/*                PARTS IN UPPER TRIANGLE STARTING AT (1,3) ..........
 */
L520:
	ns = *n - s;
	z__[(s - 1) * z_dim1 + 1] = -ilambd;
	z__[s * z_dim1 + 1] = 0.;
	if (*n == 2) {
	    goto L550;
	}
	rm1[rm1_dim1 * 3 + 1] = -ilambd;
	z__[(s - 1) * z_dim1 + 1] = 0.;
	if (*n == 3) {
	    goto L550;
	}

	i__2 = *n;
	for (i__ = 4; i__ <= i__2; ++i__) {
/* L540: */
	    rm1[i__ * rm1_dim1 + 1] = 0.;
	}

L550:
	i__2 = uk;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    mp = i__ - 1;
	    w = rm1[mp + i__ * rm1_dim1];
	    if (i__ < *n) {
		t = rm1[mp + (i__ + 1) * rm1_dim1];
	    }
	    if (i__ == *n) {
		t = z__[mp + (s - 1) * z_dim1];
	    }
	    x = rm1[mp + mp * rm1_dim1] * rm1[mp + mp * rm1_dim1] + t * t;
	    if (w * w <= x) {
		goto L580;
	    }
	    x = rm1[mp + mp * rm1_dim1] / w;
	    y = t / w;
	    rm1[mp + mp * rm1_dim1] = w;
	    if (i__ < *n) {
		rm1[mp + (i__ + 1) * rm1_dim1] = 0.;
	    }
	    if (i__ == *n) {
		z__[mp + (s - 1) * z_dim1] = 0.;
	    }

	    i__3 = uk;
	    for (j = i__; j <= i__3; ++j) {
		w = rm1[j + i__ * rm1_dim1];
		rm1[j + i__ * rm1_dim1] = rm1[j + mp * rm1_dim1] - x * w;
		rm1[j + mp * rm1_dim1] = w;
		if (j < n1) {
		    goto L555;
		}
		l = j - ns;
		z__[i__ + l * z_dim1] = z__[mp + l * z_dim1] - y * w;
		z__[mp + l * z_dim1] = 0.;
		goto L560;
L555:
		rm1[i__ + (j + 2) * rm1_dim1] = rm1[mp + (j + 2) * rm1_dim1] 
			- y * w;
		rm1[mp + (j + 2) * rm1_dim1] = 0.;
L560:
		;
	    }

	    rm1[i__ + i__ * rm1_dim1] -= y * ilambd;
	    if (i__ < n1) {
		goto L570;
	    }
	    l = i__ - ns;
	    z__[mp + l * z_dim1] = -ilambd;
	    z__[i__ + l * z_dim1] += x * ilambd;
	    goto L640;
L570:
	    rm1[mp + (i__ + 2) * rm1_dim1] = -ilambd;
	    rm1[i__ + (i__ + 2) * rm1_dim1] += x * ilambd;
	    goto L640;
L580:
	    if (x != 0.) {
		goto L600;
	    }
	    rm1[mp + mp * rm1_dim1] = eps3;
	    if (i__ < *n) {
		rm1[mp + (i__ + 1) * rm1_dim1] = 0.;
	    }
	    if (i__ == *n) {
		z__[mp + (s - 1) * z_dim1] = 0.;
	    }
	    t = 0.;
	    x = eps3 * eps3;
L600:
	    w /= x;
	    x = rm1[mp + mp * rm1_dim1] * w;
	    y = -t * w;

	    i__3 = uk;
	    for (j = i__; j <= i__3; ++j) {
		if (j < n1) {
		    goto L610;
		}
		l = j - ns;
		t = z__[mp + l * z_dim1];
		z__[i__ + l * z_dim1] = -x * t - y * rm1[j + mp * rm1_dim1];
		goto L615;
L610:
		t = rm1[mp + (j + 2) * rm1_dim1];
		rm1[i__ + (j + 2) * rm1_dim1] = -x * t - y * rm1[j + mp * 
			rm1_dim1];
L615:
		rm1[j + i__ * rm1_dim1] = rm1[j + i__ * rm1_dim1] - x * rm1[j 
			+ mp * rm1_dim1] + y * t;
/* L620: */
	    }

	    if (i__ < n1) {
		goto L630;
	    }
	    l = i__ - ns;
	    z__[i__ + l * z_dim1] -= ilambd;
	    goto L640;
L630:
	    rm1[i__ + (i__ + 2) * rm1_dim1] -= ilambd;
L640:
	    ;
	}

	if (uk < n1) {
	    goto L650;
	}
	l = uk - ns;
	t = z__[uk + l * z_dim1];
	goto L655;
L650:
	t = rm1[uk + (uk + 2) * rm1_dim1];
L655:
	if (rm1[uk + uk * rm1_dim1] == 0. && t == 0.) {
	    rm1[uk + uk * rm1_dim1] = eps3;
	}
/*     .......... BACK SUBSTITUTION FOR COMPLEX VECTOR */
/*                FOR I=UK STEP -1 UNTIL 1 DO -- .......... */
L660:
	i__2 = uk;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = uk + 1 - ii;
	    x = rv1[i__];
	    y = 0.;
	    if (i__ == uk) {
		goto L700;
	    }
	    ip1 = i__ + 1;

	    i__3 = uk;
	    for (j = ip1; j <= i__3; ++j) {
		if (j < n1) {
		    goto L670;
		}
		l = j - ns;
		t = z__[i__ + l * z_dim1];
		goto L675;
L670:
		t = rm1[i__ + (j + 2) * rm1_dim1];
L675:
		x = x - rm1[j + i__ * rm1_dim1] * rv1[j] + t * rv2[j];
		y = y - rm1[j + i__ * rm1_dim1] * rv2[j] - t * rv1[j];
/* L680: */
	    }

L700:
	    if (i__ < n1) {
		goto L710;
	    }
	    l = i__ - ns;
	    t = z__[i__ + l * z_dim1];
	    goto L715;
L710:
	    t = rm1[i__ + (i__ + 2) * rm1_dim1];
L715:
	    cdiv_(&x, &y, &rm1[i__ + i__ * rm1_dim1], &t, &rv1[i__], &rv2[i__]
		    );
/* L720: */
	}
/*     .......... ACCEPTANCE TEST FOR REAL OR COMPLEX */
/*                EIGENVECTOR AND NORMALIZATION .......... */
L740:
	++its;
	norm = 0.;
	normv = 0.;

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    if (ilambd == 0.) {
		x = (d__1 = rv1[i__], abs(d__1));
	    }
	    if (ilambd != 0.) {
		x = pythag_(&rv1[i__], &rv2[i__]);
	    }
	    if (normv >= x) {
		goto L760;
	    }
	    normv = x;
	    j = i__;
L760:
	    norm += x;
/* L780: */
	}

	if (norm < growto) {
	    goto L840;
	}
/*     .......... ACCEPT VECTOR .......... */
	x = rv1[j];
	if (ilambd == 0.) {
	    x = 1. / x;
	}
	if (ilambd != 0.) {
	    y = rv2[j];
	}

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    if (ilambd != 0.) {
		goto L800;
	    }
	    z__[i__ + s * z_dim1] = rv1[i__] * x;
	    goto L820;
L800:
	    cdiv_(&rv1[i__], &rv2[i__], &x, &y, &z__[i__ + (s - 1) * z_dim1], 
		    &z__[i__ + s * z_dim1]);
L820:
	    ;
	}

	if (uk == *n) {
	    goto L940;
	}
	j = uk + 1;
	goto L900;
/*     .......... IN-LINE PROCEDURE FOR CHOOSING */
/*                A NEW STARTING VECTOR .......... */
L840:
	if (its >= uk) {
	    goto L880;
	}
	x = ukroot;
	y = eps3 / (x + 1.);
	rv1[1] = eps3;

	i__2 = uk;
	for (i__ = 2; i__ <= i__2; ++i__) {
/* L860: */
	    rv1[i__] = y;
	}

	j = uk - its + 1;
	rv1[j] -= eps3 * x;
	if (ilambd == 0.) {
	    goto L440;
	}
	goto L660;
/*     .......... SET ERROR -- UNACCEPTED EIGENVECTOR .......... */
L880:
	j = 1;
	*ierr = -k;
/*     .......... SET REMAINING VECTOR COMPONENTS TO ZERO .......... 
*/
L900:
	i__2 = *n;
	for (i__ = j; i__ <= i__2; ++i__) {
	    z__[i__ + s * z_dim1] = 0.;
	    if (ilambd != 0.) {
		z__[i__ + (s - 1) * z_dim1] = 0.;
	    }
/* L920: */
	}

L940:
	++s;
L960:
	if (ip == -1) {
	    ip = 0;
	}
	if (ip == 1) {
	    ip = -1;
	}
/* L980: */
    }

    goto L1001;
/*     .......... SET ERROR -- UNDERESTIMATE OF EIGENVECTOR */
/*                SPACE REQUIRED .......... */
L1000:
    if (*ierr != 0) {
	*ierr -= *n;
    }
    if (*ierr == 0) {
	*ierr = -((*n << 1) + 1);
    }
L1001:
    *m = s - 1 - abs(ip);
    return 0;
} /* invit_ */

