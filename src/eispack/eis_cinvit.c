/* cinvit.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int cinvit_(integer *nm, integer *n, doublereal *ar, 
	doublereal *ai, doublereal *wr, doublereal *wi, logical *select, 
	integer *mm, integer *m, doublereal *zr, doublereal *zi, integer *
	ierr, doublereal *rm1, doublereal *rm2, doublereal *rv1, doublereal *
	rv2)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, rm1_dim1, rm1_offset, rm2_dim1, rm2_offset, 
	    i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    extern /* Subroutine */ int cdiv_(doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *);
    doublereal norm;
    integer i__, j=0, k, s;
    doublereal x, y, normv;
    integer ii;
    doublereal ilambd;
    integer mp, uk;
    doublereal rlambd;
    extern doublereal pythag_(doublereal *, doublereal *), epslon_(doublereal 
	    *);
    integer km1, ip1;
    doublereal growto=0.0, ukroot=0.0;
    integer its;
    doublereal eps3=0.0;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE CX INVIT */
/*     BY PETERS AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP. VOL.II-LINEAR ALGEBRA, 418-439(1971). */

/*     THIS SUBROUTINE FINDS THOSE EIGENVECTORS OF A COMPLEX UPPER */
/*     HESSENBERG MATRIX CORRESPONDING TO SPECIFIED EIGENVALUES, */
/*     USING INVERSE ITERATION. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE HESSENBERG MATRIX. */

/*        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS, RESPECTIVELY, */
/*          OF THE EIGENVALUES OF THE MATRIX.  THE EIGENVALUES MUST BE */
/*          STORED IN A MANNER IDENTICAL TO THAT OF SUBROUTINE  COMLR, */
/*          WHICH RECOGNIZES POSSIBLE SPLITTING OF THE MATRIX. */

/*        SELECT SPECIFIES THE EIGENVECTORS TO BE FOUND.  THE */
/*          EIGENVECTOR CORRESPONDING TO THE J-TH EIGENVALUE IS */
/*          SPECIFIED BY SETTING SELECT(J) TO .TRUE.. */

/*        MM SHOULD BE SET TO AN UPPER BOUND FOR THE NUMBER OF */
/*          EIGENVECTORS TO BE FOUND. */

/*     ON OUTPUT */

/*        AR, AI, WI, AND SELECT ARE UNALTERED. */

/*        WR MAY HAVE BEEN ALTERED SINCE CLOSE EIGENVALUES ARE PERTURBED 
*/
/*          SLIGHTLY IN SEARCHING FOR INDEPENDENT EIGENVECTORS. */

/*        M IS THE NUMBER OF EIGENVECTORS ACTUALLY FOUND. */

/*        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS, RESPECTIVELY, */
/*          OF THE EIGENVECTORS.  THE EIGENVECTORS ARE NORMALIZED */
/*          SO THAT THE COMPONENT OF LARGEST MAGNITUDE IS 1. */
/*          ANY VECTOR WHICH FAILS THE ACCEPTANCE TEST IS SET TO ZERO. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          -(2*N+1)   IF MORE THAN MM EIGENVECTORS HAVE BEEN SPECIFIED, 
*/
/*          -K         IF THE ITERATION CORRESPONDING TO THE K-TH */
/*                     VALUE FAILS, */
/*          -(N+K)     IF BOTH ERROR SITUATIONS OCCUR. */

/*        RM1, RM2, RV1, AND RV2 ARE TEMPORARY STORAGE ARRAYS. */

/*     THE ALGOL PROCEDURE GUESSVEC APPEARS IN CINVIT IN LINE. */

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
    rm2_dim1 = *n;
    rm2_offset = rm2_dim1 + 1;
    rm2 -= rm2_offset;
    rm1_dim1 = *n;
    rm1_offset = rm1_dim1 + 1;
    rm1 -= rm1_offset;
    --select;
    --wi;
    --wr;
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;

    /* Function Body */
    *ierr = 0;
    uk = 0;
    s = 1;

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	if (! select[k]) {
	    goto L980;
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
	    if (ar[uk + 1 + uk * ar_dim1] == 0. && ai[uk + 1 + uk * ai_dim1] 
		    == 0.) {
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
		x += pythag_(&ar[i__ + j * ar_dim1], &ai[i__ + j * ai_dim1]);
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
/*     .......... GROWTO IS THE CRITERION FOR GROWTH .......... */
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
/*     .......... FORM UPPER HESSENBERG (AR,AI)-(RLAMBD,ILAMBD)*I */
/*                AND INITIAL COMPLEX VECTOR .......... */
L280:
	mp = 1;

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
		rm1[i__ + j * rm1_dim1] = ar[i__ + j * ar_dim1];
		rm2[i__ + j * rm2_dim1] = ai[i__ + j * ai_dim1];
/* L300: */
	    }

	    rm1[i__ + i__ * rm1_dim1] -= rlambd;
	    rm2[i__ + i__ * rm2_dim1] -= ilambd;
	    mp = i__;
	    rv1[i__] = eps3;
/* L320: */
	}
/*     .......... TRIANGULAR DECOMPOSITION WITH INTERCHANGES, */
/*                REPLACING ZERO PIVOTS BY EPS3 .......... */
	if (uk == 1) {
	    goto L420;
	}

	i__2 = uk;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    mp = i__ - 1;
	    if (pythag_(&rm1[i__ + mp * rm1_dim1], &rm2[i__ + mp * rm2_dim1]) 
		    <= pythag_(&rm1[mp + mp * rm1_dim1], &rm2[mp + mp * 
		    rm2_dim1])) {
		goto L360;
	    }

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
		y = rm1[i__ + j * rm1_dim1];
		rm1[i__ + j * rm1_dim1] = rm1[mp + j * rm1_dim1];
		rm1[mp + j * rm1_dim1] = y;
		y = rm2[i__ + j * rm2_dim1];
		rm2[i__ + j * rm2_dim1] = rm2[mp + j * rm2_dim1];
		rm2[mp + j * rm2_dim1] = y;
/* L340: */
	    }

L360:
	    if (rm1[mp + mp * rm1_dim1] == 0. && rm2[mp + mp * rm2_dim1] == 
		    0.) {
		rm1[mp + mp * rm1_dim1] = eps3;
	    }
	    cdiv_(&rm1[i__ + mp * rm1_dim1], &rm2[i__ + mp * rm2_dim1], &rm1[
		    mp + mp * rm1_dim1], &rm2[mp + mp * rm2_dim1], &x, &y);
	    if (x == 0. && y == 0.) {
		goto L400;
	    }

	    i__3 = uk;
	    for (j = i__; j <= i__3; ++j) {
		rm1[i__ + j * rm1_dim1] = rm1[i__ + j * rm1_dim1] - x * rm1[
			mp + j * rm1_dim1] + y * rm2[mp + j * rm2_dim1];
		rm2[i__ + j * rm2_dim1] = rm2[i__ + j * rm2_dim1] - x * rm2[
			mp + j * rm2_dim1] - y * rm1[mp + j * rm1_dim1];
/* L380: */
	    }

L400:
	    ;
	}

L420:
	if (rm1[uk + uk * rm1_dim1] == 0. && rm2[uk + uk * rm2_dim1] == 0.) {
	    rm1[uk + uk * rm1_dim1] = eps3;
	}
	its = 0;
/*     .......... BACK SUBSTITUTION */
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
		x = x - rm1[i__ + j * rm1_dim1] * rv1[j] + rm2[i__ + j * 
			rm2_dim1] * rv2[j];
		y = y - rm1[i__ + j * rm1_dim1] * rv2[j] - rm2[i__ + j * 
			rm2_dim1] * rv1[j];
/* L680: */
	    }

L700:
	    cdiv_(&x, &y, &rm1[i__ + i__ * rm1_dim1], &rm2[i__ + i__ * 
		    rm2_dim1], &rv1[i__], &rv2[i__]);
/* L720: */
	}
/*     .......... ACCEPTANCE TEST FOR EIGENVECTOR */
/*                AND NORMALIZATION .......... */
	++its;
	norm = 0.;
	normv = 0.;

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    x = pythag_(&rv1[i__], &rv2[i__]);
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
	y = rv2[j];

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    cdiv_(&rv1[i__], &rv2[i__], &x, &y, &zr[i__ + s * zr_dim1], &zi[
		    i__ + s * zi_dim1]);
/* L820: */
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
	    zr[i__ + s * zr_dim1] = 0.;
	    zi[i__ + s * zi_dim1] = 0.;
/* L920: */
	}

L940:
	++s;
L980:
	;
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
    *m = s - 1;
    return 0;
} /* cinvit_ */

