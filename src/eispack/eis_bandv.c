/* bandv.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int bandv_(integer *nm, integer *n, integer *mbw, doublereal 
	*a, doublereal *e21, integer *m, doublereal *w, doublereal *z__, 
	integer *ierr, integer *nv, doublereal *rv, doublereal *rv6)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3, i__4, i__5;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    integer maxj, maxk;
    doublereal norm;
    integer i__, j, k, r__;
    doublereal u, v, order;
    integer group=0, m1;
    doublereal x0=0.0, x1;
    integer mb, m21, ii, ij, jj, kj;
    doublereal uk=0.0, xu;
    extern doublereal pythag_(doublereal *, doublereal *), epslon_(doublereal 
	    *);
    integer ij1, kj1, its;
    doublereal eps2=0.0, eps3, eps4=0.0;



/*     THIS SUBROUTINE FINDS THOSE EIGENVECTORS OF A REAL SYMMETRIC */
/*     BAND MATRIX CORRESPONDING TO SPECIFIED EIGENVALUES, USING INVERSE 
*/
/*     ITERATION.  THE SUBROUTINE MAY ALSO BE USED TO SOLVE SYSTEMS */
/*     OF LINEAR EQUATIONS WITH A SYMMETRIC OR NON-SYMMETRIC BAND */
/*     COEFFICIENT MATRIX. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        MBW IS THE NUMBER OF COLUMNS OF THE ARRAY A USED TO STORE THE */
/*          BAND MATRIX.  IF THE MATRIX IS SYMMETRIC, MBW IS ITS (HALF) */
/*          BAND WIDTH, DENOTED MB AND DEFINED AS THE NUMBER OF ADJACENT 
*/
/*          DIAGONALS, INCLUDING THE PRINCIPAL DIAGONAL, REQUIRED TO */
/*          SPECIFY THE NON-ZERO PORTION OF THE LOWER TRIANGLE OF THE */
/*          MATRIX.  IF THE SUBROUTINE IS BEING USED TO SOLVE SYSTEMS */
/*          OF LINEAR EQUATIONS AND THE COEFFICIENT MATRIX IS NOT */
/*          SYMMETRIC, IT MUST HOWEVER HAVE THE SAME NUMBER OF ADJACENT */
/*          DIAGONALS ABOVE THE MAIN DIAGONAL AS BELOW, AND IN THIS */
/*          CASE, MBW=2*MB-1. */

/*        A CONTAINS THE LOWER TRIANGLE OF THE SYMMETRIC BAND INPUT */
/*          MATRIX STORED AS AN N BY MB ARRAY.  ITS LOWEST SUBDIAGONAL */
/*          IS STORED IN THE LAST N+1-MB POSITIONS OF THE FIRST COLUMN, */
/*          ITS NEXT SUBDIAGONAL IN THE LAST N+2-MB POSITIONS OF THE */
/*          SECOND COLUMN, FURTHER SUBDIAGONALS SIMILARLY, AND FINALLY */
/*          ITS PRINCIPAL DIAGONAL IN THE N POSITIONS OF COLUMN MB. */
/*          IF THE SUBROUTINE IS BEING USED TO SOLVE SYSTEMS OF LINEAR */
/*          EQUATIONS AND THE COEFFICIENT MATRIX IS NOT SYMMETRIC, A IS */
/*          N BY 2*MB-1 INSTEAD WITH LOWER TRIANGLE AS ABOVE AND WITH */
/*          ITS FIRST SUPERDIAGONAL STORED IN THE FIRST N-1 POSITIONS OF 
*/
/*          COLUMN MB+1, ITS SECOND SUPERDIAGONAL IN THE FIRST N-2 */
/*          POSITIONS OF COLUMN MB+2, FURTHER SUPERDIAGONALS SIMILARLY, */
/*          AND FINALLY ITS HIGHEST SUPERDIAGONAL IN THE FIRST N+1-MB */
/*          POSITIONS OF THE LAST COLUMN. */
/*          CONTENTS OF STORAGES NOT PART OF THE MATRIX ARE ARBITRARY. */

/*        E21 SPECIFIES THE ORDERING OF THE EIGENVALUES AND CONTAINS */
/*            0.0D0 IF THE EIGENVALUES ARE IN ASCENDING ORDER, OR */
/*            2.0D0 IF THE EIGENVALUES ARE IN DESCENDING ORDER. */
/*          IF THE SUBROUTINE IS BEING USED TO SOLVE SYSTEMS OF LINEAR */
/*          EQUATIONS, E21 SHOULD BE SET TO 1.0D0 IF THE COEFFICIENT */
/*          MATRIX IS SYMMETRIC AND TO -1.0D0 IF NOT. */

/*        M IS THE NUMBER OF SPECIFIED EIGENVALUES OR THE NUMBER OF */
/*          SYSTEMS OF LINEAR EQUATIONS. */

/*        W CONTAINS THE M EIGENVALUES IN ASCENDING OR DESCENDING ORDER. 
*/
/*          IF THE SUBROUTINE IS BEING USED TO SOLVE SYSTEMS OF LINEAR */
/*          EQUATIONS (A-W(R)*I)*X(R)=B(R), WHERE I IS THE IDENTITY */
/*          MATRIX, W(R) SHOULD BE SET ACCORDINGLY, FOR R=1,2,...,M. */

/*        Z CONTAINS THE CONSTANT MATRIX COLUMNS (B(R),R=1,2,...,M), IF */
/*          THE SUBROUTINE IS USED TO SOLVE SYSTEMS OF LINEAR EQUATIONS. 
*/

/*        NV MUST BE SET TO THE DIMENSION OF THE ARRAY PARAMETER RV */
/*          AS DECLARED IN THE CALLING PROGRAM DIMENSION STATEMENT. */

/*     ON OUTPUT */

/*        A AND W ARE UNALTERED. */

/*        Z CONTAINS THE ASSOCIATED SET OF ORTHOGONAL EIGENVECTORS. */
/*          ANY VECTOR WHICH FAILS TO CONVERGE IS SET TO ZERO.  IF THE */
/*          SUBROUTINE IS USED TO SOLVE SYSTEMS OF LINEAR EQUATIONS, */
/*          Z CONTAINS THE SOLUTION MATRIX COLUMNS (X(R),R=1,2,...,M). */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          -R         IF THE EIGENVECTOR CORRESPONDING TO THE R-TH */
/*                     EIGENVALUE FAILS TO CONVERGE, OR IF THE R-TH */
/*                     SYSTEM OF LINEAR EQUATIONS IS NEARLY SINGULAR. */

/*        RV AND RV6 ARE TEMPORARY STORAGE ARRAYS.  NOTE THAT RV IS */
/*          OF DIMENSION AT LEAST N*(2*MB-1).  IF THE SUBROUTINE */
/*          IS BEING USED TO SOLVE SYSTEMS OF LINEAR EQUATIONS, THE */
/*          DETERMINANT (UP TO SIGN) OF A-W(M)*I IS AVAILABLE, UPON */
/*          RETURN, AS THE PRODUCT OF THE FIRST N ELEMENTS OF RV. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --rv6;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --w;
    --rv;

    /* Function Body */
    *ierr = 0;
    if (*m == 0) {
	goto L1001;
    }
    mb = *mbw;
    if (*e21 < 0.) {
	mb = (*mbw + 1) / 2;
    }
    m1 = mb - 1;
    m21 = m1 + mb;
    order = 1. - abs(*e21);
/*     .......... FIND VECTORS BY INVERSE ITERATION .......... */
    i__1 = *m;
    for (r__ = 1; r__ <= i__1; ++r__) {
	its = 1;
	x1 = w[r__];
	if (r__ != 1) {
	    goto L100;
	}
/*     .......... COMPUTE NORM OF MATRIX .......... */
	norm = 0.;

	i__2 = mb;
	for (j = 1; j <= i__2; ++j) {
	    jj = mb + 1 - j;
	    kj = jj + m1;
	    ij = 1;
	    v = 0.;

	    i__3 = *n;
	    for (i__ = jj; i__ <= i__3; ++i__) {
		v += (d__1 = a[i__ + j * a_dim1], abs(d__1));
		if (*e21 >= 0.) {
		    goto L40;
		}
		v += (d__1 = a[ij + kj * a_dim1], abs(d__1));
		++ij;
L40:
		;
	    }

	    norm = max(norm,v);
/* L60: */
	}

	if (*e21 < 0.) {
	    norm *= .5;
	}
/*     .......... EPS2 IS THE CRITERION FOR GROUPING, */
/*                EPS3 REPLACES ZERO PIVOTS AND EQUAL */
/*                ROOTS ARE MODIFIED BY EPS3, */
/*                EPS4 IS TAKEN VERY SMALL TO AVOID OVERFLOW .........
. */
	if (norm == 0.) {
	    norm = 1.;
	}
	eps2 = norm * .001 * abs(order);
	eps3 = epslon_(&norm);
	uk = (doublereal) (*n);
	uk = sqrt(uk);
	eps4 = uk * eps3;
L80:
	group = 0;
	goto L120;
/*     .......... LOOK FOR CLOSE OR COINCIDENT ROOTS .......... */
L100:
	if ((d__1 = x1 - x0, abs(d__1)) >= eps2) {
	    goto L80;
	}
	++group;
	if (order * (x1 - x0) <= 0.) {
	    x1 = x0 + order * eps3;
	}
/*     .......... EXPAND MATRIX, SUBTRACT EIGENVALUE, */
/*                AND INITIALIZE VECTOR .......... */
L120:
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* Computing MIN */
	    i__3 = 0, i__4 = i__ - m1;
	    ij = i__ + min(i__3,i__4) * *n;
	    kj = ij + mb * *n;
	    ij1 = kj + m1 * *n;
	    if (m1 == 0) {
		goto L180;
	    }

	    i__3 = m1;
	    for (j = 1; j <= i__3; ++j) {
		if (ij > m1) {
		    goto L125;
		}
		if (ij > 0) {
		    goto L130;
		}
		rv[ij1] = 0.;
		ij1 += *n;
		goto L130;
L125:
		rv[ij] = a[i__ + j * a_dim1];
L130:
		ij += *n;
		ii = i__ + j;
		if (ii > *n) {
		    goto L150;
		}
		jj = mb - j;
		if (*e21 >= 0.) {
		    goto L140;
		}
		ii = i__;
		jj = mb + j;
L140:
		rv[kj] = a[ii + jj * a_dim1];
		kj += *n;
L150:
		;
	    }

L180:
	    rv[ij] = a[i__ + mb * a_dim1] - x1;
	    rv6[i__] = eps4;
	    if (order == 0.) {
		rv6[i__] = z__[i__ + r__ * z_dim1];
	    }
/* L200: */
	}

	if (m1 == 0) {
	    goto L600;
	}
/*     .......... ELIMINATION WITH INTERCHANGES .......... */
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    ii = i__ + 1;
/* Computing MIN */
	    i__3 = i__ + m1 - 1;
	    maxk = min(i__3,*n);
/* Computing MIN */
	    i__3 = *n - i__, i__4 = m21 - 2;
	    maxj = min(i__3,i__4) * *n;

	    i__3 = maxk;
	    for (k = i__; k <= i__3; ++k) {
		kj1 = k;
		j = kj1 + *n;
		jj = j + maxj;

		i__4 = jj;
		i__5 = *n;
		for (kj = j; i__5 < 0 ? kj >= i__4 : kj <= i__4; kj += i__5) {
		    rv[kj1] = rv[kj];
		    kj1 = kj;
/* L340: */
		}

		rv[kj1] = 0.;
/* L360: */
	    }

	    if (i__ == *n) {
		goto L580;
	    }
	    u = 0.;
/* Computing MIN */
	    i__3 = i__ + m1;
	    maxk = min(i__3,*n);
/* Computing MIN */
	    i__3 = *n - ii, i__5 = m21 - 2;
	    maxj = min(i__3,i__5) * *n;

	    i__3 = maxk;
	    for (j = i__; j <= i__3; ++j) {
		if ((d__1 = rv[j], abs(d__1)) < abs(u)) {
		    goto L450;
		}
		u = rv[j];
		k = j;
L450:
		;
	    }

	    j = i__ + *n;
	    jj = j + maxj;
	    if (k == i__) {
		goto L520;
	    }
	    kj = k;

	    i__3 = jj;
	    i__5 = *n;
	    for (ij = i__; i__5 < 0 ? ij >= i__3 : ij <= i__3; ij += i__5) {
		v = rv[ij];
		rv[ij] = rv[kj];
		rv[kj] = v;
		kj += *n;
/* L500: */
	    }

	    if (order != 0.) {
		goto L520;
	    }
	    v = rv6[i__];
	    rv6[i__] = rv6[k];
	    rv6[k] = v;
L520:
	    if (u == 0.) {
		goto L580;
	    }

	    i__5 = maxk;
	    for (k = ii; k <= i__5; ++k) {
		v = rv[k] / u;
		kj = k;

		i__3 = jj;
		i__4 = *n;
		for (ij = j; i__4 < 0 ? ij >= i__3 : ij <= i__3; ij += i__4) {
		    kj += *n;
		    rv[kj] -= v * rv[ij];
/* L540: */
		}

		if (order == 0.) {
		    rv6[k] -= v * rv6[i__];
		}
/* L560: */
	    }

L580:
	    ;
	}
/*     .......... BACK SUBSTITUTION */
/*                FOR I=N STEP -1 UNTIL 1 DO -- .......... */
L600:
	i__2 = *n;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = *n + 1 - ii;
	    maxj = min(ii,m21);
	    if (maxj == 1) {
		goto L620;
	    }
	    ij1 = i__;
	    j = ij1 + *n;
	    jj = j + (maxj - 2) * *n;

	    i__5 = jj;
	    i__4 = *n;
	    for (ij = j; i__4 < 0 ? ij >= i__5 : ij <= i__5; ij += i__4) {
		++ij1;
		rv6[i__] -= rv[ij] * rv6[ij1];
/* L610: */
	    }

L620:
	    v = rv[i__];
	    if (abs(v) >= eps3) {
		goto L625;
	    }
/*     .......... SET ERROR -- NEARLY SINGULAR LINEAR SYSTEM .....
..... */
	    if (order == 0.) {
		*ierr = -r__;
	    }
	    v = d_sign(&eps3, &v);
L625:
	    rv6[i__] /= v;
/* L630: */
	}

	xu = 1.;
	if (order == 0.) {
	    goto L870;
	}
/*     .......... ORTHOGONALIZE WITH RESPECT TO PREVIOUS */
/*                MEMBERS OF GROUP .......... */
	if (group == 0) {
	    goto L700;
	}

	i__2 = group;
	for (jj = 1; jj <= i__2; ++jj) {
	    j = r__ - group - 1 + jj;
	    xu = 0.;

	    i__4 = *n;
	    for (i__ = 1; i__ <= i__4; ++i__) {
/* L640: */
		xu += rv6[i__] * z__[i__ + j * z_dim1];
	    }

	    i__4 = *n;
	    for (i__ = 1; i__ <= i__4; ++i__) {
/* L660: */
		rv6[i__] -= xu * z__[i__ + j * z_dim1];
	    }

/* L680: */
	}

L700:
	norm = 0.;

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L720: */
	    norm += (d__1 = rv6[i__], abs(d__1));
	}

	if (norm >= .1) {
	    goto L840;
	}
/*     .......... IN-LINE PROCEDURE FOR CHOOSING */
/*                A NEW STARTING VECTOR .......... */
	if (its >= *n) {
	    goto L830;
	}
	++its;
	xu = eps4 / (uk + 1.);
	rv6[1] = eps4;

	i__2 = *n;
	for (i__ = 2; i__ <= i__2; ++i__) {
/* L760: */
	    rv6[i__] = xu;
	}

	rv6[its] -= eps4 * uk;
	goto L600;
/*     .......... SET ERROR -- NON-CONVERGED EIGENVECTOR .......... */
L830:
	*ierr = -r__;
	xu = 0.;
	goto L870;
/*     .......... NORMALIZE SO THAT SUM OF SQUARES IS */
/*                1 AND EXPAND TO FULL ORDER .......... */
L840:
	u = 0.;

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L860: */
	    u = pythag_(&u, &rv6[i__]);
	}

	xu = 1. / u;

L870:
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L900: */
	    z__[i__ + r__ * z_dim1] = rv6[i__] * xu;
	}

	x0 = x1;
/* L920: */
    }

L1001:
    return 0;
} /* bandv_ */

