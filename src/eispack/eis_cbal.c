/* cbal.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int cbal_(integer *nm, integer *n, doublereal *ar, 
	doublereal *ai, integer *low, integer *igh, doublereal *scale)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2;
    doublereal d__1, d__2;

    /* Local variables */
    integer iexc;
    doublereal c__, f, g;
    integer i__, j, k, l, m;
    doublereal r__, s, radix, b2;
    integer jj;
    logical noconv;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE */
/*     CBALANCE, WHICH IS A COMPLEX VERSION OF BALANCE, */
/*     NUM. MATH. 13, 293-304(1969) BY PARLETT AND REINSCH. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971). */

/*     THIS SUBROUTINE BALANCES A COMPLEX MATRIX AND ISOLATES */
/*     EIGENVALUES WHENEVER POSSIBLE. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE COMPLEX MATRIX TO BE BALANCED. */

/*     ON OUTPUT */

/*        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE BALANCED MATRIX. */

/*        LOW AND IGH ARE TWO INTEGERS SUCH THAT AR(I,J) AND AI(I,J) */
/*          ARE EQUAL TO ZERO IF */
/*           (1) I IS GREATER THAN J AND */
/*           (2) J=1,...,LOW-1 OR I=IGH+1,...,N. */

/*        SCALE CONTAINS INFORMATION DETERMINING THE */
/*           PERMUTATIONS AND SCALING FACTORS USED. */

/*     SUPPOSE THAT THE PRINCIPAL SUBMATRIX IN ROWS LOW THROUGH IGH */
/*     HAS BEEN BALANCED, THAT P(J) DENOTES THE INDEX INTERCHANGED */
/*     WITH J DURING THE PERMUTATION STEP, AND THAT THE ELEMENTS */
/*     OF THE DIAGONAL MATRIX USED ARE DENOTED BY D(I,J).  THEN */
/*        SCALE(J) = P(J),    FOR J = 1,...,LOW-1 */
/*                 = D(J,J)       J = LOW,...,IGH */
/*                 = P(J)         J = IGH+1,...,N. */
/*     THE ORDER IN WHICH THE INTERCHANGES ARE MADE IS N TO IGH+1, */
/*     THEN 1 TO LOW-1. */

/*     NOTE THAT 1 IS RETURNED FOR IGH IF IGH IS ZERO FORMALLY. */

/*     THE ALGOL PROCEDURE EXC CONTAINED IN CBALANCE APPEARS IN */
/*     CBAL  IN LINE.  (NOTE THAT THE ALGOL ROLES OF IDENTIFIERS */
/*     K,L HAVE BEEN REVERSED.) */

/*     ARITHMETIC IS REAL THROUGHOUT. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --scale;
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;

    /* Function Body */
    radix = 16.;

    b2 = radix * radix;
    k = 1;
    l = *n;
    goto L100;
/*     .......... IN-LINE PROCEDURE FOR ROW AND */
/*                COLUMN EXCHANGE .......... */
L20:
    scale[m] = (doublereal) j;
    if (j == m) {
	goto L50;
    }

    i__1 = l;
    for (i__ = 1; i__ <= i__1; ++i__) {
	f = ar[i__ + j * ar_dim1];
	ar[i__ + j * ar_dim1] = ar[i__ + m * ar_dim1];
	ar[i__ + m * ar_dim1] = f;
	f = ai[i__ + j * ai_dim1];
	ai[i__ + j * ai_dim1] = ai[i__ + m * ai_dim1];
	ai[i__ + m * ai_dim1] = f;
/* L30: */
    }

    i__1 = *n;
    for (i__ = k; i__ <= i__1; ++i__) {
	f = ar[j + i__ * ar_dim1];
	ar[j + i__ * ar_dim1] = ar[m + i__ * ar_dim1];
	ar[m + i__ * ar_dim1] = f;
	f = ai[j + i__ * ai_dim1];
	ai[j + i__ * ai_dim1] = ai[m + i__ * ai_dim1];
	ai[m + i__ * ai_dim1] = f;
/* L40: */
    }

L50:
    switch (iexc) {
	case 1:  goto L80;
	case 2:  goto L130;
    }
/*     .......... SEARCH FOR ROWS ISOLATING AN EIGENVALUE */
/*                AND PUSH THEM DOWN .......... */
L80:
    if (l == 1) {
	goto L280;
    }
    --l;
/*     .......... FOR J=L STEP -1 UNTIL 1 DO -- .......... */
L100:
    i__1 = l;
    for (jj = 1; jj <= i__1; ++jj) {
	j = l + 1 - jj;

	i__2 = l;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    if (i__ == j) {
		goto L110;
	    }
	    if (ar[j + i__ * ar_dim1] != 0. || ai[j + i__ * ai_dim1] != 0.) {
		goto L120;
	    }
L110:
	    ;
	}

	m = l;
	iexc = 1;
	goto L20;
L120:
	;
    }

    goto L140;
/*     .......... SEARCH FOR COLUMNS ISOLATING AN EIGENVALUE */
/*                AND PUSH THEM LEFT .......... */
L130:
    ++k;

L140:
    i__1 = l;
    for (j = k; j <= i__1; ++j) {

	i__2 = l;
	for (i__ = k; i__ <= i__2; ++i__) {
	    if (i__ == j) {
		goto L150;
	    }
	    if (ar[i__ + j * ar_dim1] != 0. || ai[i__ + j * ai_dim1] != 0.) {
		goto L170;
	    }
L150:
	    ;
	}

	m = k;
	iexc = 2;
	goto L20;
L170:
	;
    }
/*     .......... NOW BALANCE THE SUBMATRIX IN ROWS K TO L .......... */
    i__1 = l;
    for (i__ = k; i__ <= i__1; ++i__) {
/* L180: */
	scale[i__] = 1.;
    }
/*     .......... ITERATIVE LOOP FOR NORM REDUCTION .......... */
L190:
    noconv = FALSE_;

    i__1 = l;
    for (i__ = k; i__ <= i__1; ++i__) {
	c__ = 0.;
	r__ = 0.;

	i__2 = l;
	for (j = k; j <= i__2; ++j) {
	    if (j == i__) {
		goto L200;
	    }
	    c__ = c__ + (d__1 = ar[j + i__ * ar_dim1], abs(d__1)) + (d__2 = 
		    ai[j + i__ * ai_dim1], abs(d__2));
	    r__ = r__ + (d__1 = ar[i__ + j * ar_dim1], abs(d__1)) + (d__2 = 
		    ai[i__ + j * ai_dim1], abs(d__2));
L200:
	    ;
	}
/*     .......... GUARD AGAINST ZERO C OR R DUE TO UNDERFLOW .........
. */
	if (c__ == 0. || r__ == 0.) {
	    goto L270;
	}
	g = r__ / radix;
	f = 1.;
	s = c__ + r__;
L210:
	if (c__ >= g) {
	    goto L220;
	}
	f *= radix;
	c__ *= b2;
	goto L210;
L220:
	g = r__ * radix;
L230:
	if (c__ < g) {
	    goto L240;
	}
	f /= radix;
	c__ /= b2;
	goto L230;
/*     .......... NOW BALANCE .......... */
L240:
	if ((c__ + r__) / f >= s * .95) {
	    goto L270;
	}
	g = 1. / f;
	scale[i__] *= f;
	noconv = TRUE_;

	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    ar[i__ + j * ar_dim1] *= g;
	    ai[i__ + j * ai_dim1] *= g;
/* L250: */
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    ar[j + i__ * ar_dim1] *= f;
	    ai[j + i__ * ai_dim1] *= f;
/* L260: */
	}

L270:
	;
    }

    if (noconv) {
	goto L190;
    }

L280:
    *low = k;
    *igh = l;
    return 0;
} /* cbal_ */

