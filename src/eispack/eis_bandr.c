/* bandr.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int bandr_(integer *nm, integer *n, integer *mb, doublereal *
	a, doublereal *d__, doublereal *e, doublereal *e2, logical *matz, 
	doublereal *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3, i__4, i__5, 
	    i__6;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal dmin__;
    integer maxl, maxr;
    doublereal g;
    integer j, k, l, r__;
    doublereal u, b1, b2, c2, f1, f2;
    integer i1, i2, j1, j2, m1, n2, r1;
    doublereal s2;
    integer kr, mr;
    doublereal dminrt;
    integer ugl;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE BANDRD, */
/*     NUM. MATH. 12, 231-241(1968) BY SCHWARZ. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 273-283(1971). */

/*     THIS SUBROUTINE REDUCES A REAL SYMMETRIC BAND MATRIX */
/*     TO A SYMMETRIC TRIDIAGONAL MATRIX USING AND OPTIONALLY */
/*     ACCUMULATING ORTHOGONAL SIMILARITY TRANSFORMATIONS. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        MB IS THE (HALF) BAND WIDTH OF THE MATRIX, DEFINED AS THE */
/*          NUMBER OF ADJACENT DIAGONALS, INCLUDING THE PRINCIPAL */
/*          DIAGONAL, REQUIRED TO SPECIFY THE NON-ZERO PORTION OF THE */
/*          LOWER TRIANGLE OF THE MATRIX. */

/*        A CONTAINS THE LOWER TRIANGLE OF THE SYMMETRIC BAND INPUT */
/*          MATRIX STORED AS AN N BY MB ARRAY.  ITS LOWEST SUBDIAGONAL */
/*          IS STORED IN THE LAST N+1-MB POSITIONS OF THE FIRST COLUMN, */
/*          ITS NEXT SUBDIAGONAL IN THE LAST N+2-MB POSITIONS OF THE */
/*          SECOND COLUMN, FURTHER SUBDIAGONALS SIMILARLY, AND FINALLY */
/*          ITS PRINCIPAL DIAGONAL IN THE N POSITIONS OF THE LAST COLUMN. 
*/
/*          CONTENTS OF STORAGES NOT PART OF THE MATRIX ARE ARBITRARY. */

/*        MATZ SHOULD BE SET TO .TRUE. IF THE TRANSFORMATION MATRIX IS */
/*          TO BE ACCUMULATED, AND TO .FALSE. OTHERWISE. */

/*     ON OUTPUT */

/*        A HAS BEEN DESTROYED, EXCEPT FOR ITS LAST TWO COLUMNS WHICH */
/*          CONTAIN A COPY OF THE TRIDIAGONAL MATRIX. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL */
/*          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO. */

/*        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E. */
/*          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED. */

/*        Z CONTAINS THE ORTHOGONAL TRANSFORMATION MATRIX PRODUCED IN */
/*          THE REDUCTION IF MATZ HAS BEEN SET TO .TRUE.  OTHERWISE, Z */
/*          IS NOT REFERENCED. */

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
    --e2;
    --e;
    --d__;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    dmin__ = 5.4210108624275222e-20;
    dminrt = 2.3283064365386963e-10;
/*     .......... INITIALIZE DIAGONAL SCALING MATRIX .......... */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/* L30: */
	d__[j] = 1.;
    }

    if (! (*matz)) {
	goto L60;
    }

    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (k = 1; k <= i__2; ++k) {
/* L40: */
	    z__[j + k * z_dim1] = 0.;
	}

	z__[j + j * z_dim1] = 1.;
/* L50: */
    }

L60:
    m1 = *mb - 1;
    if ((i__1 = m1 - 1) < 0) {
	goto L900;
    } else if (i__1 == 0) {
	goto L800;
    } else {
	goto L70;
    }
L70:
    n2 = *n - 2;

    i__1 = n2;
    for (k = 1; k <= i__1; ++k) {
/* Computing MIN */
	i__2 = m1, i__3 = *n - k;
	maxr = min(i__2,i__3);
/*     .......... FOR R=MAXR STEP -1 UNTIL 2 DO -- .......... */
	i__2 = maxr;
	for (r1 = 2; r1 <= i__2; ++r1) {
	    r__ = maxr + 2 - r1;
	    kr = k + r__;
	    mr = *mb - r__;
	    g = a[kr + mr * a_dim1];
	    a[kr - 1 + a_dim1] = a[kr - 1 + (mr + 1) * a_dim1];
	    ugl = k;

	    i__3 = *n;
	    i__4 = m1;
	    for (j = kr; i__4 < 0 ? j >= i__3 : j <= i__3; j += i__4) {
		j1 = j - 1;
		j2 = j1 - 1;
		if (g == 0.) {
		    goto L600;
		}
		b1 = a[j1 + a_dim1] / g;
		b2 = b1 * d__[j1] / d__[j];
		s2 = 1. / (b1 * b2 + 1.);
		if (s2 >= .5) {
		    goto L450;
		}
		b1 = g / a[j1 + a_dim1];
		b2 = b1 * d__[j] / d__[j1];
		c2 = 1. - s2;
		d__[j1] = c2 * d__[j1];
		d__[j] = c2 * d__[j];
		f1 = a[j + m1 * a_dim1] * 2.;
		f2 = b1 * a[j1 + *mb * a_dim1];
		a[j + m1 * a_dim1] = -b2 * (b1 * a[j + m1 * a_dim1] - a[j + *
			mb * a_dim1]) - f2 + a[j + m1 * a_dim1];
		a[j1 + *mb * a_dim1] = b2 * (b2 * a[j + *mb * a_dim1] + f1) + 
			a[j1 + *mb * a_dim1];
		a[j + *mb * a_dim1] = b1 * (f2 - f1) + a[j + *mb * a_dim1];

		i__5 = j2;
		for (l = ugl; l <= i__5; ++l) {
		    i2 = *mb - j + l;
		    u = a[j1 + (i2 + 1) * a_dim1] + b2 * a[j + i2 * a_dim1];
		    a[j + i2 * a_dim1] = -b1 * a[j1 + (i2 + 1) * a_dim1] + a[
			    j + i2 * a_dim1];
		    a[j1 + (i2 + 1) * a_dim1] = u;
/* L200: */
		}

		ugl = j;
		a[j1 + a_dim1] += b2 * g;
		if (j == *n) {
		    goto L350;
		}
/* Computing MIN */
		i__5 = m1, i__6 = *n - j1;
		maxl = min(i__5,i__6);

		i__5 = maxl;
		for (l = 2; l <= i__5; ++l) {
		    i1 = j1 + l;
		    i2 = *mb - l;
		    u = a[i1 + i2 * a_dim1] + b2 * a[i1 + (i2 + 1) * a_dim1];
		    a[i1 + (i2 + 1) * a_dim1] = -b1 * a[i1 + i2 * a_dim1] + a[
			    i1 + (i2 + 1) * a_dim1];
		    a[i1 + i2 * a_dim1] = u;
/* L300: */
		}

		i1 = j + m1;
		if (i1 > *n) {
		    goto L350;
		}
		g = b2 * a[i1 + a_dim1];
L350:
		if (! (*matz)) {
		    goto L500;
		}

		i__5 = *n;
		for (l = 1; l <= i__5; ++l) {
		    u = z__[l + j1 * z_dim1] + b2 * z__[l + j * z_dim1];
		    z__[l + j * z_dim1] = -b1 * z__[l + j1 * z_dim1] + z__[l 
			    + j * z_dim1];
		    z__[l + j1 * z_dim1] = u;
/* L400: */
		}

		goto L500;

L450:
		u = d__[j1];
		d__[j1] = s2 * d__[j];
		d__[j] = s2 * u;
		f1 = a[j + m1 * a_dim1] * 2.;
		f2 = b1 * a[j + *mb * a_dim1];
		u = b1 * (f2 - f1) + a[j1 + *mb * a_dim1];
		a[j + m1 * a_dim1] = b2 * (b1 * a[j + m1 * a_dim1] - a[j1 + *
			mb * a_dim1]) + f2 - a[j + m1 * a_dim1];
		a[j1 + *mb * a_dim1] = b2 * (b2 * a[j1 + *mb * a_dim1] + f1) 
			+ a[j + *mb * a_dim1];
		a[j + *mb * a_dim1] = u;

		i__5 = j2;
		for (l = ugl; l <= i__5; ++l) {
		    i2 = *mb - j + l;
		    u = b2 * a[j1 + (i2 + 1) * a_dim1] + a[j + i2 * a_dim1];
		    a[j + i2 * a_dim1] = -a[j1 + (i2 + 1) * a_dim1] + b1 * a[
			    j + i2 * a_dim1];
		    a[j1 + (i2 + 1) * a_dim1] = u;
/* L460: */
		}

		ugl = j;
		a[j1 + a_dim1] = b2 * a[j1 + a_dim1] + g;
		if (j == *n) {
		    goto L480;
		}
/* Computing MIN */
		i__5 = m1, i__6 = *n - j1;
		maxl = min(i__5,i__6);

		i__5 = maxl;
		for (l = 2; l <= i__5; ++l) {
		    i1 = j1 + l;
		    i2 = *mb - l;
		    u = b2 * a[i1 + i2 * a_dim1] + a[i1 + (i2 + 1) * a_dim1];
		    a[i1 + (i2 + 1) * a_dim1] = -a[i1 + i2 * a_dim1] + b1 * a[
			    i1 + (i2 + 1) * a_dim1];
		    a[i1 + i2 * a_dim1] = u;
/* L470: */
		}

		i1 = j + m1;
		if (i1 > *n) {
		    goto L480;
		}
		g = a[i1 + a_dim1];
		a[i1 + a_dim1] = b1 * a[i1 + a_dim1];
L480:
		if (! (*matz)) {
		    goto L500;
		}

		i__5 = *n;
		for (l = 1; l <= i__5; ++l) {
		    u = b2 * z__[l + j1 * z_dim1] + z__[l + j * z_dim1];
		    z__[l + j * z_dim1] = -z__[l + j1 * z_dim1] + b1 * z__[l 
			    + j * z_dim1];
		    z__[l + j1 * z_dim1] = u;
/* L490: */
		}

L500:
		;
	    }

L600:
	    ;
	}

	if (k % 64 != 0) {
	    goto L700;
	}
/*     .......... RESCALE TO AVOID UNDERFLOW OR OVERFLOW .......... */
	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    if (d__[j] >= dmin__) {
		goto L650;
	    }
/* Computing MAX */
	    i__4 = 1, i__3 = *mb + 1 - j;
	    maxl = max(i__4,i__3);

	    i__4 = m1;
	    for (l = maxl; l <= i__4; ++l) {
/* L610: */
		a[j + l * a_dim1] = dminrt * a[j + l * a_dim1];
	    }

	    if (j == *n) {
		goto L630;
	    }
/* Computing MIN */
	    i__4 = m1, i__3 = *n - j;
	    maxl = min(i__4,i__3);

	    i__4 = maxl;
	    for (l = 1; l <= i__4; ++l) {
		i1 = j + l;
		i2 = *mb - l;
		a[i1 + i2 * a_dim1] = dminrt * a[i1 + i2 * a_dim1];
/* L620: */
	    }

L630:
	    if (! (*matz)) {
		goto L645;
	    }

	    i__4 = *n;
	    for (l = 1; l <= i__4; ++l) {
/* L640: */
		z__[l + j * z_dim1] = dminrt * z__[l + j * z_dim1];
	    }

L645:
	    a[j + *mb * a_dim1] = dmin__ * a[j + *mb * a_dim1];
	    d__[j] /= dmin__;
L650:
	    ;
	}

L700:
	;
    }
/*     .......... FORM SQUARE ROOT OF SCALING MATRIX .......... */
L800:
    i__1 = *n;
    for (j = 2; j <= i__1; ++j) {
/* L810: */
	e[j] = sqrt(d__[j]);
    }

    if (! (*matz)) {
	goto L840;
    }

    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (k = 2; k <= i__2; ++k) {
/* L820: */
	    z__[j + k * z_dim1] = e[k] * z__[j + k * z_dim1];
	}

/* L830: */
    }

L840:
    u = 1.;

    i__1 = *n;
    for (j = 2; j <= i__1; ++j) {
	a[j + m1 * a_dim1] = u * e[j] * a[j + m1 * a_dim1];
	u = e[j];
/* Computing 2nd power */
	d__1 = a[j + m1 * a_dim1];
	e2[j] = d__1 * d__1;
	a[j + *mb * a_dim1] = d__[j] * a[j + *mb * a_dim1];
	d__[j] = a[j + *mb * a_dim1];
	e[j] = a[j + m1 * a_dim1];
/* L850: */
    }

    d__[1] = a[*mb * a_dim1 + 1];
    e[1] = 0.;
    e2[1] = 0.;
    goto L1001;

L900:
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	d__[j] = a[j + *mb * a_dim1];
	e[j] = 0.;
	e2[j] = 0.;
/* L950: */
    }

L1001:
    return 0;
} /* bandr_ */

