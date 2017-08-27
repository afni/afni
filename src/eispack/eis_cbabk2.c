/* cbabk2.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int cbabk2_(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *scale, integer *m, doublereal *zr, doublereal *zi)
{
    /* System generated locals */
    integer zr_dim1, zr_offset, zi_dim1, zi_offset, i__1, i__2;

    /* Local variables */
    integer i__, j, k;
    doublereal s;
    integer ii;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE */
/*     CBABK2, WHICH IS A COMPLEX VERSION OF BALBAK, */
/*     NUM. MATH. 13, 293-304(1969) BY PARLETT AND REINSCH. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971). */

/*     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A COMPLEX GENERAL */
/*     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING */
/*     BALANCED MATRIX DETERMINED BY  CBAL. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY  CBAL. */

/*        SCALE CONTAINS INFORMATION DETERMINING THE PERMUTATIONS */
/*          AND SCALING FACTORS USED BY  CBAL. */

/*        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED. */

/*        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE EIGENVECTORS TO BE */
/*          BACK TRANSFORMED IN THEIR FIRST M COLUMNS. */

/*     ON OUTPUT */

/*        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE TRANSFORMED EIGENVECTORS */
/*          IN THEIR FIRST M COLUMNS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --scale;
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }
    if (*igh == *low) {
	goto L120;
    }

    i__1 = *igh;
    for (i__ = *low; i__ <= i__1; ++i__) {
	s = scale[i__];
/*     .......... LEFT HAND EIGENVECTORS ARE BACK TRANSFORMED */
/*                IF THE FOREGOING STATEMENT IS REPLACED BY */
/*                S=1.0D0/SCALE(I). .......... */
	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    zr[i__ + j * zr_dim1] *= s;
	    zi[i__ + j * zi_dim1] *= s;
/* L100: */
	}

/* L110: */
    }
/*     .......... FOR I=LOW-1 STEP -1 UNTIL 1, */
/*                IGH+1 STEP 1 UNTIL N DO -- .......... */
L120:
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = ii;
	if (i__ >= *low && i__ <= *igh) {
	    goto L140;
	}
	if (i__ < *low) {
	    i__ = *low - ii;
	}
	k = (integer) scale[i__];
	if (k == i__) {
	    goto L140;
	}

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    s = zr[i__ + j * zr_dim1];
	    zr[i__ + j * zr_dim1] = zr[k + j * zr_dim1];
	    zr[k + j * zr_dim1] = s;
	    s = zi[i__ + j * zi_dim1];
	    zi[i__ + j * zi_dim1] = zi[k + j * zi_dim1];
	    zi[k + j * zi_dim1] = s;
/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* cbabk2_ */

