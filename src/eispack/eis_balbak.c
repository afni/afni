/* balbak.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int balbak_(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *scale, integer *m, doublereal *z__)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2;

    /* Local variables */
    integer i__, j, k;
    doublereal s;
    integer ii;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE BALBAK, */
/*     NUM. MATH. 13, 293-304(1969) BY PARLETT AND REINSCH. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971). */

/*     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A REAL GENERAL */
/*     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING */
/*     BALANCED MATRIX DETERMINED BY  BALANC. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY  BALANC. */

/*        SCALE CONTAINS INFORMATION DETERMINING THE PERMUTATIONS */
/*          AND SCALING FACTORS USED BY  BALANC. */

/*        M IS THE NUMBER OF COLUMNS OF Z TO BE BACK TRANSFORMED. */

/*        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGEN- */
/*          VECTORS TO BE BACK TRANSFORMED IN ITS FIRST M COLUMNS. */

/*     ON OUTPUT */

/*        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE */
/*          TRANSFORMED EIGENVECTORS IN ITS FIRST M COLUMNS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --scale;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;

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
/* L100: */
	    z__[i__ + j * z_dim1] *= s;
	}

/* L110: */
    }
/*     ......... FOR I=LOW-1 STEP -1 UNTIL 1, */
/*               IGH+1 STEP 1 UNTIL N DO -- .......... */
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
	    s = z__[i__ + j * z_dim1];
	    z__[i__ + j * z_dim1] = z__[k + j * z_dim1];
	    z__[k + j * z_dim1] = s;
/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* balbak_ */

