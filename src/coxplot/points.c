/* points.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"




/* Subroutine */ int points_(real *x, real *y, integer *n, integer *ichar, 
	integer *ipen)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int point_(real *, real *), vector_(real *, real *
	    ), frstpt_(real *, real *);


/*  Note that ICHAR is ignored in this version (argument retained for */
/*  compatibility with NCAR). */
/* .......................................................................
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
    if (*ipen != 1) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    point_(&x[i__], &y[i__]);
/* L100: */
	}
    } else {
	frstpt_(&x[1], &y[1]);
	i__1 = *n;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    vector_(&x[i__], &y[i__]);
/* L200: */
	}
    }
    return 0;
} /* points_ */

