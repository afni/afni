/* curve.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;

/* ======================================================================= */
/*  Device independent plotting routines: */



/* Subroutine */ int curve_(real *x, real *y, integer *n)
{
    extern /* Subroutine */ int points_(real *, real *, integer *, integer *, 
	    integer *);


/*  Connect N points with N-1 straight lines */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
    points_(&x[1], &y[1], n, &c__0, &c__1);
    return 0;
} /* curve_ */

