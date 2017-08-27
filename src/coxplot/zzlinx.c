/* zzlinx.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"




/* Subroutine */ int zzlinx_(real *x1, real *x2, real *y, integer *majrx, 
	real *tmaj, integer *minrx, real *tmin)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer imaj, imin;
    static real ymajr, yminr, dx, xx;
    extern /* Subroutine */ int zzline_(real *, real *, real *, real *);


/*  Draw a linear axis from (X1,Y) to (X2,Y)  [physical coordinates], */
/*  with MAJRX major divisions (tic mark size = TMAJ) and MINRX minor */
/*  divisions (tic mark size = TMIN). */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    zzline_(x1, y, x2, y);
    if (*tmaj == 0.f && *tmin == 0.f) {
	goto L8000;
    }

    ymajr = *y + *tmaj;
    yminr = *y + *tmin;
    dx = (*x2 - *x1) / (*majrx * *minrx);
    xx = *x1;
    zzline_(&xx, y, &xx, &ymajr);
    i__1 = *majrx;
    for (imaj = 1; imaj <= i__1; ++imaj) {
	i__2 = *minrx - 1;
	for (imin = 1; imin <= i__2; ++imin) {
	    xx += dx;
	    if (*tmin != 0.f) {
		zzline_(&xx, y, &xx, &yminr);
	    }
/* L100: */
	}
	xx += dx;
	zzline_(&xx, y, &xx, &ymajr);
/* L200: */
    }

L8000:
    return 0;
} /* zzlinx_ */

