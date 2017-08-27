/* zzliny.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"




/* Subroutine */ int zzliny_(real *x, real *y1, real *y2, integer *majry, 
	real *tmaj, integer *minry, real *tmin)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer imaj, imin;
    static real xmajr, xminr, dy, yy;
    extern /* Subroutine */ int zzline_(real *, real *, real *, real *);


/*  Draw a linear axis from (X,Y1) to (X,Y2)  [physical coordinates], */
/*  with MAJRY major divisions (tic mark size = TMAJ) and MINRY minor */
/*  divisions (tic mark size = TMIN). */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    zzline_(x, y1, x, y2);
    if (*tmaj == 0.f && *tmin == 0.f) {
	goto L8000;
    }

    xmajr = *x + *tmaj;
    xminr = *x + *tmin;
    dy = (*y2 - *y1) / (*majry * *minry);
    yy = *y1;
    zzline_(x, &yy, &xmajr, &yy);
    i__1 = *majry;
    for (imaj = 1; imaj <= i__1; ++imaj) {
	i__2 = *minry - 1;
	for (imin = 1; imin <= i__2; ++imin) {
	    yy += dy;
	    if (*tmin != 0.f) {
		zzline_(x, &yy, &xminr, &yy);
	    }
/* L100: */
	}
	yy += dy;
	zzline_(x, &yy, &xmajr, &yy);
/* L200: */
    }

L8000:
    return 0;
} /* zzliny_ */

