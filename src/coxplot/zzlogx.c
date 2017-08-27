/* zzlogx.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"




/* Subroutine */ int zzlogx_(real *x1, real *x2, real *y, integer *ndec, real 
	*tmaj, real *tmin)
{
    /* Initialized data */

    static real tmlog[8] = { .30103f,.47712f,.60206f,.69897f,.77815f,.84509f,
	    .90309f,.95424f };

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer idec, imin;
    static real dxmaj, xmajr, ymajr, yminr, xx;
    extern /* Subroutine */ int zzline_(real *, real *, real *, real *);


/*  Draw a log axis from (X1,Y) to (X2,Y)  [physical coordinates], */
/*  with NDEC decades and the major and minor tic mark lengths in */
/*  TMAJ and TMIN, respectively. */

/*  This array is LOG10 of 2,3,...,9;  it is used to space the */
/*  minor divisions. */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    zzline_(x1, y, x2, y);
    if (*tmaj == 0.f || *tmin == 0.f) {
	goto L8000;
    }
    ymajr = *y + *tmaj;
    yminr = *y + *tmin;
    dxmaj = (*x2 - *x1) / *ndec;
    xmajr = *x1;
    zzline_(&xmajr, y, &xmajr, &ymajr);

    i__1 = *ndec;
    for (idec = 1; idec <= i__1; ++idec) {
	if (*tmin != 0.f) {
	    for (imin = 1; imin <= 8; ++imin) {
		xx = xmajr + tmlog[imin - 1] * dxmaj;
		zzline_(&xx, y, &xx, &yminr);
/* L100: */
	    }
	}
	xmajr += dxmaj;
	zzline_(&xmajr, y, &xmajr, &ymajr);
/* L200: */
    }

L8000:
    return 0;
} /* zzlogx_ */

