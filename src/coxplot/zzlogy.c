/* zzlogy.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"




/* Subroutine */ int zzlogy_(real *x, real *y1, real *y2, integer *ndec, real 
	*tmaj, real *tmin)
{
    /* Initialized data */

    static real tmlog[8] = { .30103f,.47712f,.60206f,.69897f,.77815f,.84509f,
	    .90309f,.95424f };

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer idec, imin;
    static real dymaj, xmajr, ymajr, xminr, yy;
    extern /* Subroutine */ int zzline_(real *, real *, real *, real *);


/*  Draw a log axis from (X,Y1) to (X,Y2)  [physical coordinates], */
/*  with NDEC decades and the major and minor tic mark lengths in */
/*  TMAJ and TMIN, respectively. */

/*  This array is LOG10 of 2,3,...,9;  it is used to space the */
/*  minor divisions. */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    zzline_(x, y1, x, y2);
    if (*tmaj == 0.f || *tmin == 0.f) {
	goto L8000;
    }
    xmajr = *x + *tmaj;
    xminr = *x + *tmin;
    dymaj = (*y2 - *y1) / *ndec;
    ymajr = *y1;
    zzline_(x, &ymajr, &xmajr, &ymajr);

    i__1 = *ndec;
    for (idec = 1; idec <= i__1; ++idec) {
	if (*tmin != 0.f) {
	    for (imin = 1; imin <= 8; ++imin) {
		yy = ymajr + tmlog[imin - 1] * dymaj;
		zzline_(x, &yy, &xminr, &yy);
/* L100: */
	    }
	}
	ymajr += dymaj;
	zzline_(x, &ymajr, &xmajr, &ymajr);
/* L200: */
    }

L8000:
    return 0;
} /* zzlogy_ */

