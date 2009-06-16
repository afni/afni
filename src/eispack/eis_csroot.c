/* csroot.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int csroot_(doublereal *xr, doublereal *xi, doublereal *yr, 
	doublereal *yi)
{
    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal s, ti, tr;
    extern doublereal pythag_(doublereal *, doublereal *);


/*     (YR,YI) = COMPLEX DSQRT(XR,XI) */
/*     BRANCH CHOSEN SO THAT YR .GE. 0.0 AND SIGN(YI) .EQ. SIGN(XI) */

    tr = *xr;
    ti = *xi;
    s = sqrt((pythag_(&tr, &ti) + abs(tr)) * .5);
    if (tr >= 0.) {
	*yr = s;
    }
    if (ti < 0.) {
	s = -s;
    }
    if (tr <= 0.) {
	*yi = s;
    }
    if (tr < 0.) {
	*yr = ti / *yi * .5;
    }
    if (tr > 0.) {
	*yi = ti / *yr * .5;
    }
    return 0;
} /* csroot_ */

