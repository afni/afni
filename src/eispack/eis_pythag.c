/* pythag.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

doublereal pythag_(doublereal *a, doublereal *b)
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3;

    /* Local variables */
    doublereal p, r__, s, t, u;


/*     FINDS DSQRT(A**2+B**2) WITHOUT OVERFLOW OR DESTRUCTIVE UNDERFLOW */

/* Computing MAX */
    d__1 = abs(*a), d__2 = abs(*b);
    p = max(d__1,d__2);
    if (p == 0.) {
	goto L20;
    }
/* Computing MIN */
    d__2 = abs(*a), d__3 = abs(*b);
/* Computing 2nd power */
    d__1 = min(d__2,d__3) / p;
    r__ = d__1 * d__1;
L10:
    t = r__ + 4.;
    if (t == 4.) {
	goto L20;
    }
    s = r__ / t;
    u = s * 2. + 1.;
    p = u * p;
/* Computing 2nd power */
    d__1 = s / u;
    r__ = d__1 * d__1 * r__;
    goto L10;
L20:
    ret_val = p;
    return ret_val;
} /* pythag_ */

