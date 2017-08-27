/* zzlgin.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static real c_b2 = 10.f;




/* Subroutine */ int zzlgin_(real *xt, real *pwrten, integer *nlog)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    double r_lg10(real *), pow_ri(real *, integer *);

    /* Local variables */
    static integer nl;
    static real xl;


/*  Return PWRTEN and NTEN such that */

/*   PWRTEN .LE. XT .LT. 10*PWRTEN      AND    PWRTEN = 10**NLOG */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    xl = r_lg10(xt) + 1e-5f;
/* Computing MAX */
    i__1 = (integer) xl;
    nl = max(i__1,-36);
    if (xl < 0.f) {
	--nl;
    }
    *pwrten = pow_ri(&c_b2, &nl);
    *nlog = nl;
    return 0;
} /* zzlgin_ */

