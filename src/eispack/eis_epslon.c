/* epslon.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

doublereal epslon_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Local variables */
    doublereal a, b, c__, eps;


/*     ESTIMATE UNIT ROUNDOFF IN QUANTITIES OF SIZE X. */


/*     THIS PROGRAM SHOULD FUNCTION PROPERLY ON ALL SYSTEMS */
/*     SATISFYING THE FOLLOWING TWO ASSUMPTIONS, */
/*        1.  THE BASE USED IN REPRESENTING FLOATING POINT */
/*            NUMBERS IS NOT A POWER OF THREE. */
/*        2.  THE QUANTITY  A  IN STATEMENT 10 IS REPRESENTED TO */
/*            THE ACCURACY USED IN FLOATING POINT VARIABLES */
/*            THAT ARE STORED IN MEMORY. */
/*     THE STATEMENT NUMBER 10 AND THE GO TO 10 ARE INTENDED TO */
/*     FORCE OPTIMIZING COMPILERS TO GENERATE CODE SATISFYING */
/*     ASSUMPTION 2. */
/*     UNDER THESE ASSUMPTIONS, IT SHOULD BE TRUE THAT, */
/*            A  IS NOT EXACTLY EQUAL TO FOUR-THIRDS, */
/*            B  HAS A ZERO FOR ITS LAST BIT OR DIGIT, */
/*            C  IS NOT EXACTLY EQUAL TO ONE, */
/*            EPS  MEASURES THE SEPARATION OF 1.0 FROM */
/*                 THE NEXT LARGER FLOATING POINT NUMBER. */
/*     THE DEVELOPERS OF EISPACK WOULD APPRECIATE BEING INFORMED */
/*     ABOUT ANY SYSTEMS WHERE THESE ASSUMPTIONS DO NOT HOLD. */

/*     THIS VERSION DATED 4/6/83. */

    a = 1.3333333333333333;
L10:
    b = a - 1.;
    c__ = b + b + b;
    eps = (d__1 = c__ - 1., abs(d__1));
    if (eps == 0.) {
	goto L10;
    }
    ret_val = eps * abs(*x);
    return ret_val;
} /* epslon_ */
