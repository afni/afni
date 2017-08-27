/* setdsh.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Common Block Declarations */

extern struct {
    integer ndash;
    real xldash[8], xid;
} zzdash_;

#define zzdash_1 zzdash_




/* Subroutine */ int setdsh_(integer *nd, real *xld)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;


/*  Set dash lengths (in internal units) */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    /* Parameter adjustments */
    --xld;

    /* Function Body */
    zzdash_1.ndash = min(*nd,8);
    zzdash_1.xid = 0.f;
    i__1 = zzdash_1.ndash;
    for (i__ = 1; i__ <= i__1; ++i__) {
	zzdash_1.xldash[i__ - 1] = xld[i__];
/* L10: */
    }
    return 0;
} /* setdsh_ */

