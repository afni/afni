/* phline.f -- translated by f2c (version 19961017).
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




/* Subroutine */ int phline_(real *x1, real *y1, real *x2, real *y2)
{
    /* System generated locals */
    real r__1, r__2;

    /* Local variables */
    static real xleft;
    static integer id;
    static real dx1, dy1, dx2, dy2, dx3, dy3;
    extern /* Subroutine */ int zzphph_(real *, real *, real *, real *);
    static real fac, xyl;


/*  Draw a dashed line between 2 internal coordinate points; */
/*  replaces old PHLINE, which is now renamed ZZPHPH */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

    dx1 = *x1;
    dy1 = *y1;
    dx2 = *x2;
    dy2 = *y2;

    if (zzdash_1.ndash <= 1) {
	zzphph_(&dx1, &dy1, &dx2, &dy2);
	goto L9000;
    }

L100:
/* Computing 2nd power */
    r__1 = dx2 - dx1;
/* Computing 2nd power */
    r__2 = dy2 - dy1;
    xyl = sqrt(r__1 * r__1 + r__2 * r__2);
    if (xyl <= 1e-5f) {
	goto L9000;
    }
    id = (integer) zzdash_1.xid + 1;
    xleft = (id - zzdash_1.xid) * (r__1 = zzdash_1.xldash[id - 1], dabs(r__1))
	    ;
    if (xyl <= xleft) {
	if (zzdash_1.xldash[id - 1] > 0.f) {
	    zzphph_(&dx1, &dy1, &dx2, &dy2);
	}
	zzdash_1.xid += xyl / (r__1 = zzdash_1.xldash[id - 1], dabs(r__1));
	goto L9000;
    } else {
	fac = xleft / xyl;
	dx3 = dx1 + fac * (dx2 - dx1);
	dy3 = dy1 + fac * (dy2 - dy1);
	if (zzdash_1.xldash[id - 1] > 0.f) {
	    zzphph_(&dx1, &dy1, &dx3, &dy3);
	}
	dx1 = dx3;
	dy1 = dy3;
	zzdash_1.xid = (real) (id % zzdash_1.ndash);
	goto L100;
    }

L9000:
    return 0;
} /* phline_ */

