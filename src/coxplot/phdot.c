/* phdot.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"




/* Subroutine */ int phdot_(real *x1, real *y1)
{
    extern /* Subroutine */ int zzphph_(real *, real *, real *, real *);


/*  Plot a physical coordinate dot (the cheap way). */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    zzphph_(x1, y1, x1, y1);
    return 0;
} /* phdot_ */

