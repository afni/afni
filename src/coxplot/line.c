/* line.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"




/* Subroutine */ int line_(real *x1, real *y1, real *x2, real *y2)
{
    extern /* Subroutine */ int zzline_(real *, real *, real *, real *);
    static real xx1, xx2, yy1, yy2;
    extern /* Subroutine */ int zzphys_(real *, real *);


/*  Draw one line between given user coordinates. */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    xx1 = *x1;
    yy1 = *y1;
    zzphys_(&xx1, &yy1);
    xx2 = *x2;
    yy2 = *y2;
    zzphys_(&xx2, &yy2);

    zzline_(&xx1, &yy1, &xx2, &yy2);

    return 0;
} /* line_ */

