/* cdiv.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int cdiv_(doublereal *ar, doublereal *ai, doublereal *br, 
	doublereal *bi, doublereal *cr, doublereal *ci)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    doublereal s, ais, bis, ars, brs;


/*     COMPLEX DIVISION, (CR,CI) = (AR,AI)/(BR,BI) */

    s = abs(*br) + abs(*bi);
    ars = *ar / s;
    ais = *ai / s;
    brs = *br / s;
    bis = *bi / s;
/* Computing 2nd power */
    d__1 = brs;
/* Computing 2nd power */
    d__2 = bis;
    s = d__1 * d__1 + d__2 * d__2;
    *cr = (ars * brs + ais * bis) / s;
    *ci = (ais * brs - ars * bis) / s;
    return 0;
} /* cdiv_ */

