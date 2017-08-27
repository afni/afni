/* zzphys.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Common Block Declarations */

extern struct {
    real xpgmin, ypgmin, xpgmax, ypgmax, xclbot, yclbot, xcltop, ycltop, xbot,
	     ybot, xtop, ytop, xmin, ymin, xmax, ymax;
    integer ixcoor, iycoor;
    real alphxx, betaxx, alphyy, betayy, tmajx, tminx, tmajy, tminy;
    integer majrx, minrx, majry, minry, isizx, isizy;
    real xphold, yphold;
} zzzplt_;

#define zzzplt_1 zzzplt_

extern struct {
    real xphmax, yphmax;
    integer ixpmax, iypmax;
    real xpscal, ypscal;
    integer iflip, nplotr;
    char cfile[64];
} zzpltr_;

#define zzpltr_1 zzpltr_




/* Subroutine */ int zzphys_(real *x, real *y)
{
    /* System generated locals */
    real r__1;

    /* Builtin functions */
    double r_lg10(real *);


/*  Convert user to physical coordinates. */
/* .......................................................................
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

/*  Internal Data for PLOTPAK */

    if (zzzplt_1.ixcoor < 0) {
	r__1 = dabs(*x) + 1e-37f;
	*x = r_lg10(&r__1);
    }
    *x = zzzplt_1.alphxx * *x + zzzplt_1.betaxx;

    if (zzzplt_1.iycoor < 0) {
	r__1 = dabs(*y) + 1e-37f;
	*y = r_lg10(&r__1);
    }
    *y = zzzplt_1.alphyy * *y + zzzplt_1.betayy;

    return 0;
} /* zzphys_ */

