/* setw.f -- translated by f2c (version 19961017).
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




/* Subroutine */ int setw_(real *x1, real *y1, real *x2, real *y2)
{

/*  Set the clipping rectangle (physical coords). */
/* .......................................................................
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

/*  Internal Data for PLOTPAK */

    if (*x1 >= *x2) {
	zzzplt_1.xclbot = zzzplt_1.xpgmin;
	zzzplt_1.xcltop = zzzplt_1.xpgmax;
    } else {
	zzzplt_1.xclbot = dmax(*x1,zzzplt_1.xpgmin);
	zzzplt_1.xcltop = dmin(*x2,zzzplt_1.xpgmax);
    }

    if (*y1 >= *y2) {
	zzzplt_1.yclbot = zzzplt_1.ypgmin;
	zzzplt_1.ycltop = zzzplt_1.ypgmax;
    } else {
	zzzplt_1.yclbot = dmax(*y1,zzzplt_1.ypgmin);
	zzzplt_1.ycltop = dmin(*y2,zzzplt_1.ypgmax);
    }

    return 0;
} /* setw_ */

