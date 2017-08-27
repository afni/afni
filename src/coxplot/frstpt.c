/* frstpt.f -- translated by f2c (version 19961017).
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




/* Subroutine */ int frstpt_(real *x, real *y)
{
    static real xx, yy;
    extern /* Subroutine */ int zzphys_(real *, real *);


/*  Set the first point ("pen up move") */
/* .......................................................................
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

/*  Internal Data for PLOTPAK */

    xx = *x;
    yy = *y;
    zzphys_(&xx, &yy);
    zzzplt_1.xphold = xx;
    zzzplt_1.yphold = yy;
    return 0;
} /* frstpt_ */

