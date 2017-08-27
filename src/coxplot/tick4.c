/* tick4.f -- translated by f2c (version 19961017).
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




/* Subroutine */ int tick4_(integer *lmajx, integer *lminx, integer *lmajy, 
	integer *lminy)
{
    static real scale;


/*  Set the tick marks in units of 1/1000 the x-width */
/* .......................................................................
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

/*  Internal Data for PLOTPAK */

    scale = (zzzplt_1.xpgmax - zzzplt_1.xpgmin) * .001f;
    zzzplt_1.tmajx = scale * *lmajx;
    zzzplt_1.tminx = scale * *lminx;
    zzzplt_1.tmajy = scale * *lmajy;
    zzzplt_1.tminy = scale * *lminy;
    return 0;
} /* tick4_ */

