/* zzline.f -- translated by f2c (version 19961017).
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




/* Subroutine */ int zzline_(real *x1, real *y1, real *x2, real *y2)
{
    extern /* Subroutine */ int phline_(real *, real *, real *, real *), 
	    zzclip_(real *, real *, real *, real *);
    static real xx1, xx2, yy1, yy2;


/*  Draw a line between 2 physical coordinates points. */
/* .......................................................................
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

/*  Internal Data for PLOTPAK */

    xx1 = *x1;
    yy1 = *y1;
    xx2 = *x2;
    yy2 = *y2;
    zzclip_(&xx1, &yy1, &xx2, &yy2);
    if (xx1 >= zzzplt_1.xpgmin) {
	phline_(&xx1, &yy1, &xx2, &yy2);
    }
    zzzplt_1.xphold = *x2;
    zzzplt_1.yphold = *y2;

    return 0;
} /* zzline_ */

