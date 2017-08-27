/* zzperi.f -- translated by f2c (version 19961017).
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

/* Table of constant values */

static integer c__0 = 0;




/* Subroutine */ int zzperi_(integer *ilab)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer xlab, ylab;
    static real sxmin, sxmax, symin, symax;
    static integer isidex, isidey;
    extern /* Subroutine */ int zzaxxx_(real *, real *, real *, integer *, 
	    integer *), zzaxyy_(real *, real *, real *, integer *, integer *);


/*  Do the perimeter axes. */
/*  ILAB = 0  --> no labels on axes */
/*       = 1  --> labels on x and y */
/*       = 2  --> labels on x only */
/*       = 3  --> labels on y only */
/* .......................................................................
 */

/*  Internal Data for PLOTPAK */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    if (zzzplt_1.xmin < zzzplt_1.xmax) {
	isidey = 1;
	sxmin = zzzplt_1.xmin;
	sxmax = zzzplt_1.xmax;
    } else {
	isidey = -1;
	sxmin = zzzplt_1.xmax;
	sxmax = zzzplt_1.xmin;
    }

    if (zzzplt_1.ymin < zzzplt_1.ymax) {
	isidex = 1;
	symin = zzzplt_1.ymin;
	symax = zzzplt_1.ymax;
    } else {
	isidex = -1;
	symin = zzzplt_1.ymax;
	symax = zzzplt_1.ymin;
    }

    xlab = 0;
    ylab = 0;
    if (*ilab == 1 || *ilab == 2) {
	xlab = 1;
    }
    if (*ilab == 1 || *ilab == 3) {
	ylab = 1;
    }

    zzaxxx_(&sxmin, &sxmax, &symin, &isidex, &xlab);
    i__1 = -isidex;
    zzaxxx_(&sxmin, &sxmax, &symax, &i__1, &c__0);

    zzaxyy_(&sxmin, &symin, &symax, &isidey, &ylab);
    i__1 = -isidey;
    zzaxyy_(&sxmax, &symin, &symax, &i__1, &c__0);

    return 0;
} /* zzperi_ */

