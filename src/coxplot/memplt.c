/* memplt.f -- translated by f2c (version 19961017).
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

static real c_b2 = 0.f;




/* Subroutine */ int memplt_(real *aspect)
{
    extern /* Subroutine */ int setfrm_(real *, real *, real *, real *);


/*  Set the plotter to be the "memplot" C routines */
/* .......................................................................
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

/*  Internal Data for PLOTPAK */

    zzpltr_1.xpscal = 1.f;
    zzpltr_1.ypscal = 1.f;
    zzpltr_1.iflip = 0;
    zzpltr_1.nplotr = 7;
    if (*aspect <= 0.f) {
	zzpltr_1.xphmax = 1.3f;
    } else {
	zzpltr_1.xphmax = *aspect;
    }
    zzpltr_1.yphmax = 1.f;
    setfrm_(&c_b2, &zzpltr_1.xphmax, &c_b2, &zzpltr_1.yphmax);

    return 0;
} /* memplt_ */

