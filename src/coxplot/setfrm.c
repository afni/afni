/* setfrm.f -- translated by f2c (version 19961017).
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

static integer c__1 = 1;




/* Subroutine */ int setfrm_(real *xobj1, real *xobj2, real *yobj1, real *
	yobj2)
{
    extern /* Subroutine */ int setlin_(integer *);


/*  This routine has no analog in NCAR.  It is called once before all */
/*  plots to set various parameters that define a "frame". */

/*  The entries are the minimum and maximum "physical" x and y values */
/*  allowed.  In NCAR they would always be 0,1,0,1.  Here they can be */
/*  anything to allow for various output device aspect ratios.  However, 
*/
/*  for plots to look reasonable, 1 unit in the x-direction should be the 
*/
/*  same physical size as 1 unit in the y-direction. */
/* .......................................................................
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
/*  Set size of physical page. */


/*  Internal Data for PLOTPAK */

    zzzplt_1.xpgmin = *xobj1;
    zzzplt_1.xpgmax = *xobj2;
    zzzplt_1.ypgmin = *yobj1;
    zzzplt_1.ypgmax = *yobj2;

/*  Initialize the clipping region in case SETW is never called. */

    zzzplt_1.xclbot = zzzplt_1.xpgmin;
    zzzplt_1.xcltop = zzzplt_1.xpgmax;
    zzzplt_1.yclbot = zzzplt_1.ypgmin;
    zzzplt_1.ycltop = zzzplt_1.ypgmax;

/*  Initialize various parameters in case SET is never called. */
/*  Physical coordinate range: */

    zzzplt_1.xbot = zzzplt_1.xpgmin;
    zzzplt_1.xtop = zzzplt_1.xpgmax;
    zzzplt_1.ybot = zzzplt_1.ypgmin;
    zzzplt_1.ytop = zzzplt_1.ypgmax;

/*  User coordinate range: */

    zzzplt_1.xmin = zzzplt_1.xpgmin;
    zzzplt_1.xmax = zzzplt_1.xpgmax;
    zzzplt_1.ymin = zzzplt_1.ypgmin;
    zzzplt_1.ymax = zzzplt_1.ypgmax;

/*  Last plotting location (lower left of page): */

    zzzplt_1.xphold = zzzplt_1.xpgmin;
    zzzplt_1.yphold = zzzplt_1.ypgmin;

/*  Axis types (linear): */

    zzzplt_1.ixcoor = 1;
    zzzplt_1.iycoor = 1;

/*  Axis scalings from user to physical coordinates: */

    zzzplt_1.alphxx = 1.f;
    zzzplt_1.alphyy = 1.f;
    zzzplt_1.betaxx = 0.f;
    zzzplt_1.betayy = 0.f;

/*  Grid parameters: */

    zzzplt_1.tmajx = (zzzplt_1.xpgmax - zzzplt_1.xpgmin) * .01f;
    zzzplt_1.tminx = zzzplt_1.tmajx * .6f;
    zzzplt_1.tmajy = zzzplt_1.tmajx;
    zzzplt_1.tminy = zzzplt_1.tminx;

    zzzplt_1.majrx = 5;
    zzzplt_1.minrx = 10;
    zzzplt_1.majry = 5;
    zzzplt_1.minry = 10;
    zzzplt_1.isizx = 11;
    zzzplt_1.isizy = 11;

/*  Dashed line type (solid) */

    setlin_(&c__1);

    return 0;
} /* setfrm_ */

