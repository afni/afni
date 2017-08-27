/* labmod.f -- translated by f2c (version 19961017).
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




/* Subroutine */ int labmod_(integer *ifmtx, integer *ifmty, integer *numx, 
	integer *numy, integer *jsizx, integer *jsizy, integer *ixdec, 
	integer *iydec, integer *ixor)
{

/*  Modify the labels for the axes.  Note that only the JSIZX and JSIZY */
/*  arguments are used in this call.  The other arguments are retained */
/*  for compatibility with NCAR. */
/* .......................................................................
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

/*  Internal Data for PLOTPAK */

    zzzplt_1.isizx = *jsizx;
    if (zzzplt_1.isizx <= 0) {
	zzzplt_1.isizx = 8;
    } else if (zzzplt_1.isizx == 1) {
	zzzplt_1.isizx = 12;
    } else if (zzzplt_1.isizx == 2) {
	zzzplt_1.isizx = 16;
    } else if (zzzplt_1.isizx == 3) {
	zzzplt_1.isizx = 24;
    }

    zzzplt_1.isizy = *jsizy;
    if (zzzplt_1.isizy <= 0) {
	zzzplt_1.isizy = 8;
    } else if (zzzplt_1.isizy == 1) {
	zzzplt_1.isizy = 12;
    } else if (zzzplt_1.isizy == 2) {
	zzzplt_1.isizy = 16;
    } else if (zzzplt_1.isizy == 3) {
	zzzplt_1.isizy = 24;
    }

    return 0;
} /* labmod_ */

