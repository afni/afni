/* perim.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

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




/* Subroutine */ int perim_(integer *mbx, integer *mlx, integer *mby, integer 
	*mly)
{
    extern /* Subroutine */ int zzperi_(integer *);


/*  Perimeter along the SET lines -- draw 4 axes -- no labels. */
/* .......................................................................
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

/*  Internal Data for PLOTPAK */

    zzzplt_1.majrx = *mbx;
    zzzplt_1.minrx = *mlx;
    zzzplt_1.majry = *mby;
    zzzplt_1.minry = *mly;

/*  Call perimeter routine with no labels. */

    zzperi_(&c__0);
    return 0;
} /* perim_ */

