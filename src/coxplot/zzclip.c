/* zzclip.f -- translated by f2c (version 19961017).
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




/* Subroutine */ int zzclip_(real *x1in, real *y1in, real *x2in, real *y2in)
{
    static real temp, slope, x1, x2, y1, y2, dx, dy;
    static logical linter;


/*  Clip the input line to the predefined plotting region. */

/*  INPUTS */
/*  ------ */
/*  X1IN   = start X-coordinate of line  (physical coordinates) */
/*  Y1IN   = start Y-coordinate of line */
/*  X2IN   = end X-coordinate of line */
/*  Y2IN   = end Y-coordinate of line */

/*  OUTPUTS */
/*  ------- */
/*  same as above but clipped so that the line fits into the plotting */
/*  region defined by calling SETW. */

/*  If the line falls entirely outside of the plotting region, then */
/*  X1IN is returned as -1.E+38. */
/* .......................................................................
 */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
/*  Check to see if line can be thrown out as being totally out of */
/*  the plotting region. */


/*  Internal Data for PLOTPAK */

    if (dmax(*x1in,*x2in) < zzzplt_1.xclbot || dmin(*x1in,*x2in) > 
	    zzzplt_1.xcltop || dmax(*y1in,*y2in) < zzzplt_1.yclbot || dmin(*
	    y1in,*y2in) > zzzplt_1.ycltop) {

	*x1in = -1e38f;
	goto L8000;
    }

/*  Copy input coordinates to local variables, making sure */
/*  that X1 .LE. X2 by interchanging the points if necessary. */

    linter = *x1in > *x2in;
    if (linter) {
	x1 = *x2in;
	x2 = *x1in;
	y1 = *y2in;
	y2 = *y1in;
    } else {
	x1 = *x1in;
	x2 = *x2in;
	y1 = *y1in;
	y2 = *y2in;
    }

/*  Clip line in X direction */

    dx = x2 - x1;
    if (dx > 0.f) {
/*  only clip if line has some X range */
	slope = (y2 - y1) / dx;
	if (x1 < zzzplt_1.xclbot) {
/*  intercept of line at left side */
	    y1 += slope * (zzzplt_1.xclbot - x1);
	    x1 = zzzplt_1.xclbot;
	}
	if (x2 > zzzplt_1.xcltop) {
/*  intercept at right */
	    y2 += slope * (zzzplt_1.xcltop - x2);
	    x2 = zzzplt_1.xcltop;
	}
    }

/*  Check line again to see if it falls outside of plot region. */

    if (dmax(y1,y2) < zzzplt_1.yclbot || dmin(y1,y2) > zzzplt_1.ycltop) {
	*x1in = -1e38f;
	goto L8000;
    }

/*  Clip Y-direction.  To do this, must have Y1 .LE. Y2 */

    if (y1 > y2) {
	temp = x1;
	x1 = x2;
	x2 = temp;
	temp = y1;
	y1 = y2;
	y2 = temp;

	linter = ! linter;
    }

    dy = y2 - y1;
    if (dy > 0.f) {
/*  only clip if line has some Y range */
	slope = (x2 - x1) / dy;
	if (y1 < zzzplt_1.yclbot) {
/*  intercept of line at bottom */
	    x1 += slope * (zzzplt_1.yclbot - y1);
	    y1 = zzzplt_1.yclbot;
	}
	if (y2 > zzzplt_1.ycltop) {
/*  intercept at top */
	    x2 += slope * (zzzplt_1.ycltop - y2);
	    y2 = zzzplt_1.ycltop;
	}
    }

/*  Line is now guaranteed to be totally inside the plot region. */
/*  Copy local clipped coordinates to output values and return. */
/*  Note that we must restore points to original input order. */

    if (linter) {
	*x1in = x2;
	*x2in = x1;
	*y1in = y2;
	*y2in = y1;
    } else {
	*x1in = x1;
	*y1in = y1;
	*x2in = x2;
	*y2in = y2;
    }

L8000:
    return 0;
} /* zzclip_ */

