/* pwrit.f -- translated by f2c (version 19961017).
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




/* Subroutine */ int pwrit_(real *x, real *y, char *ch, integer *nch, integer 
	*isiz, integer *ior, integer *icent, ftnlen ch_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, nchar;
    static real width;
    static integer isize;
    static real ct, dx, dy, oor, st, xx, yy;
    extern /* Subroutine */ int zzchar_(char *, real *, real *, real *, real *
	    , ftnlen), zzphys_(real *, real *);


/*  Additional options besides NCAR's choices: */

/*    NCH < 0    ==> use absolute coordinates rather than user coords. */
/*                   [this is because the use of integer absolute   ] */
/*                   [coordinates is not implemented in this package] */

/*    ABS(NCH) = 999 ==> find length of string by looking for a 0 byte. */

/*    ICENT = -2 ==> (X,Y) is lower left corner of string to plot. */

/* .......................................................................
 */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
/*  Calculate character width in terms of 1/1000 of the x-width. */


/*  Internal Data for PLOTPAK */

    /* Parameter adjustments */
    --ch;

    /* Function Body */
    isize = *isiz;
    if (isize <= 0) {
	isize = 8;
    } else if (isize == 1) {
	isize = 12;
    } else if (isize == 2) {
	isize = 16;
    } else if (isize == 3) {
	isize = 24;
    }

    width = isize * .001f * (zzzplt_1.xpgmax - zzzplt_1.xpgmin);

/*  Rotation/scaling factors for digitization.  Include factor of 1/6 */
/*  to allow for digitization scale in ZZCHAR. */

    oor = *ior * .017453292f;
    dx = width * cos(oor);
    dy = width * sin(oor);
    ct = dx * .1666667f;
    st = dy * .1666667f;

/*  Starting location for first character. */

    xx = *x;
    yy = *y;
    if (*nch > 0) {
	zzphys_(&xx, &yy);
    }

/*  Get no. of characters in string.  Special option 999 must be checked. 
*/

    nchar = abs(*nch);
    if (nchar == 999) {
	i__1 = nchar;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (*(unsigned char *)&ch[i__] == '\0') {
		goto L20;
	    }
/* L10: */
	}
L20:
	nchar = i__ - 1;
    }

/*  If centering option is not lower-left corner, must calculate */
/*  location of lower left corner. */

    if (*icent != -2) {
/*  Move from center of character down to bottom (aspect ratio = 7/6) 
*/
	xx += dy * .5833333f;
	yy -= dx * .5833333f;
	if (*icent == 0) {
	    xx -= nchar * .5f * dx;
	    yy -= nchar * .5f * dy;
	} else if (*icent == 1) {
	    xx -= nchar * dx;
	    yy -= nchar * dy;
	}
    }
/* .......................................................................
 */
    i__1 = nchar;
    for (i__ = 1; i__ <= i__1; ++i__) {
	zzchar_(ch + i__, &xx, &yy, &ct, &st, 1L);
	xx += dx;
	yy += dy;
/* L100: */
    }
/* .......................................................................
 */
    zzzplt_1.xphold = xx;
    zzzplt_1.yphold = yy;
    return 0;
} /* pwrit_ */

