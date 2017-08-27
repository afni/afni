/* zzaxxx.f -- translated by f2c (version 19961017).
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
static integer c__0 = 0;

/* ======================================================================= */
/*  Routines that start with ZZ are internal utility routines: */



/* Subroutine */ int zzaxxx_(real *x1, real *x2, real *y, integer *iside, 
	integer *ilab)
{
    /* Format strings */
    static char fmt_101[] = "(\0021.E\002,i2)";
    static char fmt_102[] = "(\0021.E\002,i3)";

    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2;
    static char equiv_0[10];

    /* Builtin functions */
    integer s_wsfi(icilist *), do_fio(integer *, char *, ftnlen), e_wsfi(void)
	    ;

    /* Local variables */
    static integer ndec, nlab;
#define buf10 (equiv_0)
    static real ylab, temp, dxlab;
    static integer nchar, il;
    static real xv, xx, yy;
    extern /* Subroutine */ int zzlgin_(real *, real *, integer *);
    static integer nl1, nl2;
    extern /* Subroutine */ int zzlabl_(real *, char *, integer *, ftnlen);
    static integer npower;
    extern /* Subroutine */ int pwritf_(real *, real *, char *, integer *, 
	    integer *, integer *, integer *, ftnlen), zzlogx_(real *, real *, 
	    real *, integer *, real *, real *), zzlinx_(real *, real *, real *
	    , integer *, real *, integer *, real *);
    static real xv1, xv2, xx1, xx2;
    extern /* Subroutine */ int zzphys_(real *, real *);
#define buf (equiv_0)
    static real dxv;

    /* Fortran I/O blocks */
    static icilist io___20 = { 0, buf10, 0, fmt_101, 10, 1 };
    static icilist io___21 = { 0, buf10, 0, fmt_102, 10, 1 };



/*  Draw an axis in the x-direction from (X1,Y) to (X2,Y)  [user coords] 
*/
/*  with the specified divisions and tics.  If ISIDE is positive, the */
/*  tic marks appear in the +y direction and the labels in the -y */
/*  direction from the axis.  If ILAB = 0, no labels are drawn. */
/* .......................................................................
 */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

/*  Internal Data for PLOTPAK */

    if (*x1 == *x2) {
	goto L8000;
    }

    xv1 = dmin(*x1,*x2);
    xv2 = dmax(*x1,*x2);

/*  For log x-axis, must push lower value of X down and upper value of */
/*  X up to powers of 10. */

    if (zzzplt_1.ixcoor < 0) {
	zzlgin_(&xv1, &xv1, &nl1);
	temp = xv2;
	zzlgin_(&temp, &xv2, &nl2);
	if (xv2 <= temp * .999f) {
	    xv2 *= 10.f;
	    ++nl2;
	}
	ndec = nl2 - nl1;
	if (ndec <= 0) {
	    goto L8000;
	}
    }
/* .......................................................................
 */
/*  Convert to physical coordinates and plot axes */

    xx1 = xv1;
    xx2 = xv2;
    yy = *y;
    zzphys_(&xx1, &temp);
    zzphys_(&xx2, &yy);

    if (zzzplt_1.ixcoor >= 0) {
	r__1 = *iside * zzzplt_1.tmajx;
	r__2 = *iside * zzzplt_1.tminx;
	zzlinx_(&xx1, &xx2, &yy, &zzzplt_1.majrx, &r__1, &zzzplt_1.minrx, &
		r__2);
    } else {
	r__1 = *iside * zzzplt_1.tmajx;
	r__2 = *iside * zzzplt_1.tminx;
	zzlogx_(&xx1, &xx2, &yy, &ndec, &r__1, &r__2);
    }
/* .......................................................................
 */
/*  Plot labels */

    if (*ilab == 0) {
	goto L8000;
    }

    if (zzzplt_1.ixcoor >= 0) {
	nlab = zzzplt_1.majrx;
    } else {
	nlab = ndec;
    }

    dxlab = (xx2 - xx1) / nlab;
    ylab = yy - *iside * .0011f * zzzplt_1.isizx * (zzzplt_1.xpgmax - 
	    zzzplt_1.xpgmin);

    if (zzzplt_1.ixcoor >= 0) {
	dxv = (xv2 - xv1) / nlab;
    }

    i__1 = nlab;
    for (il = 0; il <= i__1; ++il) {
	if (zzzplt_1.ixcoor >= 0) {
	    xv = xv1 + il * dxv;
/* Computing MIN */
	    r__1 = dabs(xv1), r__2 = dabs(xv2);
	    if (dabs(xv) <= dmin(r__1,r__2) * 1e-5f) {
		xv = 0.f;
	    }
	    zzlabl_(&xv, buf, &nchar, 1L);
	} else {
	    npower = nl1 + il;
	    if (abs(npower) < 10) {
		s_wsfi(&io___20);
		do_fio(&c__1, (char *)&npower, (ftnlen)sizeof(integer));
		e_wsfi();
		nchar = 5;
	    } else {
		s_wsfi(&io___21);
		do_fio(&c__1, (char *)&npower, (ftnlen)sizeof(integer));
		e_wsfi();
		nchar = 6;
	    }
	    if (*(unsigned char *)&buf[3] == ' ') {
		*(unsigned char *)&buf[3] = '+';
	    }
	}
	xx = xx1 + il * dxlab;
	i__2 = -nchar;
	pwritf_(&xx, &ylab, buf, &i__2, &zzzplt_1.isizx, &c__0, &c__0, 1L);
/* L100: */
    }
/* .......................................................................
 */
L8000:
    return 0;
} /* zzaxxx_ */

#undef buf
#undef buf10


