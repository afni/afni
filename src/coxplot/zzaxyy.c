/* zzaxyy.f -- translated by f2c (version 19961017).
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




/* Subroutine */ int zzaxyy_(real *x, real *y1, real *y2, integer *iside, 
	integer *ilab)
{
    /* Format strings */
    static char fmt_101[] = "(\0021.E\002,i2)";
    static char fmt_102[] = "(\0021.E\002,i3)";

    /* System generated locals */
    integer i__1, i__2, i__3;
    real r__1, r__2;
    static char equiv_0[10];

    /* Builtin functions */
    integer s_wsfi(icilist *), do_fio(integer *, char *, ftnlen), e_wsfi(void)
	    ;

    /* Local variables */
    static integer ndec, nlab;
#define buf10 (equiv_0)
    static real xlab, temp;
    static integer nchar;
    static real dylab, csize, dycsz;
    static integer il;
    static real yv, xx, yy;
    extern /* Subroutine */ int zzlgin_(real *, real *, integer *);
    static integer nl1, nl2, npower, nshift;
    extern /* Subroutine */ int zzlabl_(real *, char *, integer *, ftnlen), 
	    pwritf_(real *, real *, char *, integer *, integer *, integer *, 
	    integer *, ftnlen), zzlogy_(real *, real *, real *, integer *, 
	    real *, real *), zzliny_(real *, real *, real *, integer *, real *
	    , integer *, real *);
    static real yv1, yv2, yy1, yy2;
    extern /* Subroutine */ int zzphys_(real *, real *);
#define buf (equiv_0)
    static real dyv;

    /* Fortran I/O blocks */
    static icilist io___23 = { 0, buf10, 0, fmt_101, 10, 1 };
    static icilist io___24 = { 0, buf10, 0, fmt_102, 10, 1 };



/*  Draw an axis in the y-direction from (X,Y1) to (X,Y2)  [user coords] 
*/
/*  with the specified divisions and tics.  If ISIDE is positive, the */
/*  tic marks appear in the +x direction and the labels in the -x */
/*  direction from the axis.  If ILAB = 0, no labels are drawn. */
/* .......................................................................
 */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

/*  Internal Data for PLOTPAK */

    if (*y1 == *y2) {
	goto L8000;
    }

    yv1 = dmin(*y1,*y2);
    yv2 = dmax(*y1,*y2);

/*  For log y-axis, must push lower value of Y down and upper value of */
/*  Y up to powers of 10. */

    if (zzzplt_1.iycoor < 0) {
	zzlgin_(&yv1, &yv1, &nl1);
	temp = yv2;
	zzlgin_(&temp, &yv2, &nl2);
	if (yv2 <= temp * .999f) {
	    yv2 *= 10.f;
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

    yy1 = yv1;
    yy2 = yv2;
    xx = *x;
    zzphys_(&temp, &yy1);
    zzphys_(&xx, &yy2);

    if (zzzplt_1.iycoor >= 0) {
	r__1 = *iside * zzzplt_1.tmajy;
	r__2 = *iside * zzzplt_1.tminy;
	zzliny_(&xx, &yy1, &yy2, &zzzplt_1.majry, &r__1, &zzzplt_1.minry, &
		r__2);
    } else {
	r__1 = *iside * zzzplt_1.tmajy;
	r__2 = *iside * zzzplt_1.tminy;
	zzlogy_(&xx, &yy1, &yy2, &ndec, &r__1, &r__2);
    }
/* .......................................................................
 */
/*  Plot labels */

    if (*ilab == 0) {
	goto L8000;
    }

    if (zzzplt_1.iycoor >= 0) {
	nlab = zzzplt_1.majry;
    } else {
	nlab = ndec;
    }

/*  Calculate the max number of characters needed for labels into NSHIFT. 
*/

    if (zzzplt_1.iycoor < 0) {
/*  Log-axis:  1.E+x or 1.E+xx are the possibilities */
/* Computing MAX */
	i__1 = abs(nl1), i__2 = abs(nl2);
	npower = max(i__1,i__2);
	if (npower < 10) {
	    nshift = 5;
	} else {
	    nshift = 6;
	}
    } else {
/*  Linear-axis:  calculate all labels and find the longest */
	nshift = 1;
	dyv = (yv2 - yv1) / nlab;
	i__1 = nlab;
	for (il = 0; il <= i__1; ++il) {
	    yv = yv1 + il * dyv;
/* Computing MIN */
	    r__1 = dabs(yv1), r__2 = dabs(yv2);
	    if (dabs(yv) <= dmin(r__1,r__2) * 1e-5f) {
		yv = 0.f;
	    }
	    zzlabl_(&yv, buf, &nchar, 1L);
	    nshift = max(nshift,nchar);
/* L50: */
	}
    }

    dylab = (yy2 - yy1) / nlab;
    csize = zzzplt_1.isizy * .0011f * (zzzplt_1.xpgmax - zzzplt_1.xpgmin);
    xlab = xx - csize * *iside * nshift;
    dycsz = csize * .5f;
    if (dylab < 0.f) {
	dycsz = -dycsz;
    }

    i__1 = nlab;
    for (il = 0; il <= i__1; ++il) {
	if (zzzplt_1.iycoor >= 0) {
	    yv = yv1 + il * dyv;
/* Computing MIN */
	    r__1 = dabs(yv1), r__2 = dabs(yv2);
	    if (dabs(yv) <= dmin(r__1,r__2) * 1e-5f) {
		yv = 0.f;
	    }
	    zzlabl_(&yv, buf, &nchar, 1L);
	} else {
	    npower = nl1 + il;
	    if (abs(npower) < 10) {
		s_wsfi(&io___23);
		do_fio(&c__1, (char *)&npower, (ftnlen)sizeof(integer));
		e_wsfi();
		nchar = 5;
	    } else {
		s_wsfi(&io___24);
		do_fio(&c__1, (char *)&npower, (ftnlen)sizeof(integer));
		e_wsfi();
		nchar = 6;
	    }
	    if (*(unsigned char *)&buf[3] == ' ') {
		*(unsigned char *)&buf[3] = '+';
	    }
	}
	if (il == 0) {
	    yy = yy1 + dycsz;
	} else {
	    yy = yy1 + il * dylab;
	    if (il == nlab) {
		yy -= dycsz;
	    }
	}
	i__2 = -nchar;
	i__3 = -(*iside);
	pwritf_(&xlab, &yy, buf, &i__2, &zzzplt_1.isizy, &c__0, &i__3, 1L);
/* L100: */
    }
/* .......................................................................
 */
L8000:
    return 0;
} /* zzaxyy_ */

#undef buf
#undef buf10


