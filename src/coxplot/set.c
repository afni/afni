/* set.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include <stdlib.h>
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

/* ======================================================================= */
/*  Device independent setup routines: */



/* Subroutine */ int set_(real *xobj1, real *xobj2, real *yobj1, real *yobj2, 
	real *xsub1, real *xsub2, real *ysub1, real *ysub2, integer *ltype)
{
    /* Initialized data */

    static shortint ixc[4] = { 1,1,-1,-1 };
    static shortint iyc[4] = { 1,-1,1,-1 };

    /* Format strings */
    static char fmt_9001[] = "(//\002 ********** Illegal parameters in SET *"
	    "*********\002/4(1x,1pg12.5)/4(1x,1pg12.5),i6)";

    /* Builtin functions */
    double r_lg10(real *);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static real sxmin, sxmax, symin, symax;

    /* Fortran I/O blocks */
    static cilist io___7 = { 0, 6, 0, fmt_9001, 0 };



/*  Set the relationship between the physical space and the user space. */
/* .......................................................................
 */


/*  Internal Data for PLOTPAK */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
/*  Check entry values for reasonableness. */

    if (*xobj1 < zzzplt_1.xpgmin || *xobj1 >= *xobj2 || *xobj2 > 
	    zzzplt_1.xpgmax || *xsub1 == *xsub2 || *yobj1 < zzzplt_1.ypgmin ||
	     *yobj1 >= *yobj2 || *yobj2 > zzzplt_1.ypgmax || *ysub1 == *ysub2 
	    || *ltype <= 0 || *ltype > 4) {
	goto L9000;
    }
/* .......................................................................
 */
    zzzplt_1.xbot = *xobj1;
    zzzplt_1.ybot = *yobj1;
    zzzplt_1.xtop = *xobj2;
    zzzplt_1.ytop = *yobj2;

    zzzplt_1.xmin = *xsub1;
    zzzplt_1.xmax = *xsub2;
    zzzplt_1.ymin = *ysub1;
    zzzplt_1.ymax = *ysub2;

    zzzplt_1.ixcoor = ixc[*ltype - 1];
    zzzplt_1.iycoor = iyc[*ltype - 1];

    if (zzzplt_1.ixcoor >= 0) {
	sxmin = *xsub1;
	sxmax = *xsub2;
    } else {
	if (*xsub1 <= 0.f || *xsub2 <= 0.f) {
	    goto L9000;
	}
	sxmin = r_lg10(xsub1);
	sxmax = r_lg10(xsub2);
    }

    if (zzzplt_1.iycoor >= 0) {
	symin = *ysub1;
	symax = *ysub2;
    } else {
	if (*ysub1 <= 0.f || *ysub2 <= 0.f) {
	    goto L9000;
	}
	symin = r_lg10(ysub1);
	symax = r_lg10(ysub2);
    }

/*  Calculate the alpha and beta scaling factors to map user space */
/*  into physical space. */

    zzzplt_1.alphxx = (zzzplt_1.xtop - zzzplt_1.xbot) / (sxmax - sxmin);
    zzzplt_1.betaxx = zzzplt_1.xbot - zzzplt_1.alphxx * sxmin;

    zzzplt_1.alphyy = (zzzplt_1.ytop - zzzplt_1.ybot) / (symax - symin);
    zzzplt_1.betayy = zzzplt_1.ybot - zzzplt_1.alphyy * symin;

    return 0;
/* .......................................................................
 */
L9000:
/* CC      OPEN( 98 , FILE='PLOTPAK.ERR' , STATUS='NEW' ) */
/* CC      WRITE(98,9001) XOBJ1,XOBJ2 , YOBJ1,YOBJ2 , */
/* CC     X               XSUB1,XSUB2 , YSUB1,YSUB2 , LTYPE */
/* L9001: */
/* cc      CLOSE( 98 ) */

    s_wsfe(&io___7);
    do_fio(&c__1, (char *)&(*xobj1), (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&(*xobj2), (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&(*yobj1), (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&(*yobj2), (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&(*xsub1), (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&(*xsub2), (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&(*ysub1), (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&(*ysub2), (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&(*ltype), (ftnlen)sizeof(integer));
    e_wsfe();
    exit(0) ;
    return 0;
} /* set_ */

