/* zzphph.f -- translated by f2c (version 19961017).
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




/* Subroutine */ int zzphph_(real *x1, real *y1, real *x2, real *y2)
{
    extern /* Subroutine */ int zzmpli_(real *, real *, real *, real *);
    static real px1, px2, py1, py2;


/*  Plot a physical coordinate line. */

/*  Everything in this package eventually funnels into this routine, */
/*  which depends on the hardware being drawn upon.  The device */
/*  is chosen by the NPLOTR variable (in 'plotpak.inc' COMMON /ZZPLTR/): 
*/

/*    1 = Los Alamos CGS SunView window */
/*    2 = Los Alamos CGS Metafile */
/*    3 = Write all line coordinates to an ASCII file for later work */
/*    4 = Write all line coordinates to a Unix plot format file */
/*    5 = Use Microsoft GRAPHICS.LIB */
/*    6 = Write PostScript code to an output file */
/*    7 = Call the C "memplot" routine */
/* .......................................................................
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

/*  Internal Data for PLOTPAK */

    if (zzpltr_1.iflip == 1) {
/*  Flipped coordinates */
	px1 = zzpltr_1.xpscal * (zzpltr_1.yphmax - *y1);
	px2 = zzpltr_1.xpscal * (zzpltr_1.yphmax - *y2);
	py1 = zzpltr_1.ypscal * (zzpltr_1.xphmax - *x1);
	py2 = zzpltr_1.ypscal * (zzpltr_1.xphmax - *x2);
    } else if (zzpltr_1.iflip == 2) {
/*  Flipped coordinates */
	px1 = zzpltr_1.xpscal * (zzpltr_1.yphmax - *y1);
	px2 = zzpltr_1.xpscal * (zzpltr_1.yphmax - *y2);
	py1 = zzpltr_1.xpscal * *x1;
	py2 = zzpltr_1.xpscal * *x2;
    } else {
/*  Normal coordinates */
	px1 = zzpltr_1.xpscal * *x1;
	px2 = zzpltr_1.xpscal * *x2;
	py1 = zzpltr_1.ypscal * *y1;
	py2 = zzpltr_1.ypscal * *y2;
    }

/*  Plot it! */

/* cc      IF( NPLOTR.EQ.1 .OR. NPLOTR.EQ.2 )THEN */
/* cc         CALL GMOVA2( PX1 , PY1 ) */
/* cc         CALL GLINA2( PX2 , PY2 ) */
/* cc      ENDIF */

/* cc      IF( NPLOTR .EQ. 3 )THEN */
/* cc         WRITE(99,101) PX1,PY1,PX2,PY2 */
/* cc101      FORMAT(4(F6.4,',')) */
/* cc      ENDIF */

/* cc      IF( NPLOTR .EQ. 4 )THEN */
/* cc         CALL ZZUPLI( NINT(PX1),NINT(PY1) , NINT(PX2),NINT(PY2) ) */
/* cc      ENDIF */

/* cc      IF( NPLOTR .EQ. 5 )THEN */
/* cc         CALL ZZPCLI( PX1,PY1 , PX2,PY2 ) */
/* cc      ENDIF */

/* cc      IF( NPLOTR .EQ. 6 )THEN */
/* cc         CALL ZZPSLI( NINT(PX1),NINT(PY1) , NINT(PX2),NINT(PY2) ) */
/* cc      ENDIF */

    if (zzpltr_1.nplotr == 7) {
	zzmpli_(&px1, &py1, &px2, &py2);
    }

/* cc      WRITE(*,999) PX1,PY1,PX2,PY2 */
/* cc999   FORMAT('ZZPHPH:',4(1X,1PG10.3)) */

    return 0;
} /* zzphph_ */

