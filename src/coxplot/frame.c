/* frame.f -- translated by f2c (version 19961017).
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

/* ======================================================================= */
/*  Device dependent plotting routines: */



/* Subroutine */ int frame_(void)
{

/*  Stop plotting in the current window (or frame). */
/* .......................................................................
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
/* cc      IF( NPLOTR.EQ.1 .OR. NPLOTR.EQ.2 )THEN */
/* cc         CALL GPAGE */
/* cc      ENDIF */

/* cc      IF( NPLOTR .EQ. 3 )THEN */
/* cc         CLOSE( UNIT=99 ) */
/* cc      ENDIF */

/* cc      IF( NPLOTR .EQ. 4 )THEN */
/* cc         CALL ZZUPFR */
/* cc      ENDIF */

/* cc      IF( NPLOTR .EQ. 5 )THEN */
/* cc         CALL ZZPCFR */
/* cc      ENDIF */

/* cc      IF( NPLOTR .EQ. 6 )THEN */
/* cc         CALL ZZPSFR */
/* cc      ENDIF */


/*  Internal Data for PLOTPAK */

    return 0;
} /* frame_ */

