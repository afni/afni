/* color.f -- translated by f2c (version 19961017).
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




/* Subroutine */ int color_(integer *ncol)
{
    /* Initialized data */

    static real rgb[21]	/* was [3][7] */ = { 0.f,0.f,0.f,1.f,0.f,0.f,0.f,0.f,
	    1.f,0.f,1.f,0.f,1.f,1.f,0.f,1.f,0.f,1.f,0.f,1.f,1.f };

    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer ic;
    extern /* Subroutine */ int zzmpco_(real *, real *, real *);


/*  Set the color of subsequent lines */
/* .......................................................................
 */


/*  Internal Data for PLOTPAK */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
/* cc      IF( NPLOTR.EQ.1 .OR. NPLOTR.EQ.2 )THEN */
/* cc         IF( NCOL .LE. 1 .OR. NCOL .GT. 7 )THEN */
/* cc            CALL GCOLOR( 5Hclear ) */
/* cc         ELSEIF( NCOL .EQ. 2 )THEN */
/* cc            CALL GCOLOR( 3Hred ) */
/* cc         ELSEIF( NCOL .EQ. 3 )THEN */
/* cc            CALL GCOLOR( 4Hblue ) */
/* cc         ELSEIF( NCOL .EQ. 4 )THEN */
/* cc            CALL GCOLOR( 5Hgreen ) */
/* cc         ELSEIF( NCOL .EQ. 5 )THEN */
/* cc            CALL GCOLOR( 6Hyellow ) */
/* cc         ELSEIF( NCOL .EQ. 6 )THEN */
/* cc            CALL GCOLOR( 7Hmagenta ) */
/* cc         ELSEIF( NCOL .EQ. 7 )THEN */
/* cc            CALL GCOLOR( 4Hcyan ) */
/* cc         ENDIF */

/* cc      IF( NPLOTR.EQ.5 )THEN */
/* cc         CALL ZZPCOL( NCOL ) */
/* cc      ENDIF */

/* cc      IF( NPLOTR .EQ. 6 )THEN */
/* cc         IC = MAX( 1 , MIN( 7 , NCOL ) ) */
/* cc         CALL ZZPSCO( RGB(1,IC) , RGB(2,IC) , RGB(3,IC) ) */
/* cc      ENDIF */
    if (zzpltr_1.nplotr == 7) {
/* Computing MAX */
	i__1 = 1, i__2 = min(7,*ncol);
	ic = max(i__1,i__2);
	zzmpco_(&rgb[ic * 3 - 3], &rgb[ic * 3 - 2], &rgb[ic * 3 - 1]);
    }

    return 0;
} /* color_ */




/* Subroutine */ int fcolor_(real *cr, real *cg, real *cb)
{
    extern /* Subroutine */ int zzmpco_(real *, real *, real *);


/*  Set the color of subsequent lines */
/* .......................................................................
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

/*  Internal Data for PLOTPAK */

    if (zzpltr_1.nplotr == 7) {
	zzmpco_(cr, cg, cb);
    }
    return 0;
} /* fcolor_ */

