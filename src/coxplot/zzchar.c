/* zzchar.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"




/* Subroutine */ int zzchar_(char *ch, real *xp, real *yp, real *ct, real *st,
	 ftnlen ch_len)
{
    /* Initialized data */

    static integer ia[128] = { 473,473,473,473,473,473,473,473,473,473,473,
	    473,473,473,473,473,473,473,473,473,473,473,473,473,473,473,473,
	    473,473,473,473,473,473,473,473,473,473,473,473,473,448,456,429,
	    414,476,423,486,444,143,286,296,308,326,339,352,368,378,398,473,
	    473,473,464,473,473,473,1,13,28,40,49,60,68,82,92,104,113,123,130,
	    137,273,157,166,182,194,210,219,229,236,245,252,262,448,473,456,
	    473,474,473,1,13,28,40,49,60,68,82,92,104,113,123,130,137,273,157,
	    166,182,194,210,219,229,236,245,252,262,473,473,473,473,473 };
    static integer ku[494] = { 0,4,7,0,0,1,3,4,4,7,6,7,0,3,4,4,3,0,7,3,4,4,3,
	    0,7,6,7,7,4,3,1,0,0,1,3,4,7,6,7,0,3,4,4,3,0,7,6,7,0,4,7,3,0,7,0,4,
	    7,6,7,0,4,7,0,3,7,6,7,7,4,3,1,0,0,1,3,4,4,3,7,6,7,0,7,0,4,7,4,4,7,
	    6,7,7,1,3,7,2,2,7,1,3,7,6,7,7,0,1,3,4,4,7,6,7,0,7,0,4,7,2,4,7,6,7,
	    7,0,0,4,7,6,7,0,2,4,4,7,6,7,0,4,4,7,6,7,4,7,4,4,3,1,0,0,1,3,4,7,6,
	    7,0,3,4,4,3,0,7,6,7,7,0,0,1,3,4,4,3,1,0,7,2,4,7,6,7,0,3,4,4,3,0,7,
	    2,4,7,6,7,7,0,1,3,4,4,3,1,0,0,1,3,4,7,6,7,7,0,4,7,2,2,7,6,7,7,0,0,
	    1,3,4,4,7,6,7,7,0,2,4,7,6,7,7,0,0,2,4,4,7,6,7,4,7,0,4,7,6,7,7,0,2,
	    4,7,2,2,7,6,7,7,3,1,7,0,4,0,4,7,6,7,7,4,3,1,0,0,1,3,4,4,7,6,7,7,1,
	    2,2,7,1,3,7,6,7,7,0,1,3,4,4,0,0,4,7,6,7,7,0,1,3,4,4,3,1,7,3,4,4,3,
	    1,0,7,6,7,7,3,3,2,0,0,4,7,2,4,7,6,7,7,0,1,3,4,4,3,0,0,4,7,6,7,7,4,
	    3,1,0,0,1,3,4,4,3,1,0,7,6,7,7,0,0,4,4,2,2,7,6,7,7,2,0,0,1,3,4,4,2,
	    2,0,0,1,3,4,4,2,7,6,7,7,0,1,3,4,4,3,1,0,0,1,3,4,7,6,7,7,0,4,7,2,2,
	    7,6,7,7,0,4,7,6,7,7,0,4,7,2,2,7,4,0,7,0,4,7,6,7,4,7,6,7,7,3,2,2,3,
	    7,6,7,7,1,2,2,1,7,6,7,7,4,0,7,0,4,7,6,7,7,6,7,7,1,2,2,1,1,2,7,6,7,
	    7,2,1,1,2,2,7,6,7 };
    static integer kv[494] = { 3,3,0,3,6,7,7,6,0,0,0,7,7,7,6,5,4,4,0,4,3,1,0,
	    0,0,0,7,0,6,7,7,6,1,0,0,1,0,0,7,7,7,6,1,0,0,0,0,7,7,7,0,4,4,0,0,0,
	    0,0,7,7,7,0,4,4,0,0,7,0,6,7,7,6,1,0,0,1,3,3,0,0,7,7,0,4,4,0,7,0,0,
	    0,7,0,7,7,0,7,0,0,0,0,0,0,7,0,1,0,0,1,7,0,0,7,7,0,3,7,0,5,0,0,0,7,
	    0,7,0,0,0,0,7,7,3,7,0,0,0,7,7,0,7,0,0,7,7,0,1,6,7,7,6,1,0,0,1,0,0,
	    7,7,7,6,5,4,4,0,0,7,0,1,6,7,7,6,1,0,0,1,0,2,0,0,0,7,7,7,6,5,4,4,0,
	    4,0,0,0,7,0,1,0,0,1,3,4,4,5,6,7,7,6,0,0,7,0,7,7,0,7,0,0,0,7,0,7,1,
	    0,0,1,7,0,0,7,0,7,0,7,0,0,7,0,7,0,4,0,7,0,0,7,7,0,7,0,0,0,7,0,7,4,
	    7,0,4,0,0,0,7,0,4,4,0,7,7,0,0,0,0,7,0,1,0,0,1,6,7,7,6,1,0,0,7,0,6,
	    7,0,0,0,0,0,0,7,0,6,7,7,6,5,1,0,0,0,0,7,0,7,7,7,6,5,4,4,0,4,3,1,0,
	    0,1,0,0,7,0,0,7,7,4,3,3,0,0,0,0,0,7,0,1,0,0,1,3,4,4,7,7,0,0,7,0,7,
	    7,7,6,1,0,0,1,3,4,4,3,0,0,7,0,6,7,7,6,1,0,0,0,7,0,4,5,6,7,7,6,5,4,
	    4,2,1,0,0,1,2,4,0,0,7,0,1,0,0,1,6,7,7,6,4,3,3,4,0,0,7,0,3,3,0,5,1,
	    0,0,7,0,3,3,0,0,7,0,1,5,0,5,1,0,3,3,0,5,1,0,0,7,7,0,0,7,1,7,6,1,0,
	    0,0,7,0,7,6,1,0,0,0,7,0,5,5,0,2,2,0,0,7,0,0,7,0,0,1,2,2,1,1,0,0,7,
	    0,0,0,1,1,0,0,0,7 };

    static real xold, yold, xnew, ynew;
    static integer nu, nv, ipoint;
    extern /* Subroutine */ int zzline_(real *, real *, real *, real *);
    static real ctl, stl;


/*  Plot one character in CH with lower left corner at XP,YP physical */
/*  coordinates, with CT and ST the cosine and sine scaling/rotation */
/*  factors. */

/* .......................................................................
 */
/*  The following digitization stuff is stolen from the NCAR metacode */
/*  interpreter MCVAX.  Various minor changes have been made.  Most */
/*  notable of these is the interchange of the '0' and 'O' characters -- 
*/
/*  I just couldn't stand the slash going through the 'O' as the CDC */
/*  custom has it. */


/*  The following pointers relate standard FORTRAN characters to their */
/*  digitizations.  Note the plethora of 473's.  That location does */
/*  nothing. */

/*  <control characters> */
/*  <ctrls> */
/*  <ctrls> */
/*  <ctrls> */
/*  <ctrls> */
/*  <ctrls> */
/*  <ctrl><ctrl><space>!" */
/*  #$%&' */
/*  ()*+, */
/*  -./01 */
/*  23456 */
/*  789:; */
/*  <=>?@ */
/*  ABCDE */
/*  FGHIJ */
/*  KLMNO */
/*  PQRST */
/*  UVWXY */
/*  Z[\]^ */
/*  _` */
/*  abcde */
/*  fghij */
/*  klmno */
/*  pqrst */
/*  uvwxy */
/*  z */
/*  {| */
/*  }~<DEL> */

/*  The following DATA statements contain the digitizations of the */
/*  characters.  The characters are digitized on a box 6 units wide and */
/*  7 units tall.  This includes 2 units of white space to the right of */
/*  each character.  If KU=7, KV is a flag: */
/*     KV=0 ==> the next KU and KV are a pen up move */
/*              (normal coordinates are pen down moves) */
/*     KV=7 ==> the end of the digitization for a particular character */
/*              has been reached. */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
/*  Select digitization for this character. */

    xold = *xp;
    yold = *yp;
    ipoint = ia[*(unsigned char *)ch];

/*  Scale lower case letters to be slightly smaller */

    if (*(unsigned char *)ch >= 'a' && *(unsigned char *)ch <= 'z') {
	ctl = *ct * .8f;
	stl = *st * .8f;
    } else {
	ctl = *ct;
	stl = *st;
    }

L100:
    nu = ku[ipoint - 1];
    nv = kv[ipoint - 1];
    ++ipoint;
/* .......................................................................
 */
/*  Test for op-code stored in NV.  This is flagged by a 7 in NU. */

    if (nu == 7) {

/*  Op-codes are: NV = 7             ==> end of character */
/*                     anything else ==> pen up move to next location 
*/

	if (nv == 7) {
	    return 0;
	} else {
	    xold = *xp + ctl * ku[ipoint - 1] - stl * kv[ipoint - 1];
	    yold = *yp + stl * ku[ipoint - 1] + ctl * kv[ipoint - 1];
	    ++ipoint;
	}
/* ...................................................................
.... */
/*  Here, plot the next stroke. */

    } else {
	xnew = *xp + ctl * nu - stl * nv;
	ynew = *yp + stl * nu + ctl * nv;
	zzline_(&xold, &yold, &xnew, &ynew);
	xold = xnew;
	yold = ynew;
    }
/* .......................................................................
 */
/*  Loopback to get next plotting order from KU, KV. */

    goto L100;
} /* zzchar_ */

