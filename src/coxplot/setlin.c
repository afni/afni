/* setlin.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"




/* Subroutine */ int setlin_(integer *ntype)
{
    /* Initialized data */

    static integer ndash[5] = { 1,2,2,6,2 };
    static real xdash[40]	/* was [8][5] */ = { 1.f,1.f,1.f,1.f,1.f,1.f,
	    1.f,1.f,.007f,-.007f,1.f,1.f,1.f,1.f,1.f,1.f,.002f,-.003f,1.f,1.f,
	    1.f,1.f,1.f,1.f,.007f,-.004f,.002f,-.004f,.002f,-.004f,1.f,1.f,
	    2e-4f,-.005f,1.f,1.f,1.f,1.f,1.f,1.f };

    static integer nd;
    extern /* Subroutine */ int setdsh_(integer *, real *);


/*  Set default dash types */
/*    1 = solid */
/*    2 = long dash */
/*    3 = short */
/*    4 = long - short - short */
/*    5 = very short */
/*  Outside this range ==> solid */



/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

    nd = *ntype;
    if (*ntype <= 0 || *ntype > 5) {
	nd = 1;
    }

    setdsh_(&ndash[nd - 1], &xdash[(nd << 3) - 8]);
    return 0;
} /* setlin_ */

