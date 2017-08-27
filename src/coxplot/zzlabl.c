/* zzlabl.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static integer c__1 = 1;




/* Subroutine */ int zzlabl_(real *val, char *cout, integer *nchar, ftnlen 
	cout_len)
{
    /* Format strings */
    static char fmt_101[] = "(f9.3)";
    static char fmt_301[] = "(1pe9.2)";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfi(icilist *), do_fio(integer *, char *, ftnlen), e_wsfi(void)
	    ;

    /* Local variables */
    static integer nbot, ntop, n, nch;
    static char buf[10];

    /* Fortran I/O blocks */
    static icilist io___3 = { 0, buf, 0, fmt_101, 10, 1 };
    static icilist io___6 = { 0, buf, 0, fmt_301, 10, 1 };



/*  Generate a character string for a label for a linear axis in DRAXES */
/* .......................................................................
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    /* Parameter adjustments */
    --cout;

    /* Function Body */
    if (*val == 0.f) {
	s_copy(buf, "0", 10L, 1L);
	nch = 1;
/* ...................................................................
.... */
/*   Intermediate values get an F format. */

    } else if (dabs(*val) >= .01f && dabs(*val) <= 9999.99f) {
	s_wsfi(&io___3);
	do_fio(&c__1, (char *)&(*val), (ftnlen)sizeof(real));
	e_wsfi();

/*  Strip off leading blanks */

	nbot = 1;
L100:
	if (*(unsigned char *)&buf[nbot - 1] != ' ') {
	    goto L200;
	}
	++nbot;
	if (nbot < 9) {
	    goto L100;
	}
L200:

/*  Strip off trailing zeroes */

	ntop = 9;
L300:
	if (*(unsigned char *)&buf[ntop - 1] != '0') {
	    goto L400;
	}
	--ntop;
	if (ntop > nbot) {
	    goto L300;
	}
L400:

/*  Store desired part of string in first part of BUF */

	nch = ntop - nbot + 1;
	s_copy(buf, buf + (nbot - 1), nch, ntop - (nbot - 1));
/* ...................................................................
.... */
/*  Large or small values get an E format. */

    } else {
	s_wsfi(&io___6);
	do_fio(&c__1, (char *)&(*val), (ftnlen)sizeof(real));
	e_wsfi();
	if (*(unsigned char *)buf == ' ') {
	    s_copy(buf, buf + 1, 8L, 8L);
	    nch = 8;
	} else {
	    nch = 9;
	}
    }
/* .......................................................................
 */
    i__1 = nch;
    for (n = 1; n <= i__1; ++n) {
	*(unsigned char *)&cout[n] = *(unsigned char *)&buf[n - 1];
/* L900: */
    }
    *nchar = nch;

    return 0;
} /* zzlabl_ */

