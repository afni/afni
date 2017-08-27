/* colcalc.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static integer c__1 = 1;
static logical c_true = TRUE_;
static integer c__3 = 3;
static integer c__5 = 5;

/* Main program */ MAIN__(void)
{
    /* Format strings */
    static char fmt_101[] = "(\002 output\002,i1,\002> \002$)";
    static char fmt_111[] = "(a)";
    static char fmt_1001[] = "(\002 Must enter at least one output column"
	    "!\002)";
    static char fmt_1002[] = "(\002 Enter \002,a6,\002 filename           :"
	    " \002$)";
    static char fmt_1019[] = "(\002*** cannot read numbers from that file "
	    "***\002)";
    static char fmt_1029[] = "(/\002 *** max column # in expressions =\002,i"
	    "3/\002 ***              in input file  =\002,i3/\002 *** trailin"
	    "g columns set to zero ***\002/)";
    static char fmt_1031[] = "(\002 OK, enter number of rows to run: \002$)";
    static char fmt_1051[] = "(\002 stopping expression (end < 0) : \002$)";
    static char fmt_1201[] = "(9(1x,1pg20.13))";

    /* System generated locals */
    integer i__1, i__2, i__3;
    olist o__1;
    cllist cl__1;
    alist al__1;
    static doublereal equiv_0[1];

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_rsfe(cilist *), e_rsfe(void), s_cmp(char *, char *, ftnlen, 
	    ftnlen), f_open(olist *), f_rew(alist *), f_clos(cllist *), 
	    s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void);

    /* Local variables */
    static integer ncol, ierr, num_code__[26], irow, nout, nrow;
    static doublereal rout[9], r8val[26];
    static integer i__, ncmax;
    extern integer inumc_(char *, ftnlen);
#define c8 ((char *)equiv_0)
#define r8 (equiv_0)
    static char c_code__[8*200*26];
    static integer ialpha;
    static char c_expr__[666];
    extern /* Subroutine */ int parser_(char *, logical *, integer *, char *, 
	    ftnlen, ftnlen);
    static logical lstout;
    extern doublereal pareval_(integer *, char *, doublereal *, ftnlen);
    static char c_cstop__[8*200];
    static integer n_cstop__;
    static doublereal r_cstop__;

    /* Fortran I/O blocks */
    static cilist io___8 = { 0, 6, 0, fmt_101, 0 };
    static cilist io___9 = { 1, 5, 1, fmt_111, 0 };
    static cilist io___13 = { 0, 6, 0, fmt_1001, 0 };
    static cilist io___15 = { 0, 6, 0, fmt_1002, 0 };
    static cilist io___16 = { 1, 5, 1, fmt_111, 0 };
    static cilist io___18 = { 0, 77, 0, fmt_111, 0 };
    static cilist io___19 = { 0, 6, 0, fmt_1019, 0 };
    static cilist io___20 = { 0, 6, 0, fmt_1029, 0 };
    static cilist io___21 = { 0, 6, 0, fmt_1031, 0 };
    static cilist io___22 = { 0, 5, 0, 0, 0 };
    static cilist io___24 = { 0, 6, 0, fmt_1002, 0 };
    static cilist io___25 = { 1, 5, 1, fmt_111, 0 };
    static cilist io___28 = { 0, 6, 0, fmt_1051, 0 };
    static cilist io___29 = { 1, 5, 1, fmt_111, 0 };
    static cilist io___32 = { 1, 77, 1, 0, 0 };
    static cilist io___35 = { 0, 6, 0, fmt_1201, 0 };
    static cilist io___36 = { 0, 78, 0, fmt_1201, 0 };




/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

    for (i__ = 1; i__ <= 26; ++i__) {
	r8val[i__ - 1] = 0.;
/* L90: */
    }

    nout = 1;
    ncmax = 0;
    ialpha = 'A' - 1;
/* .......................................................................
 */
L100:
    s_wsfe(&io___8);
    do_fio(&c__1, (char *)&nout, (ftnlen)sizeof(integer));
    e_wsfe();
    i__1 = s_rsfe(&io___9);
    if (i__1 != 0) {
	goto L1000;
    }
    i__1 = do_fio(&c__1, c_expr__, 666L);
    if (i__1 != 0) {
	goto L1000;
    }
    i__1 = e_rsfe();
    if (i__1 != 0) {
	goto L1000;
    }
    if (s_cmp(c_expr__, " ", 666L, 1L) == 0 || s_cmp(c_expr__, "end", 666L, 
	    3L) == 0 || s_cmp(c_expr__, "exit", 666L, 4L) == 0 || s_cmp(
	    c_expr__, "quit", 666L, 4L) == 0) {
	goto L1000;
    }

    parser_(c_expr__, &c_true, &num_code__[nout - 1], c_code__ + (nout * 200 
	    - 200 << 3), 666L, 8L);

    if (num_code__[nout - 1] <= 0) {
	goto L100;
    }

/*  find maximum symbol (column) reference */

    i__1 = num_code__[nout - 1] - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (s_cmp(c_code__ + (i__ + nout * 200 - 201 << 3), "PUSHSYM", 8L, 7L)
		 == 0) {
/* Computing MAX */
	    i__2 = ncmax, i__3 = *(unsigned char *)&c_code__[(i__ + 1 + nout *
		     200 - 201) * 8] - ialpha;
	    ncmax = max(i__2,i__3);
	}
/* L200: */
    }

    ++nout;
    if (nout <= 9) {
	goto L100;
    }
/* ---------------------------------------------------------------------- 
*/
L1000:
    --nout;
    if (nout <= 0) {
	s_wsfe(&io___13);
	e_wsfe();
	goto L9000;
    }

L1010:
    ncol = 0;
    s_wsfe(&io___15);
    do_fio(&c__1, "input", 5L);
    e_wsfe();
    i__1 = s_rsfe(&io___16);
    if (i__1 != 0) {
	goto L100001;
    }
    i__1 = do_fio(&c__1, c_expr__, 666L);
    if (i__1 != 0) {
	goto L100001;
    }
    i__1 = e_rsfe();
L100001:
    if (i__1 < 0) {
	goto L9000;
    }
    if (i__1 > 0) {
	goto L1010;
    }
    if (*(unsigned char *)c_expr__ == ' ') {
	goto L1030;
    }

    o__1.oerr = 1;
    o__1.ounit = 77;
    o__1.ofnmlen = 666;
    o__1.ofnm = c_expr__;
    o__1.orl = 0;
    o__1.osta = "OLD";
    o__1.oacc = 0;
    o__1.ofm = "FORMATTED";
    o__1.oblnk = 0;
    ierr = f_open(&o__1);
    if (ierr != 0) {
	goto L1010;
    }

/*  Find out how many columns of numbers are in this file by */
/*  reading the first line */

    s_rsfe(&io___18);
    do_fio(&c__1, c_expr__, 666L);
    e_rsfe();
    al__1.aerr = 0;
    al__1.aunit = 77;
    f_rew(&al__1);
    ncol = inumc_(c_expr__, 666L);
/* cc      write(*,7707) ncol */
/* cc7707  format('inumc returns ',I5) */
    if (ncol <= 0) {
	s_wsfe(&io___19);
	e_wsfe();
	cl__1.cerr = 0;
	cl__1.cunit = 77;
	cl__1.csta = 0;
	f_clos(&cl__1);
	goto L1010;
    }

    if (ncmax > ncol) {
	s_wsfe(&io___20);
	do_fio(&c__1, (char *)&ncmax, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ncol, (ftnlen)sizeof(integer));
	e_wsfe();
    }
    ncol = min(ncol,ncmax);

L1030:
    if (ncol == 0) {
	s_wsfe(&io___21);
	e_wsfe();
	s_rsle(&io___22);
	do_lio(&c__3, &c__1, (char *)&nrow, (ftnlen)sizeof(integer));
	e_rsle();
	if (nrow <= 0) {
	    goto L1030;
	}
    } else {
	nrow = 999999;
    }

    s_wsfe(&io___24);
    do_fio(&c__1, "output", 6L);
    e_wsfe();
    i__1 = s_rsfe(&io___25);
    if (i__1 != 0) {
	goto L100002;
    }
    i__1 = do_fio(&c__1, c_expr__, 666L);
    if (i__1 != 0) {
	goto L100002;
    }
    i__1 = e_rsfe();
L100002:
    if (i__1 < 0) {
	goto L9000;
    }
    if (i__1 > 0) {
	goto L1030;
    }

    lstout = *(unsigned char *)c_expr__ == ' ';

    if (! lstout) {
	o__1.oerr = 1;
	o__1.ounit = 78;
	o__1.ofnmlen = 666;
	o__1.ofnm = c_expr__;
	o__1.orl = 0;
	o__1.osta = "NEW";
	o__1.oacc = 0;
	o__1.ofm = "FORMATTED";
	o__1.oblnk = 0;
	ierr = f_open(&o__1);
	if (ierr != 0) {
	    goto L1030;
	}
    }
/* ..................................................................... 
*/
L1050:
    n_cstop__ = 0;
    s_wsfe(&io___28);
    e_wsfe();
    i__1 = s_rsfe(&io___29);
    if (i__1 != 0) {
	goto L1090;
    }
    i__1 = do_fio(&c__1, c_expr__, 666L);
    if (i__1 != 0) {
	goto L1090;
    }
    i__1 = e_rsfe();
    if (i__1 != 0) {
	goto L1090;
    }
    if (s_cmp(c_expr__, " ", 666L, 1L) == 0 || s_cmp(c_expr__, "1", 666L, 1L) 
	    == 0) {
	goto L1090;
    }
    parser_(c_expr__, &c_true, &n_cstop__, c_cstop__, 666L, 8L);
    if (n_cstop__ <= 0) {
	goto L1050;
    }
/* ..................................................................... 
*/
L1090:
    irow = 0;
L1100:
    ++irow;
    r8val[25] = (doublereal) irow;
    if (ncol > 0) {
	i__1 = s_rsle(&io___32);
	if (i__1 != 0) {
	    goto L9000;
	}
	i__2 = ncol;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__1 = do_lio(&c__5, &c__1, (char *)&r8val[i__ - 1], (ftnlen)
		    sizeof(doublereal));
	    if (i__1 != 0) {
		goto L9000;
	    }
	}
	i__1 = e_rsle();
	if (i__1 != 0) {
	    goto L9000;
	}
    }

    i__1 = nout;
    for (i__ = 1; i__ <= i__1; ++i__) {
	rout[i__ - 1] = pareval_(&num_code__[i__ - 1], c_code__ + (i__ * 200 
		- 200 << 3), r8val, 8L);
/* L1200: */
    }
    if (n_cstop__ > 0) {
	r_cstop__ = pareval_(&n_cstop__, c_cstop__, r8val, 8L);
	if (r_cstop__ < 0.) {
	    goto L9000;
	}
    }

    if (lstout) {
	s_wsfe(&io___35);
	i__1 = nout;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&rout[i__ - 1], (ftnlen)sizeof(doublereal));
	}
	e_wsfe();
    } else {
	s_wsfe(&io___36);
	i__1 = nout;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&rout[i__ - 1], (ftnlen)sizeof(doublereal));
	}
	e_wsfe();
    }

    if (ncol > 0 || irow < nrow) {
	goto L1100;
    }
/* .......................................................................
 */
L9000:
    return 0;
} /* MAIN__ */

#undef r8
#undef c8





integer inumc_(char *cline, ftnlen cline_len)
{
    /* System generated locals */
    integer ret_val, i__1;
    doublereal d__1;
    icilist ici__1;

    /* Builtin functions */
    integer s_rsli(icilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsli(void);

    /* Local variables */
    static integer ierr;
    static doublereal rval[26];
    static integer itry, i__;


/*  Find how many columns there are in the string CLINE */



/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

    for (itry = 1; itry <= 26; ++itry) {
	i__1 = itry;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    rval[i__ - 1] = -9.876543e26;
/* L50: */
	}
	ici__1.icierr = 1;
	ici__1.iciend = 1;
	ici__1.icirnum = 1;
	ici__1.icirlen = cline_len;
	ici__1.iciunit = cline;
	ici__1.icifmt = 0;
	ierr = s_rsli(&ici__1);
	if (ierr != 0) {
	    goto L100003;
	}
	i__1 = itry;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ierr = do_lio(&c__5, &c__1, (char *)&rval[i__ - 1], (ftnlen)
		    sizeof(doublereal));
	    if (ierr != 0) {
		goto L100003;
	    }
	}
	ierr = e_rsli();
L100003:
	if (ierr != 0) {
	    goto L200;
	}
	if ((d__1 = rval[itry - 1] / -9.876543e26 - 1., abs(d__1)) <= 1e-11) {
	    goto L200;
	}
/* L100: */
    }
    itry = 27;

L200:
    ret_val = itry - 1;
    return ret_val;
} /* inumc_ */

/* Main program alias */ int colcalc_ () { MAIN__ (); return 0; }
