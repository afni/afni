/* colfit.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Common Block Declarations */

struct {
    doublereal xx[32768], yy[32768];
    integer numpts;
} xydata_;

#define xydata_1 xydata_

struct {
    char ccode[4096];
    integer numcod, ivevar, numpar, ifxvar[26];
} exdata_;

#define exdata_1 exdata_

/* Table of constant values */

static integer c__1 = 1;
static integer c__5 = 5;
static logical c_true = TRUE_;
static integer c__2 = 2;
static integer c_b73 = 983040;
static integer c__32768 = 32768;

/* Main program */ MAIN__(void)
{
    /* Format strings */
    static char fmt_101[] = "(\002 Enter data file name : \002$)";
    static char fmt_111[] = "(a)";
    static char fmt_102[] = "(\002 Start, stop indices in data file  [0,0=al"
	    "l] ? \002$)";
    static char fmt_112[] = "(2i10)";
    static char fmt_119[] = "(\002   *** failure when skipping record \002,i"
	    "5)";
    static char fmt_201[] = "(\002   --- read\002,i6,\002 data lines from fi"
	    "le\002)";
    static char fmt_301[] = "(/\002 Expression to fit 2nd column to 1st ?"
	    " \002$)";
    static char fmt_309[] = "(\002   *** expression too complex!\002)";
    static char fmt_338[] = "(\002   *** not enough variable names in expres"
	    "sion!\002)";
    static char fmt_339[] = "(\002   *** too many variable names in expressi"
	    "on!\002)";
    static char fmt_341[] = "(\002 Which variable represents 1st data column"
	    " ? \002/1x,a,\002 : \002$)";
    static char fmt_351[] = "(\002  Initial value for parameter \002,a,\002 "
	    "? \002$)";
    static char fmt_501[] = "(\002   --- \002,a,\002 exits with INFO =\002,i"
	    "2)";
    static char fmt_511[] = "(3x,a,\002 -> \002,1pg14.7,\002 +/- \002,1pg14."
	    "7)";
    static char fmt_521[] = "(\002   --- mean absolute deviation =\002,1pg10"
	    ".3)";
    static char fmt_601[] = "(\002 Filename to save fit error curve in (blan"
	    "k=none) ? \002$)";
    static char fmt_611[] = "(1pg20.13,1x,1pg20.13)";

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2, i__3[2];
    doublereal d__1;
    char ch__1[1];
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(void), s_rsfe(cilist *), do_fio(integer *
	    , char *, ftnlen), e_rsfe(void), f_open(olist *), f_clos(cllist *)
	    , s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void), s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    double sqrt(doublereal);

    /* Local variables */
    static integer iend;
    static doublereal fvec[32768];
    static integer info;
    extern /* Subroutine */ int dcov_(U_fp, integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static integer ierr;
    static doublereal xpar[66];
    static integer iopt;
    static doublereal work[983040]	/* was [32768][30] */;
    static integer i__, m, n;
    static char cfile[64], chans[1];
    static integer numch;
    static char cexpr[128];
    static integer iwork[66];
    extern /* Subroutine */ int dnls1e_(U_fp, integer *, integer *, integer *,
	     doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    integer *, doublereal *, integer *);
    extern /* Subroutine */ int ff_();
    static logical lchuse[26];
    extern /* Subroutine */ int parser_(char *, logical *, integer *, char *, 
	    ftnlen, ftnlen);
    static integer ilower, iupper, istart, nprint, ich;
    static doublereal xin, yin, tol, war1[30], war2[30], war3[30], war4[32768]
	    ;

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 6, 0, fmt_101, 0 };
    static cilist io___2 = { 0, 5, 0, fmt_111, 0 };
    static cilist io___5 = { 0, 6, 0, fmt_102, 0 };
    static cilist io___6 = { 0, 5, 0, fmt_112, 0 };
    static cilist io___10 = { 1, 1, 1, 0, 0 };
    static cilist io___13 = { 0, 6, 0, fmt_119, 0 };
    static cilist io___14 = { 1, 1, 1, 0, 0 };
    static cilist io___15 = { 0, 6, 0, fmt_201, 0 };
    static cilist io___16 = { 0, 6, 0, fmt_301, 0 };
    static cilist io___17 = { 1, 5, 1, fmt_111, 0 };
    static cilist io___19 = { 0, 6, 0, fmt_309, 0 };
    static cilist io___25 = { 0, 6, 0, fmt_338, 0 };
    static cilist io___26 = { 0, 6, 0, fmt_339, 0 };
    static cilist io___27 = { 0, 6, 0, fmt_341, 0 };
    static cilist io___28 = { 0, 5, 0, fmt_111, 0 };
    static cilist io___30 = { 0, 6, 0, fmt_351, 0 };
    static cilist io___31 = { 1, 5, 1, 0, 0 };
    static cilist io___42 = { 0, 6, 0, fmt_501, 0 };
    static cilist io___47 = { 0, 6, 0, fmt_501, 0 };
    static cilist io___48 = { 0, 6, 0, fmt_511, 0 };
    static cilist io___49 = { 0, 6, 0, fmt_511, 0 };
    static cilist io___50 = { 0, 6, 0, fmt_521, 0 };
    static cilist io___51 = { 0, 6, 0, fmt_601, 0 };
    static cilist io___52 = { 0, 5, 0, fmt_111, 0 };
    static cilist io___53 = { 0, 1, 0, fmt_611, 0 };



/*  Program to do a nonlinear least squares fit of a two-column file in */
/*  the format */

/*    x1  y1 */
/*    x2  y2 */
/*    ..  .. */
/*    xN  yN */

/*  to a formula of the form  y(x) = expression(x,a,b,c,...), where */
/*  a, b, c, ... are parameters to be found. */
/* -----------------------------------------------------------------------
 */
/*  Common block XYDATA holds the data read from disk */

/* .......................................................................
 */
/*  Common block EXDATA holds the parsed expression for evaluation */

/* .......................................................................
 */
/*  local variables */




/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

/*  Open data file */

L100:
    s_wsfe(&io___1);
    e_wsfe();
    s_rsfe(&io___2);
    do_fio(&c__1, cfile, 64L);
    e_rsfe();
    if (*(unsigned char *)cfile == ' ') {
	goto L9900;
    }

    o__1.oerr = 1;
    o__1.ounit = 1;
    o__1.ofnmlen = 64;
    o__1.ofnm = cfile;
    o__1.orl = 0;
    o__1.osta = "OLD";
    o__1.oacc = 0;
    o__1.ofm = "FORMATTED";
    o__1.oblnk = 0;
    ierr = f_open(&o__1);

    if (ierr != 0) {
	cl__1.cerr = 0;
	cl__1.cunit = 1;
	cl__1.csta = 0;
	f_clos(&cl__1);
	goto L100;
    }
/* .......................................................................
 */
/*  Read data in */

    s_wsfe(&io___5);
    e_wsfe();
    s_rsfe(&io___6);
    do_fio(&c__1, (char *)&istart, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iend, (ftnlen)sizeof(integer));
    e_rsfe();

    i__1 = istart - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ierr = s_rsle(&io___10);
	if (ierr != 0) {
	    goto L100001;
	}
	ierr = do_lio(&c__5, &c__1, (char *)&xin, (ftnlen)sizeof(doublereal));
	if (ierr != 0) {
	    goto L100001;
	}
	ierr = do_lio(&c__5, &c__1, (char *)&yin, (ftnlen)sizeof(doublereal));
	if (ierr != 0) {
	    goto L100001;
	}
	ierr = e_rsle();
L100001:
	if (ierr != 0) {
	    s_wsfe(&io___13);
	    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	    e_wsfe();
	    goto L9900;
	}
/* L110: */
    }

    if (iend <= istart) {
	iend = 32768;
    } else {
/* Computing MIN */
	i__1 = 32768, i__2 = iend - istart + 1;
	iend = min(i__1,i__2);
    }

    i__ = 0;
L200:
    ierr = s_rsle(&io___14);
    if (ierr != 0) {
	goto L100002;
    }
    ierr = do_lio(&c__5, &c__1, (char *)&xin, (ftnlen)sizeof(doublereal));
    if (ierr != 0) {
	goto L100002;
    }
    ierr = do_lio(&c__5, &c__1, (char *)&yin, (ftnlen)sizeof(doublereal));
    if (ierr != 0) {
	goto L100002;
    }
    ierr = e_rsle();
L100002:
    if (ierr == 0) {
	++i__;
	xydata_1.xx[i__ - 1] = xin;
	xydata_1.yy[i__ - 1] = yin;
	if (i__ < iend) {
	    goto L200;
	}
    }

    cl__1.cerr = 0;
    cl__1.cunit = 1;
    cl__1.csta = 0;
    f_clos(&cl__1);
    xydata_1.numpts = i__;
    s_wsfe(&io___15);
    do_fio(&c__1, (char *)&xydata_1.numpts, (ftnlen)sizeof(integer));
    e_wsfe();
    if (xydata_1.numpts <= 1) {
	goto L9900;
    }
/* .......................................................................
 */
/*  Read in expression that will be used to fit */

L300:
    s_wsfe(&io___16);
    e_wsfe();
    ierr = s_rsfe(&io___17);
    if (ierr != 0) {
	goto L100003;
    }
    ierr = do_fio(&c__1, cexpr, 128L);
    if (ierr != 0) {
	goto L100003;
    }
    ierr = e_rsfe();
L100003:
    if (ierr != 0) {
	goto L9900;
    }
    parser_(cexpr, &c_true, &exdata_1.numcod, exdata_1.ccode, 128L, 8L);
    if (exdata_1.numcod <= 0) {
	goto L300;
    }
    if (exdata_1.numcod > 512) {
	s_wsfe(&io___19);
	e_wsfe();
	goto L9900;
    }
/* .......................................................................
 */
/*  Get the names of all the variables referred to in the expression */

    for (i__ = 1; i__ <= 26; ++i__) {
	lchuse[i__ - 1] = FALSE_;
/* L310: */
    }

    iupper = 'A' - 1;
    ilower = 'a' - 1;
    i__1 = exdata_1.numcod;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (s_cmp(exdata_1.ccode + (i__ - 1 << 3), "PUSHSYM", 8L, 7L) == 0) {
	    ich = *(unsigned char *)&exdata_1.ccode[i__ * 8] - iupper;
	    lchuse[ich - 1] = TRUE_;
	}
/* L320: */
    }

    s_copy(cfile, " ", 64L, 1L);
    numch = 0;
    for (i__ = 1; i__ <= 26; ++i__) {
	if (lchuse[i__ - 1]) {
	    ++numch;
	    i__1 = (numch << 1) - 1;
/* Writing concatenation */
	    *(unsigned char *)&ch__1[0] = i__ + iupper;
	    i__3[0] = 1, a__1[0] = ch__1;
	    i__3[1] = 1, a__1[1] = " ";
	    s_cat(cfile + i__1, a__1, i__3, &c__2, (numch << 1) + 1 - i__1);
	}
/* L330: */
    }

    if (numch <= 1) {
	s_wsfe(&io___25);
	e_wsfe();
	goto L300;
    }
    if (numch > xydata_1.numpts) {
	s_wsfe(&io___26);
	e_wsfe();
	goto L300;
    }
/* .......................................................................
 */
/*  Find out which variable is the 1st data column */

L340:
    s_wsfe(&io___27);
    do_fio(&c__1, cfile, numch << 1);
    e_wsfe();
    s_rsfe(&io___28);
    do_fio(&c__1, chans, 1L);
    e_rsfe();
    if (*(unsigned char *)chans >= 'A' && *(unsigned char *)chans <= 'Z') {
	ich = *(unsigned char *)chans - iupper;
    } else if (*(unsigned char *)chans >= 'a' && *(unsigned char *)chans <= 
	    'z') {
	ich = *(unsigned char *)chans - ilower;
    } else {
	goto L340;
    }
    if (! lchuse[ich - 1]) {
	goto L340;
    }
/* .......................................................................
 */
/*  Assign 1st column variable name to VEctor VARiable, and */
/*  all others to be FiXed Variables;  get initial values for the latter 
*/

    exdata_1.ivevar = ich;
    exdata_1.numpar = numch - 1;
    ich = 0;
    for (i__ = 1; i__ <= 26; ++i__) {
	if (lchuse[i__ - 1] && i__ != exdata_1.ivevar) {
	    ++ich;
	    exdata_1.ifxvar[ich - 1] = i__;

L350:
	    s_wsfe(&io___30);
	    *(unsigned char *)&ch__1[0] = (char) (i__ + iupper);
	    do_fio(&c__1, ch__1, 1L);
	    e_wsfe();
	    ierr = s_rsle(&io___31);
	    if (ierr != 0) {
		goto L100004;
	    }
	    ierr = do_lio(&c__5, &c__1, (char *)&xpar[ich - 1], (ftnlen)
		    sizeof(doublereal));
	    if (ierr != 0) {
		goto L100004;
	    }
	    ierr = e_rsle();
L100004:
	    if (ierr != 0) {
		goto L350;
	    }
	}
/* L360: */
    }
/* .......................................................................
 */
/*  Call the fitting routine */

    iopt = 1;
    m = xydata_1.numpts;
    n = exdata_1.numpar;
    tol = 1e-6;
    nprint = 0;

/*     ************ */
    dnls1e_((U_fp)ff_, &iopt, &m, &n, xpar, fvec, &tol, &nprint, &info, iwork,
	     work, &c_b73);
/*     ************ */

/*  Write results out: */

    s_wsfe(&io___42);
    do_fio(&c__1, "DNLS1E", 6L);
    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
    e_wsfe();
    if (info == 0) {
	goto L9900;
    }

    dcov_((U_fp)ff_, &iopt, &m, &n, xpar, fvec, work, &c__32768, &info, war1, 
	    war2, war3, war4);

    s_wsfe(&io___47);
    do_fio(&c__1, "DCOV", 4L);
    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
    e_wsfe();
    if (info == 0) {
	goto L9900;
    }

    i__1 = exdata_1.numpar;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (info == 1) {
	    xin = sqrt(work[i__ + (i__ << 15) - 32769]);
	    s_wsfe(&io___48);
	    *(unsigned char *)&ch__1[0] = (char) (exdata_1.ifxvar[i__ - 1] + 
		    iupper);
	    do_fio(&c__1, ch__1, 1L);
	    do_fio(&c__1, (char *)&xpar[i__ - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&xin, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	} else {
	    s_wsfe(&io___49);
	    *(unsigned char *)&ch__1[0] = (char) (exdata_1.ifxvar[i__ - 1] + 
		    iupper);
	    do_fio(&c__1, ch__1, 1L);
	    do_fio(&c__1, (char *)&xpar[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
/* L510: */
    }

    xin = 0.;
    i__1 = xydata_1.numpts;
    for (i__ = 1; i__ <= i__1; ++i__) {
	xin += (d__1 = fvec[i__ - 1], abs(d__1));
/* L520: */
    }
    xin /= xydata_1.numpts;
    s_wsfe(&io___50);
    do_fio(&c__1, (char *)&xin, (ftnlen)sizeof(doublereal));
    e_wsfe();
/* .......................................................................
 */
/*  Write fit error curve to a file (if desired) */

L600:
    s_wsfe(&io___51);
    e_wsfe();
    s_rsfe(&io___52);
    do_fio(&c__1, cfile, 64L);
    e_rsfe();
    if (*(unsigned char *)cfile != ' ') {
	o__1.oerr = 1;
	o__1.ounit = 1;
	o__1.ofnmlen = 64;
	o__1.ofnm = cfile;
	o__1.orl = 0;
	o__1.osta = "UNKNOWN";
	o__1.oacc = 0;
	o__1.ofm = "FORMATTED";
	o__1.oblnk = 0;
	ierr = f_open(&o__1);
	if (ierr != 0) {
	    goto L600;
	}

	i__1 = xydata_1.numpts;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_wsfe(&io___53);
	    do_fio(&c__1, (char *)&xydata_1.xx[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&fvec[i__ - 1], (ftnlen)sizeof(doublereal));
	    e_wsfe();
/* L610: */
	}

	cl__1.cerr = 0;
	cl__1.cunit = 1;
	cl__1.csta = 0;
	f_clos(&cl__1);
    }
/* -----------------------------------------------------------------------
 */
L9900:
    return 0;
} /* MAIN__ */




/* Subroutine */ int ff_(integer *iflag, integer *m, integer *n, doublereal *
	x, doublereal *fvec, doublereal *fjac, integer *ldfjac)
{
    /* Format strings */
    static char fmt_101[] = "(\002 --- FF X=\002,5(1x,1pg12.5))";
    static char fmt_999[] = "(\002   *** FF has illegal IFLAG=\002,i5)";

    /* System generated locals */
    integer x_dim1, i__1, i__2;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    static integer i__, j, k;
    static doublereal vaz[851968]	/* was [32768][26] */;
    extern /* Subroutine */ int parevec_(integer *, char *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *, ftnlen);

    /* Fortran I/O blocks */
    static cilist io___54 = { 0, 6, 0, fmt_101, 0 };
    static cilist io___59 = { 0, 6, 0, fmt_999, 0 };



/*  Routine supplied to DNLS1E to compute the functions we are */
/*  are trying to fit. */

/* .......................................................................
 */
/*  Common block XYDATA holds the data read from disk */

/* .......................................................................
 */
/*  Common block EXDATA holds the parsed expression for evaluation */

/* .......................................................................
 */
/*  Local variables */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

    /* Parameter adjustments */
    --fvec;
    x_dim1 = *n;
    --x;

    /* Function Body */
    if (*iflag == 0) {
	s_wsfe(&io___54);
	do_fio(&x_dim1, (char *)&x[1], (ftnlen)sizeof(doublereal));
	e_wsfe();
/* ...................................................................
.... */
    } else if (*iflag == 1) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    k = exdata_1.ifxvar[i__ - 1];
	    i__2 = *m;
	    for (j = 1; j <= i__2; ++j) {
		vaz[j + (k << 15) - 32769] = x[i__];
/* L105: */
	    }
/* L110: */
	}
	i__1 = *m;
	for (j = 1; j <= i__1; ++j) {
	    vaz[j + (exdata_1.ivevar << 15) - 32769] = xydata_1.xx[j - 1];
/* L115: */
	}
	parevec_(&exdata_1.numcod, exdata_1.ccode, vaz, &vaz[32768], &vaz[
		65536], &vaz[98304], &vaz[131072], &vaz[163840], &vaz[196608],
		 &vaz[229376], &vaz[262144], &vaz[294912], &vaz[327680], &vaz[
		360448], &vaz[393216], &vaz[425984], &vaz[458752], &vaz[
		491520], &vaz[524288], &vaz[557056], &vaz[589824], &vaz[
		622592], &vaz[655360], &vaz[688128], &vaz[720896], &vaz[
		753664], &vaz[786432], &vaz[819200], m, &fvec[1], 8L);
	i__1 = *m;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    fvec[i__] -= xydata_1.yy[i__ - 1];
/* L120: */
	}
/* ...................................................................
.... */
    } else {
	s_wsfe(&io___59);
	do_fio(&c__1, (char *)&(*iflag), (ftnlen)sizeof(integer));
	e_wsfe();
	s_stop("", 0L);
    }

    return 0;
} /* ff_ */

/* Main program alias */ int colfit_ () { MAIN__ (); return 0; }
