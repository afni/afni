/* parser.f -- translated by f2c (version 20090411).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lconverted_from_fortran -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lconverted_from_fortran -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static doublereal c_b430 = 0.;
static doublereal c_b444 = 1.;
static doublereal c_b445 = 2.;
static doublereal c_b446 = 3.;
static doublereal c_b447 = 4.;
static doublereal c_b448 = 5.;
static doublereal c_b449 = 6.;
static doublereal c_b450 = 7.;
static doublereal c_b451 = 8.;
static doublereal c_b452 = 9.;
static doublereal c_b453 = 10.;
static doublereal c_b454 = 11.;
static doublereal c_b455 = 12.;

/* Subroutine */ int parser_(char *c_expr__, logical *l_print__, integer *
	num_code__, char *c_code__, ftnlen c_expr_len, ftnlen c_code_len)
{
    /* Initialized data */

    static integer n_funcargs__[122] = { 1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,
	    1,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,2,1,1,1,
	    -1,4,4,4,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,3,3,3,3,3,3,2,2,2,1,-1,-1,
	    2,1,1,1,1,-1,1,-1,-1,-1,1,1,2,1,1,-1,-1,-1,2,5,5,-1,-1,-1,1,3,2,2,
	    1,1,2,-1,-1,-1,-1,-1,-1,3,1,4,2,2 };

    /* Format strings */
    static char fmt_9001[] = "(\002 PARSER error\002,i4,\002: \002,a/1x,a/80"
	    "a1)";

    /* System generated locals */
    address a__1[3];
    integer i__1, i__2[3], i__3;
    static doublereal equiv_0[1];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
#define r8_token__ (equiv_0)
    static integer nextcode;
    static char c_message__[30];
    extern /* Subroutine */ int get_token__(char *, integer *, doublereal *, 
	    integer *, ftnlen);
    static doublereal val_token__;
    static integer nf;
    static char c_ch__[1];
    static integer narg, nlen, nerr, ipos, npos, ncode, nfunc, nused;
    extern integer last_nonblank__(char *, ftnlen);
    static integer n_code__[2048], n_func__[40], ntoken;
    static char c_local__[10000];
    extern /* Subroutine */ int execute_(integer *, char *, ftnlen);
#define c8_token__ ((char *)equiv_0)

    /* Fortran I/O blocks */
    static cilist io___22 = { 0, 6, 0, fmt_9001, 0 };



/*  Parse the arithmetic expression in C_EXPR.  The code required to */
/*  evaluate the expression is returned in the first NUM_CODE entries */
/*  of the CHARACTER*8 array C_CODE. If NUM_CODE is returned as zero, */
/*  an error occurred. On input, L_PRINT determines whether or not to */
/*  print error messages. */

/*  Modified 02/17/89 by RWCox from APEVAL subroutine in APFORT, for PC. */
/*  Modified 06/29/89 by RWCox for Sun Fortran. */
/*  Modified 04/04/91 by RWCox to fix problem with -x**2 type operations. */
/*  Modified 11/20/96 by RWCox to try to control errors in evaluation. */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


/*  Compilation, evaluation, and function stacks. */



/*  Random local stuff */




/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/* ----------------------------------------------------------------------- */
/*  Include file for PARSER.  This file must be kept with PARSER.FOR. */
/*  It defines some symbolic constants that PARSER and its subsidiary */
/*  routines use. */
/* ....................................................................... */
/* Define Token types and values */



/* ....................................................................... */
/*  Define the Nonterminals */


/* ....................................................................... */
/*  Define the Opcodes */


/* ....................................................................... */
/*  Define Function names, etc. */



    /* Parameter adjustments */
    c_code__ -= 8;

    /* Function Body */

/* ----------------------------------------------------------------------- */
    nlen = last_nonblank__(c_expr__, c_expr_len);
    if (nlen <= 0 || nlen > 9999) {
/* !no input, or too much */
	*num_code__ = 0;
	goto L8000;
    }

/*  Copy input string to local, deleting blanks and converting case. */

    npos = 0;
    i__1 = nlen;
    for (ipos = 1; ipos <= i__1; ++ipos) {
	*(unsigned char *)c_ch__ = *(unsigned char *)&c_expr__[ipos - 1];
	if (*(unsigned char *)c_ch__ != ' ') {
	    if (*(unsigned char *)c_ch__ >= 'a' && *(unsigned char *)c_ch__ <=
		     'z') {
		*(unsigned char *)c_ch__ = (char) (*(unsigned char *)c_ch__ + 
			('A' - 'a'));
	    }
/* !convert case */
	    ++npos;
	    *(unsigned char *)&c_local__[npos - 1] = *(unsigned char *)c_ch__;
	}
/* L10: */
    }
/* !tack 1 blank at the end */
    nlen = npos + 1;
    *(unsigned char *)&c_local__[nlen - 1] = ' ';
/* ....................................................................... */
/*  This routine parses expressions according to the grammar: */

/*   EXPR  == E9 E8 E6 E4 $ */

/*   E4    == <addop> E9 E8 E6 E4 | <null> */

/*   E6    == <mulop> E9 E8 E6 | <null> */

/*   E8    == <expop> E9 E8 | <null> */

/*   E9    == <number> | <function> ( E9 E8 E6 E4 ARGTL ) */
/*            | ( E9 E8 E6 E4 ) | <addop> E9 */

/*   ARGTL == , E9 E8 E6 E4 ARGTL | <null> */

/*   <addop>    is + or - */
/*   <mulop>    is * or / */
/*   <expop>    is ** */
/*   <number>   is a literal number or a DCL variable */
/*   <function> is in the list C_FUNCNAME */

/*  The predictive parser described in Aho and Ullman, "Principles of */
/*  Compiler Design" on pages 185-191 for LL(1) grammars is used here, */
/*  with additions to perform the evaluation as the parsing proceeds. */
/*  These consist of adding code (NC_) to the compilation stack when an */
/*  expansion is made.  When the code is popped off the stack, it is */
/*  executed. */

/*  02/17/89:  Now, when code is popped off the stack, it is just */
/*             added to the output code list. */
/* ....................................................................... */
/*  Prepare to process input string.  Initialize the stacks, etc. */

/* !start scan at 1st character */
    npos = 1;
/* !no function calls yet */
    nfunc = 0;
/* !initial compile stack is E9 E8 E6 E4 $ */
    n_code__[0] = 2000;
    n_code__[1] = 2001;
    n_code__[2] = 2002;
    n_code__[3] = 2003;
    n_code__[4] = 2004;
    ncode = 5;
    *num_code__ = 0;
/* ....................................................................... */
/*  1000 is the loop back point to process the next token in the input */
/*  string. */

L1000:
    get_token__(c_local__ + (npos - 1), &ntoken, &val_token__, &nused, nlen - 
	    (npos - 1));

    if (ntoken == 1999) {
	nerr = 1;
	s_copy(c_message__, "Can't interpret symbol", (ftnlen)30, (ftnlen)22);
	goto L9000;
/* !error exit */
    }

/*  At 2000, process the next compile code until the token is used up. */

L2000:
    nextcode = n_code__[ncode - 1];

/*  If next entry on the compile stack is an opcode, then apply it to */
/*  the evaluation stack. */
/*  02/17/89:  just add it to the output */

    if (nextcode >= 3000 && nextcode <= 4999) {
	++(*num_code__);
	execute_(&nextcode, c_code__ + (*num_code__ << 3), (ftnlen)8);
	--ncode;
/* !remove opcode from compile stack */
	goto L2000;
/* !loop back for next compile stack entry */
    }

/*  If next compile stack entry is a token itself, it must match the */
/*  new token from the input. */

    if (nextcode >= 1000 && nextcode <= 1999) {
	if (nextcode == ntoken) {
/* !a match */
	    --ncode;
/* !pop token from compile stack */
	    goto L5000;
/* !loop back for next token */
	}
	nerr = 2;
	if (nextcode == 1004) {
	    *(unsigned char *)c_ch__ = '(';
	} else if (nextcode == 1005) {
	    *(unsigned char *)c_ch__ = ')';
	} else if (nextcode == 1006) {
	    *(unsigned char *)c_ch__ = ',';
	} else {
	    *(unsigned char *)c_ch__ = '?';
	}
/* Writing concatenation */
	i__2[0] = 12, a__1[0] = "Expected a \"";
	i__2[1] = 1, a__1[1] = c_ch__;
	i__2[2] = 1, a__1[2] = "\"";
	s_cat(c_message__, a__1, i__2, &c__3, (ftnlen)30);
	goto L9000;
/* !error exit */
    }

/*  Should have a nonterminal (NN) here. */

    if (nextcode < 2000 || nextcode > 2999) {
	nerr = 3;
	s_copy(c_message__, "Internal parser error", (ftnlen)30, (ftnlen)21);
	goto L9000;
/* !error exit */
    }

/*  Expand the nonterminal appropriately, depending on the token. */
/*  If no legal expansion, then stop with an error. */

/*  TOKEN = end of string */

    if (ntoken == 1000) {
	if (nextcode == 2000) {
/* !end of string = end of expr ==> compilation done */
	    goto L8000;

	} else if (nextcode == 2003 || nextcode == 2002 || nextcode == 2001) {
	    --ncode;
/* !expand this to nothing */
	    goto L2000;
/* !and try this token again */
	}
	nerr = 4;
	s_copy(c_message__, "Unexpected end of input", (ftnlen)30, (ftnlen)23)
		;
	goto L9000;
/* !error exit */
    }

/*  Check if end of input was expected but not encountered. */

    if (nextcode == 2000) {
	nerr = 15;
	s_copy(c_message__, "Expected end of input", (ftnlen)30, (ftnlen)21);
	goto L9000;
/* !error exit */
    }

/*  TOKEN = number or symbol */
/*  02/17/89:  added NT_SYMBOL token type;  now, the code for */
/*             pushing the number or symbol onto the stack is */
/*             added to the output. */

    if (ntoken == 1007 || ntoken == 1009) {
	if (nextcode == 2004) {
/* !only legal time for a number */
	    if (ntoken == 1007) {
		s_copy(c_code__ + (*num_code__ + 1 << 3), "PUSHNUM", (ftnlen)
			8, (ftnlen)7);
	    } else {
		s_copy(c_code__ + (*num_code__ + 1 << 3), "PUSHSYM", (ftnlen)
			8, (ftnlen)7);
	    }
	    *r8_token__ = val_token__;
	    s_copy(c_code__ + (*num_code__ + 2 << 3), c8_token__, (ftnlen)8, (
		    ftnlen)8);
	    *num_code__ += 2;
	    --ncode;
/* !pop E9 from compile stack */
	    goto L5000;
/* !go to next token */
	}
	nerr = 5;
	s_copy(c_message__, "Expected an operator", (ftnlen)30, (ftnlen)20);
	goto L9000;
/* !error exit */
    }

/*  TOKEN = function call */

    if (ntoken == 1008) {
	if (nextcode == 2004) {
/* !only legal time for a function */

	    n_code__[ncode + 6] = 1004;
/* !expand E9 into ( E9 E8 E6 E4 ARGTL ) <func> */
	    n_code__[ncode + 5] = 2004;
	    n_code__[ncode + 4] = 2003;
	    n_code__[ncode + 3] = 2002;
	    n_code__[ncode + 2] = 2001;
	    n_code__[ncode + 1] = 2005;
	    n_code__[ncode] = 1005;
	    n_code__[ncode - 1] = (integer) val_token__ + 4000;
	    ncode += 7;

	    nfunc += 2;
/* !setup function stack to check # arguments */
	    n_func__[nfunc - 2] = (integer) val_token__;
	    n_func__[nfunc - 1] = 0;
	    goto L5000;
/* !process next token */
	}
	nerr = 6;
	s_copy(c_message__, "Expected an operator", (ftnlen)30, (ftnlen)20);
	goto L9000;
/* !error exit */
    }

/*  TOKEN = addition operator */

    if (ntoken == 1001) {
	if (nextcode == 2001) {
/* !expand E4 into E9 E8 E6 <binary addop> E4 */
	    n_code__[ncode + 3] = 2004;
	    n_code__[ncode + 2] = 2003;
	    n_code__[ncode + 1] = 2002;
	    if (val_token__ == 1.) {
		n_code__[ncode] = 3001;
	    } else {
		n_code__[ncode] = 3002;
	    }
	    n_code__[ncode - 1] = 2001;
	    ncode += 4;
	    goto L5000;
/* !process next token */

	} else if (nextcode == 2002 || nextcode == 2003) {
	    --ncode;
/* !expand E6 or E8 to null and try again */
	    goto L2000;

	} else if (nextcode == 2004) {
/* !unary + or - */
	    if (val_token__ == 2.) {
/* !expand E9 into E9 E8 <unary minus> if addop is - otherwise leave E9 alone */
/* [04/04/91 change: */
/*  used to expand to E9 <unary minus>, which makes -x**2 become (-x)**2] */
		n_code__[ncode + 1] = 2004;
		n_code__[ncode] = 2003;
		n_code__[ncode - 1] = 3006;
		ncode += 2;
	    }
	    goto L5000;
/* !process next token */
	}
	nerr = 7;
	s_copy(c_message__, "Illegal arithmetic syntax", (ftnlen)30, (ftnlen)
		25);
	goto L9000;
/* !error exit */
    }

/*  TOKEN = multiplying operator */

    if (ntoken == 1002) {
	if (nextcode == 2002) {
/* !expand E6 into E9 E8 <operator> E6 */
	    n_code__[ncode + 2] = 2004;
	    n_code__[ncode + 1] = 2003;
	    if (val_token__ == 1.) {
		n_code__[ncode] = 3003;
	    } else {
		n_code__[ncode] = 3004;
	    }
	    n_code__[ncode - 1] = 2002;
	    ncode += 3;
	    goto L5000;
/* !next token */

	} else if (nextcode == 2003) {
/* !expand E8 to null and try this token again */
	    --ncode;
	    goto L2000;
	}
	nerr = 8;
	s_copy(c_message__, "Illegal arithmetic syntax", (ftnlen)30, (ftnlen)
		25);
	goto L9000;
/* !error exit */
    }

/*  TOKEN = exponentiation operator */

    if (ntoken == 1003) {
	if (nextcode == 2003) {
/* !expand E8 into E9 E8 <**> */
	    n_code__[ncode + 1] = 2004;
	    n_code__[ncode] = 2003;
	    n_code__[ncode - 1] = 3005;
	    ncode += 2;
	    goto L5000;
/* !process next token */
	}
	nerr = 9;
	s_copy(c_message__, "Illegal arithmetic syntax", (ftnlen)30, (ftnlen)
		25);
	goto L9000;
/* !error exit */
    }

/*  TOKEN = comma */

    if (ntoken == 1006) {
	if (nextcode == 2001 || nextcode == 2002 || nextcode == 2003) {

	    --ncode;
/* !pop this nonterminal and try this token again */
	    goto L2000;

	} else if (nextcode == 2005) {
/* !expand ARGTL into E9 E8 E6 E4 ARGTL */
	    n_code__[ncode + 3] = 2004;
	    n_code__[ncode + 2] = 2003;
	    n_code__[ncode + 1] = 2002;
	    n_code__[ncode] = 2001;
	    n_code__[ncode - 1] = 2005;
	    ncode += 4;
/* !add 1 to no. of args. encountered, and check if there are too many */
	    ++n_func__[nfunc - 1];
	    nf = n_func__[nfunc - 2];
	    if (n_funcargs__[nf - 1] <= n_func__[nfunc - 1] && n_funcargs__[
		    nf - 1] > 0) {
		nerr = 12;
		s_copy(c_message__, "Wrong number of arguments", (ftnlen)30, (
			ftnlen)25);
		goto L9000;
/* !error exit */
	    }
	    goto L5000;
/* !process next token */
	}
	nerr = 10;
	s_copy(c_message__, "Expected an expression", (ftnlen)30, (ftnlen)22);
	goto L9000;
/* !error exit */
    }

/*  TOKEN = open parenthesis */

    if (ntoken == 1004) {
	if (nextcode == 2004) {
/* !expand E9 into E9 E8 E6 E4 ) */
	    n_code__[ncode + 3] = 2004;
	    n_code__[ncode + 2] = 2003;
	    n_code__[ncode + 1] = 2002;
	    n_code__[ncode] = 2001;
	    n_code__[ncode - 1] = 1005;
	    ncode += 4;
	    goto L5000;
/* !process next token */
	}
	nerr = 11;
	s_copy(c_message__, "Expected an operator", (ftnlen)30, (ftnlen)20);
	goto L9000;
/* !error exit */
    }

/*  TOKEN = close parenthesis */

    if (ntoken == 1005) {
	if (nextcode == 2001 || nextcode == 2002 || nextcode == 2003) {

	    --ncode;
/* !pop this nonterminal and try this token out on the next one */
	    goto L2000;

	} else if (nextcode == 2005) {
/* !end of function call */

	    narg = n_func__[nfunc - 1] + 1;
/* !check # arguments */
	    nf = n_func__[nfunc - 2];
	    nfunc += -2;
	    if (n_funcargs__[nf - 1] <= 0) {
/* !variable # of args ==> push number of args on stack (Feb 1997) */
		s_copy(c_code__ + (*num_code__ + 1 << 3), "PUSHNUM", (ftnlen)
			8, (ftnlen)7);
		*r8_token__ = (doublereal) narg;
		s_copy(c_code__ + (*num_code__ + 2 << 3), c8_token__, (ftnlen)
			8, (ftnlen)8);
		*num_code__ += 2;
	    } else if (n_funcargs__[nf - 1] != narg) {
/* !illegal # of args */
		nerr = 12;
		s_copy(c_message__, "Wrong number of arguments", (ftnlen)30, (
			ftnlen)25);
		goto L9000;
/* !error exit */
	    }

	    --ncode;
/* !pop this nonterminal and try to match the ) with the next compile stack entry */
	    goto L2000;
	}
	nerr = 13;
	s_copy(c_message__, "Expected an expression", (ftnlen)30, (ftnlen)22);
	goto L9000;
/* !error exit */
    }
    nerr = 14;
    s_copy(c_message__, "Internal parser error", (ftnlen)30, (ftnlen)21);
    goto L9000;
/* !error exit */
/* ....................................................................... */
/*  At 5000, advance to the next token and loop back */

L5000:
    npos += nused;
    goto L1000;
/* ....................................................................... */
/*  At 8000, exit */

L8000:
    return 0;
/* ....................................................................... */
/*  At 9000, error exit */

L9000:
    if (*l_print__) {
	if (nused < 1) {
	    nused = 1;
	}
	s_wsfe(&io___22);
	do_fio(&c__1, (char *)&nerr, (ftnlen)sizeof(integer));
	do_fio(&c__1, c_message__, (ftnlen)30);
	do_fio(&c__1, c_local__, nlen);
	i__1 = npos;
	for (nf = 1; nf <= i__1; ++nf) {
	    do_fio(&c__1, ".", (ftnlen)1);
	}
	i__3 = nused;
	for (nf = 1; nf <= i__3; ++nf) {
	    do_fio(&c__1, "#", (ftnlen)1);
	}
	e_wsfe();

/* CC         WRITE(*,9002) (N_CODE(NF),NF=NCODE,1,-1) */
/* CC9002     FORMAT(' Compile stack is (top down)' / 10(1X,I6) ) */
    }

    *num_code__ = 0;
    return 0;
} /* parser_ */

#undef c8_token__
#undef r8_token__





/* Subroutine */ int execute_(integer *n_opcode__, char *c_code__, ftnlen 
	c_code_len)
{
    /* Initialized data */

    static char c_funcname__[32*123] = "SIN                             " 
	    "COS                             " "TAN                         "
	    "    " "ASIN                            " "ACOS                  "
	    "          " "ATAN                            " "ATAN2           "
	    "                " "SINH                            " "COSH      "
	    "                      " "TANH                            " "ASIN"
	    "H                           " "ACOSH                           " 
	    "ATANH                           " "EXP                         "
	    "    " "LOG                             " "LOG10                 "
	    "          " "ABS                             " "INT             "
	    "                " "SQRT                            " "MAX       "
	    "                      " "MIN                             " "AI  "
	    "                            " "DAI                             " 
	    "I0                              " "I1                          "
	    "    " "J0                              " "J1                    "
	    "          " "K0                              " "K1              "
	    "                " "Y0                              " "Y1        "
	    "                      " "BI                              " "DBI "
	    "                            " "ERF                             " 
	    "ERFC                            " "GAMMA                       "
	    "    " "QG                              " "QGINV                 "
	    "          " "BELL2                           " "RECT            "
	    "                " "STEP                            " "BOOL      "
	    "                      " "AND                             " "OR  "
	    "                            " "MOFN                            " 
	    "ASTEP                           " "SIND                        "
	    "    " "COSD                            " "TAND                  "
	    "          " "MEDIAN                          " "FICO_T2P        "
	    "                " "FICO_P2T                        " "FICO_T2Z  "
	    "                      " "FITT_T2P                        " "FITT"
	    "_P2T                        " "FITT_T2Z                        " 
	    "FIFT_T2P                        " "FIFT_P2T                    "
	    "    " "FIFT_T2Z                        " "FIZT_T2P              "
	    "          " "FIZT_P2T                        " "FIZT_T2Z        "
	    "                " "FICT_T2P                        " "FICT_P2T  "
	    "                      " "FICT_T2Z                        " "FIBT"
	    "_T2P                        " "FIBT_P2T                        " 
	    "FIBT_T2Z                        " "FIBN_T2P                    "
	    "    " "FIBN_P2T                        " "FIBN_T2Z              "
	    "          " "FIGT_T2P                        " "FIGT_P2T        "
	    "                " "FIGT_T2Z                        " "FIPT_T2P  "
	    "                      " "FIPT_P2T                        " "FIPT"
	    "_T2Z                        " "ZTONE                           " 
	    "LMODE                           " "HMODE                       "
	    "    " "GRAN                            " "URAN                  "
	    "          " "IRAN                            " "ERAN            "
	    "                " "LRAN                            " "ORSTAT    "
	    "                      " "TENT                            " "MAD "
	    "                            " "ARGMAX                          " 
	    "ARGNUM                          " "NOTZERO                     "
	    "    " "ISZERO                          " "EQUALS                "
	    "          " "ISPOSITIVE                      " "ISNEGATIVE      "
	    "                " "MEAN                            " "STDEV     "
	    "                      " "SEM                             " "PLEG"
	    "                            " "CDF2STAT                        " 
	    "STAT2CDF                        " "PAIRMAX                     "
	    "    " "PAIRMIN                         " "AMONGST               "
	    "          " "CBRT                            " "RHDDC2          "
	    "                " "HRFBK4                          " "HRFBK5    "
	    "                      " "POSVAL                          " "NOT "
	    "                            " "MOD                             " 
	    "WITHIN                          " "MINABOVE                    "
	    "    " "MAXBELOW                        " "EXTREME               "
	    "          " "ABSEXTREME                      " "CHOOSE          "
	    "                " "IFELSE                          " "LOGCOSH   "
	    "                      " "ACFWXM                          " "GAMP"
	    "                            " "GAMQ                            " 
	    "DUMMY                           ";

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);


/*  Execute the opcode on the evaluation stack.  Note that no attempt is */
/*  made to intercept errors, such as divide by zero, ACOS(2), etc. */


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*  Branch to special code for function evaluations */

/* ----------------------------------------------------------------------- */
/*  Include file for PARSER.  This file must be kept with PARSER.FOR. */
/*  It defines some symbolic constants that PARSER and its subsidiary */
/*  routines use. */
/* ....................................................................... */
/* Define Token types and values */



/* ....................................................................... */
/*  Define the Nonterminals */


/* ....................................................................... */
/*  Define the Opcodes */


/* ....................................................................... */
/*  Define Function names, etc. */




/* ----------------------------------------------------------------------- */
    if (*n_opcode__ >= 4000) {
	goto L5000;
    }
/* ....................................................................... */
    if (*n_opcode__ == 3006) {
/* !unary minus, the only unary op. */
	s_copy(c_code__, "--", (ftnlen)8, (ftnlen)2);

    } else {
/* !a binary operation */
	if (*n_opcode__ == 3001) {
/* !add */
	    s_copy(c_code__, "+", (ftnlen)8, (ftnlen)1);
	} else if (*n_opcode__ == 3002) {
/* !subtract */
	    s_copy(c_code__, "-", (ftnlen)8, (ftnlen)1);
	} else if (*n_opcode__ == 3003) {
/* !multiply */
	    s_copy(c_code__, "*", (ftnlen)8, (ftnlen)1);
	} else if (*n_opcode__ == 3004) {
/* !divide */
	    s_copy(c_code__, "/", (ftnlen)8, (ftnlen)1);
	} else if (*n_opcode__ == 3005) {
/* !** */
	    s_copy(c_code__, "**", (ftnlen)8, (ftnlen)2);
	}
    }
    goto L8000;
/* ....................................................................... */
/*  Function evaluation */

L5000:
    s_copy(c_code__, c_funcname__ + (*n_opcode__ - 4001 << 5), (ftnlen)8, (
	    ftnlen)32);
/* ....................................................................... */
L8000:
    return 0;
} /* execute_ */




/* Subroutine */ int get_token__(char *c_input__, integer *ntype, doublereal *
	value, integer *nused, ftnlen c_input_len)
{
    /* Initialized data */

    static char c_funcname__[32*123] = "SIN                             " 
	    "COS                             " "TAN                         "
	    "    " "ASIN                            " "ACOS                  "
	    "          " "ATAN                            " "ATAN2           "
	    "                " "SINH                            " "COSH      "
	    "                      " "TANH                            " "ASIN"
	    "H                           " "ACOSH                           " 
	    "ATANH                           " "EXP                         "
	    "    " "LOG                             " "LOG10                 "
	    "          " "ABS                             " "INT             "
	    "                " "SQRT                            " "MAX       "
	    "                      " "MIN                             " "AI  "
	    "                            " "DAI                             " 
	    "I0                              " "I1                          "
	    "    " "J0                              " "J1                    "
	    "          " "K0                              " "K1              "
	    "                " "Y0                              " "Y1        "
	    "                      " "BI                              " "DBI "
	    "                            " "ERF                             " 
	    "ERFC                            " "GAMMA                       "
	    "    " "QG                              " "QGINV                 "
	    "          " "BELL2                           " "RECT            "
	    "                " "STEP                            " "BOOL      "
	    "                      " "AND                             " "OR  "
	    "                            " "MOFN                            " 
	    "ASTEP                           " "SIND                        "
	    "    " "COSD                            " "TAND                  "
	    "          " "MEDIAN                          " "FICO_T2P        "
	    "                " "FICO_P2T                        " "FICO_T2Z  "
	    "                      " "FITT_T2P                        " "FITT"
	    "_P2T                        " "FITT_T2Z                        " 
	    "FIFT_T2P                        " "FIFT_P2T                    "
	    "    " "FIFT_T2Z                        " "FIZT_T2P              "
	    "          " "FIZT_P2T                        " "FIZT_T2Z        "
	    "                " "FICT_T2P                        " "FICT_P2T  "
	    "                      " "FICT_T2Z                        " "FIBT"
	    "_T2P                        " "FIBT_P2T                        " 
	    "FIBT_T2Z                        " "FIBN_T2P                    "
	    "    " "FIBN_P2T                        " "FIBN_T2Z              "
	    "          " "FIGT_T2P                        " "FIGT_P2T        "
	    "                " "FIGT_T2Z                        " "FIPT_T2P  "
	    "                      " "FIPT_P2T                        " "FIPT"
	    "_T2Z                        " "ZTONE                           " 
	    "LMODE                           " "HMODE                       "
	    "    " "GRAN                            " "URAN                  "
	    "          " "IRAN                            " "ERAN            "
	    "                " "LRAN                            " "ORSTAT    "
	    "                      " "TENT                            " "MAD "
	    "                            " "ARGMAX                          " 
	    "ARGNUM                          " "NOTZERO                     "
	    "    " "ISZERO                          " "EQUALS                "
	    "          " "ISPOSITIVE                      " "ISNEGATIVE      "
	    "                " "MEAN                            " "STDEV     "
	    "                      " "SEM                             " "PLEG"
	    "                            " "CDF2STAT                        " 
	    "STAT2CDF                        " "PAIRMAX                     "
	    "    " "PAIRMIN                         " "AMONGST               "
	    "          " "CBRT                            " "RHDDC2          "
	    "                " "HRFBK4                          " "HRFBK5    "
	    "                      " "POSVAL                          " "NOT "
	    "                            " "MOD                             " 
	    "WITHIN                          " "MINABOVE                    "
	    "    " "MAXBELOW                        " "EXTREME               "
	    "          " "ABSEXTREME                      " "CHOOSE          "
	    "                " "IFELSE                          " "LOGCOSH   "
	    "                      " "ACFWXM                          " "GAMP"
	    "                            " "GAMQ                            " 
	    "DUMMY                           ";

    /* Format strings */
    static char fmt_5501[] = "(\002(F\002,i1,\002.0)\002)";
    static char fmt_5502[] = "(\002(F\002,i2,\002.0)\002)";

    /* System generated locals */
    char ch__1[1];
    icilist ici__1;
    static doublereal equiv_0[1];

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfi(icilist *), do_fio(integer *, char *, ftnlen), e_wsfi(void)
	    , s_rsfi(icilist *), e_rsfi(void);

    /* Local variables */
    static char c_id__[32];
    static integer nlen, ipos, npos;
    static char c_val__[32];
    static integer ifunc;
#define c8_val__ ((char *)equiv_0)
#define r8_val__ (equiv_0)
    static integer io_code__;
    static char c_first__[1];

    /* Fortran I/O blocks */
    static icilist io___36 = { 0, c_val__, 0, fmt_5501, 32, 1 };
    static icilist io___37 = { 0, c_val__, 0, fmt_5502, 32, 1 };



/*  Return the 1st token in the input stream. */




/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*  Statement function definitions */

/* ----------------------------------------------------------------------- */
/*  Include file for PARSER.  This file must be kept with PARSER.FOR. */
/*  It defines some symbolic constants that PARSER and its subsidiary */
/*  routines use. */
/* ....................................................................... */
/* Define Token types and values */



/* ....................................................................... */
/*  Define the Nonterminals */


/* ....................................................................... */
/*  Define the Opcodes */


/* ....................................................................... */
/*  Define Function names, etc. */




/* ----------------------------------------------------------------------- */


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

    *ntype = 1000;
    *nused = 0;
    nlen = i_len(c_input__, c_input_len);
    if (nlen <= 0) {
	goto L8000;
    }

/*  Process the simple cases 1st */

    *(unsigned char *)c_first__ = *(unsigned char *)c_input__;

    if (*(unsigned char *)c_first__ == ' ') {
	goto L8000;
    }

    *nused = 1;
    if (*(unsigned char *)c_first__ == '+') {
	*ntype = 1001;
	*value = 1.;
    } else if (*(unsigned char *)c_first__ == '-') {
	*ntype = 1001;
	*value = 2.;
    } else if (*(unsigned char *)c_first__ == '/') {
	*ntype = 1002;
	*value = 2.;
    } else if (*(unsigned char *)c_first__ == '*') {
	if (s_cmp(c_input__, "**", (ftnlen)2, (ftnlen)2) == 0) {
	    *ntype = 1003;
	    *value = 1.;
	    *nused = 2;
	} else {
	    *ntype = 1002;
	    *value = 1.;
	}
    } else if (*(unsigned char *)c_first__ == '^') {
	*ntype = 1003;
	*value = 1.;
    } else if (*(unsigned char *)c_first__ == '(' || *(unsigned char *)
	    c_first__ == '[') {
	*ntype = 1004;
    } else if (*(unsigned char *)c_first__ == ')' || *(unsigned char *)
	    c_first__ == ']') {
	*ntype = 1005;
    } else if (*(unsigned char *)c_first__ == ',') {
	*ntype = 1006;
    }

    if (*ntype != 1000) {
	goto L8000;
    }
/* !exit if above was successful */
/* ....................................................................... */
/*  The only possibilities left are a variable name, a function name, */
/*  or a number. */

    *(unsigned char *)&ch__1[0] = *(unsigned char *)c_first__;
    if (*(unsigned char *)&ch__1[0] >= 'A' && *(unsigned char *)&ch__1[0] <= 
	    'Z') {
/* !a name */

	npos = 2;
L110:
	*(unsigned char *)&ch__1[0] = *(unsigned char *)&c_input__[npos - 1];
	if (! (*(unsigned char *)&ch__1[0] >= 'A' && *(unsigned char *)&ch__1[
		0] <= 'Z' || *(unsigned char *)&ch__1[0] >= '0' && *(unsigned 
		char *)&ch__1[0] <= '9' || *(unsigned char *)&ch__1[0] == '_' 
		|| *(unsigned char *)&ch__1[0] == '$')) {
	    goto L120;
	}
	++npos;
	goto L110;
L120:
	--npos;
	s_copy(c_id__, c_input__, (ftnlen)32, npos);

/*  The name is now in C_ID.  Check to see if it is a function name. */

	ifunc = 1;
	s_copy(c_funcname__ + 3904, c_id__, (ftnlen)32, (ftnlen)32);
L210:
	if (! (s_cmp(c_id__, c_funcname__ + (ifunc - 1 << 5), (ftnlen)32, (
		ftnlen)32) != 0)) {
	    goto L220;
	}
	++ifunc;
	goto L210;
L220:
	if (ifunc <= 122) {
/* !it is a function */
	    *ntype = 1008;
	    *value = (doublereal) ifunc;
	    *nused = npos;
	} else if (s_cmp(c_id__, "PI", npos, (ftnlen)2) == 0) {
/* !symbolic pi */
	    *ntype = 1007;
	    *value = 3.1415926535897932;
	    *nused = npos;
	} else {
/* !must be a symbol */
	    *ntype = 1009;
	    s_copy(c8_val__, c_id__, (ftnlen)8, npos);
	    *value = *r8_val__;
	    *nused = npos;
	}
/* ....................................................................... */
/*  try for a number */

    } else /* if(complicated condition) */ {
	*(unsigned char *)&ch__1[0] = *(unsigned char *)c_first__;
	if (*(unsigned char *)&ch__1[0] >= '0' && *(unsigned char *)&ch__1[0] 
		<= '9' || *(unsigned char *)c_first__ == '.') {
	    npos = 2;
L310:
	    *(unsigned char *)&ch__1[0] = *(unsigned char *)&c_input__[npos - 
		    1];
	    if (! (*(unsigned char *)&ch__1[0] >= '0' && *(unsigned char *)&
		    ch__1[0] <= '9')) {
		goto L320;
	    }
/* !skip digits */
	    ++npos;
	    goto L310;
L320:
	    if (*(unsigned char *)c_first__ != '.' && *(unsigned char *)&
		    c_input__[npos - 1] == '.') {
		++npos;
L410:
		*(unsigned char *)&ch__1[0] = *(unsigned char *)&c_input__[
			npos - 1];
		if (! (*(unsigned char *)&ch__1[0] >= '0' && *(unsigned char *
			)&ch__1[0] <= '9')) {
		    goto L420;
		}
/* !skip digits after decimal pt */
		++npos;
		goto L410;
L420:
		;
	    }
/* !allow for exponent */
	    if (*(unsigned char *)&c_input__[npos - 1] == 'E' || *(unsigned 
		    char *)&c_input__[npos - 1] == 'D') {
		ipos = npos + 1;
		if (*(unsigned char *)&c_input__[ipos - 1] == '+' || *(
			unsigned char *)&c_input__[ipos - 1] == '-') {
		    ++ipos;
		}
		*(unsigned char *)&ch__1[0] = *(unsigned char *)&c_input__[
			ipos - 1];
		if (*(unsigned char *)&ch__1[0] >= '0' && *(unsigned char *)&
			ch__1[0] <= '9') {
/* !only if a digit follows the E can it be legal */
		    npos = ipos;
L510:
		    *(unsigned char *)&ch__1[0] = *(unsigned char *)&
			    c_input__[npos - 1];
		    if (! (*(unsigned char *)&ch__1[0] >= '0' && *(unsigned 
			    char *)&ch__1[0] <= '9')) {
			goto L520;
		    }
		    ++npos;
		    goto L510;
L520:
		    ;
		}
	    }
	    --npos;
/* !number runs from position 1 to NPOS */
	    *nused = npos;
	    if (npos <= 9) {
		s_wsfi(&io___36);
		do_fio(&c__1, (char *)&npos, (ftnlen)sizeof(integer));
		e_wsfi();
	    } else {
		s_wsfi(&io___37);
		do_fio(&c__1, (char *)&npos, (ftnlen)sizeof(integer));
		e_wsfi();
	    }
	    ici__1.icierr = 1;
	    ici__1.iciend = 1;
	    ici__1.icirnum = 1;
	    ici__1.icirlen = npos;
	    ici__1.iciunit = c_input__;
	    ici__1.icifmt = c_val__;
	    io_code__ = s_rsfi(&ici__1);
	    if (io_code__ != 0) {
		goto L100001;
	    }
	    io_code__ = do_fio(&c__1, (char *)&(*value), (ftnlen)sizeof(
		    doublereal));
	    if (io_code__ != 0) {
		goto L100001;
	    }
	    io_code__ = e_rsfi();
L100001:

/* CC         WRITE(*,5509) C_INPUT(1:NPOS) , C_VAL , VALUE */
/* CC5509     FORMAT( */
/* CC     X     ' scanned text ',A/ */
/* CC     X     ' using format ',A/ */
/* CC     X     ' giving VALUE ',1PG14.7) */

	    if (io_code__ == 0) {
		*ntype = 1007;
	    } else {
		*ntype = 1999;
	    }
/* ....................................................................... */
/*  If not a number, an error! */

	} else {
	    *ntype = 1999;
	    *nused = 1;
	}
    }
/* ....................................................................... */
L8000:
    return 0;
} /* get_token__ */

#undef r8_val__
#undef c8_val__





/* (((.................................................................... */
integer last_nonblank__(char *cline, ftnlen cline_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    static integer npos;


/*  Return the position of the last nonblank character in the input */
/*  character string.  CLINE is CHARACTER*(*).  Even if CLINE is all */
/*  blanks, LAST_NONBLANK will be returned as 1 so that operations of the */
/*  form CLINE(1:LAST_NONBLANK) won't be garbage. */
/* ))).................................................................... */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*  Start at the end and work backwards until a nonblank is found. */
/*  Loop back to 100 to check position # NPOS each time. */

    npos = i_len(cline, cline_len);
L100:
/*  quit if at the beginning */
    if (npos <= 1) {
	goto L200;
    }
/*  quit if not a blank or a null */
    if (*(unsigned char *)&cline[npos - 1] != ' ' && *(unsigned char *)&cline[
	    npos - 1] != '\0') {
	goto L200;
    }
/*  move back one position and try again */
    --npos;
    goto L100;
/* ....................................................................... */
L200:
    ret_val = npos;
    return ret_val;
} /* last_nonblank__ */




integer hassym_(char *sym, integer *num_code__, char *c_code__, ftnlen 
	sym_len, ftnlen c_code_len)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char sss[1];
    static integer ncode;



/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

    /* Parameter adjustments */
    c_code__ -= 8;

    /* Function Body */
    ret_val = 0;
    if (*num_code__ <= 0) {
	return ret_val;
    }
    *(unsigned char *)sss = *(unsigned char *)sym;

    i__1 = *num_code__;
    for (ncode = 1; ncode <= i__1; ++ncode) {
	if (s_cmp(c_code__ + (ncode << 3), "PUSHSYM", (ftnlen)8, (ftnlen)7) ==
		 0) {
	    if (*(unsigned char *)&c_code__[(ncode + 1) * 8] == *(unsigned 
		    char *)sss) {
		ret_val = 1;
		return ret_val;
	    }
	}
/* L1000: */
    }

    return ret_val;
} /* hassym_ */




doublereal pareval_(integer *num_code__, char *c_code__, doublereal *r8val, 
	ftnlen c_code_len)
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2;
    static doublereal equiv_0[1];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    double d_int(doublereal *), pow_dd(doublereal *, doublereal *), sin(
	    doublereal), cos(doublereal), tan(doublereal), sqrt(doublereal), 
	    exp(doublereal), log(doublereal), d_lg10(doublereal *), asin(
	    doublereal), acos(doublereal), atan(doublereal), atan2(doublereal,
	     doublereal), sinh(doublereal), cosh(doublereal), tanh(doublereal)
	    ;

    /* Local variables */
    extern doublereal legendre_(doublereal *, doublereal *), minabove_(
	    integer *, doublereal *), maxbelow_(integer *, doublereal *);
    static doublereal x, y;
    extern doublereal qg_(doublereal *), absextreme_(integer *, doublereal *),
	     dai_(doublereal *), dbi_(doublereal *, integer *), mad_(integer *
	    , doublereal *), sem_(integer *, doublereal *);
    static integer itm;
    extern doublereal lor_(integer *, doublereal *);
    static integer ntm;
    extern doublereal land_(integer *, doublereal *), mean_(integer *, 
	    doublereal *), derf_(doublereal *), gamp_(doublereal *, 
	    doublereal *), eran_(doublereal *), gamq_(doublereal *, 
	    doublereal *), gran_(doublereal *, doublereal *), iran_(
	    doublereal *), bool_(doublereal *), lran_(doublereal *), rect_(
	    doublereal *), uran_(doublereal *), tent_(doublereal *), step_(
	    doublereal *), bell2_(doublereal *), derfc_(doublereal *);
    static integer ncode;
    extern doublereal hmode_(integer *, doublereal *), lmode_(integer *, 
	    doublereal *);
    static integer neval;
    extern doublereal lmofn_(integer *, integer *, doublereal *), qginv_(
	    doublereal *), stdev_(integer *, doublereal *), ztone_(doublereal 
	    *), zzmod_(doublereal *, doublereal *), dbesi0_(doublereal *), 
	    dbesi1_(doublereal *), dbesj0_(doublereal *), dbesj1_(doublereal *
	    ), dbesk0_(doublereal *), dbesk1_(doublereal *);
#define c8_val__ ((char *)equiv_0)
    extern doublereal rhddc2_(doublereal *, doublereal *, doublereal *), 
	    hrfbk4_(doublereal *, doublereal *), hrfbk5_(doublereal *, 
	    doublereal *), cdf2st_(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), dbesy0_(doublereal *), dbesy1_(
	    doublereal *), st2cdf_(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);
#define r8_val__ (equiv_0)
    extern doublereal dgamma_(doublereal *);
    static char cncode[8];
    extern doublereal median_(integer *, doublereal *);
    static integer ialpha;
    extern doublereal cbrtff_(doublereal *), amongf_(integer *, doublereal *),
	     argmax_(integer *, doublereal *), choose_(integer *, integer *, 
	    doublereal *), fibntp_(doublereal *, doublereal *, doublereal *), 
	    fibnpt_(doublereal *, doublereal *, doublereal *), ficotp_(
	    doublereal *, doublereal *, doublereal *, doublereal *), acfwxm_(
	    doublereal *, doublereal *, doublereal *, doublereal *), pairmn_(
	    integer *, doublereal *), lncosh_(doublereal *), ficopt_(
	    doublereal *, doublereal *, doublereal *, doublereal *), argnum_(
	    integer *, doublereal *), ficttp_(doublereal *, doublereal *), 
	    fictpt_(doublereal *, doublereal *), fifttp_(doublereal *, 
	    doublereal *, doublereal *), fiftpt_(doublereal *, doublereal *, 
	    doublereal *), ficotz_(doublereal *, doublereal *, doublereal *, 
	    doublereal *), fibttp_(doublereal *, doublereal *, doublereal *), 
	    pairmx_(integer *, doublereal *), fibtpt_(doublereal *, 
	    doublereal *, doublereal *), fibttz_(doublereal *, doublereal *, 
	    doublereal *), ficttz_(doublereal *, doublereal *), posval_(
	    doublereal *), fibntz_(doublereal *, doublereal *, doublereal *), 
	    fifttz_(doublereal *, doublereal *, doublereal *), figttp_(
	    doublereal *, doublereal *, doublereal *), figtpt_(doublereal *, 
	    doublereal *, doublereal *), figttz_(doublereal *, doublereal *, 
	    doublereal *), fitttp_(doublereal *, doublereal *), fittpt_(
	    doublereal *, doublereal *), orstat_(integer *, integer *, 
	    doublereal *), fipttp_(doublereal *, doublereal *), fiptpt_(
	    doublereal *, doublereal *), fizttp_(doublereal *), fiztpt_(
	    doublereal *), fipttz_(doublereal *, doublereal *), fitttz_(
	    doublereal *, doublereal *), fizttz_(doublereal *);
    static doublereal r8_eval__[128];
    extern doublereal withinf_(integer *, doublereal *), extreme_(integer *, 
	    doublereal *);





/*  Internal library functions */


/*  External library functions */


/*  Statistics functions (01 Mar 1999 - see parser_int.c) */


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

    /* Parameter adjustments */
    --r8val;
    c_code__ -= 8;

    /* Function Body */
    if (*num_code__ <= 0) {
	ret_val = 0.;
	goto L8000;
    }
/* ----------------------------------------------------------------------- */
    ialpha = 'A' - 1;
    neval = 0;
    ncode = 0;

L1000:
    ++ncode;
    s_copy(cncode, c_code__ + (ncode << 3), (ftnlen)8, (ftnlen)8);
/* ....................................................................... */
    if (s_cmp(cncode, "PUSHSYM", (ftnlen)8, (ftnlen)7) == 0) {
	++neval;
	r8_eval__[neval - 1] = r8val[*(unsigned char *)&c_code__[(ncode + 1) *
		 8] - ialpha];
	++ncode;
/* ....................................................................... */
    } else if (s_cmp(cncode, "PUSHNUM", (ftnlen)8, (ftnlen)7) == 0) {
	++neval;
	s_copy(c8_val__, c_code__ + (ncode + 1 << 3), (ftnlen)8, (ftnlen)8);
	r8_eval__[neval - 1] = *r8_val__;
	++ncode;
/* ....................................................................... */
    } else if (s_cmp(cncode, "+", (ftnlen)8, (ftnlen)1) == 0) {
	--neval;
	r8_eval__[neval - 1] += r8_eval__[neval];
/* ....................................................................... */
    } else if (s_cmp(cncode, "-", (ftnlen)8, (ftnlen)1) == 0) {
	--neval;
	r8_eval__[neval - 1] -= r8_eval__[neval];
/* ....................................................................... */
    } else if (s_cmp(cncode, "*", (ftnlen)8, (ftnlen)1) == 0) {
	--neval;
	r8_eval__[neval - 1] *= r8_eval__[neval];
/* ....................................................................... */
    } else if (s_cmp(cncode, "/", (ftnlen)8, (ftnlen)1) == 0) {
	--neval;
	if (r8_eval__[neval] != 0.) {
	    r8_eval__[neval - 1] /= r8_eval__[neval];
	} else {
	    r8_eval__[neval - 1] = 0.;
	}
/* ....................................................................... */
    } else if (s_cmp(cncode, "**", (ftnlen)8, (ftnlen)2) == 0) {
	--neval;
	if (r8_eval__[neval - 1] > 0. || r8_eval__[neval - 1] != 0. && 
		r8_eval__[neval] == d_int(&r8_eval__[neval])) {
	    r8_eval__[neval - 1] = pow_dd(&r8_eval__[neval - 1], &r8_eval__[
		    neval]);
	}
/* ....................................................................... */
    } else if (s_cmp(cncode, "--", (ftnlen)8, (ftnlen)2) == 0) {
	r8_eval__[neval - 1] = -r8_eval__[neval - 1];
/* ....................................................................... */
    } else if (s_cmp(cncode, "SIN", (ftnlen)8, (ftnlen)3) == 0) {
	r8_eval__[neval - 1] = sin(r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "SIND", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = sin(r8_eval__[neval - 1] * .01745329251994);
/* ....................................................................... */
    } else if (s_cmp(cncode, "COS", (ftnlen)8, (ftnlen)3) == 0) {
	r8_eval__[neval - 1] = cos(r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "COSD", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = cos(r8_eval__[neval - 1] * .01745329251994);
/* ....................................................................... */
    } else if (s_cmp(cncode, "TAN", (ftnlen)8, (ftnlen)3) == 0) {
	r8_eval__[neval - 1] = tan(r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "TAND", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = tan(r8_eval__[neval - 1] * .01745329251994);
/* ....................................................................... */
    } else if (s_cmp(cncode, "SQRT", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = sqrt((d__1 = r8_eval__[neval - 1], abs(d__1)));
/* ....................................................................... */
    } else if (s_cmp(cncode, "CBRT", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = cbrtff_(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "ABS", (ftnlen)8, (ftnlen)3) == 0) {
	r8_eval__[neval - 1] = (d__1 = r8_eval__[neval - 1], abs(d__1));
/* ....................................................................... */
    } else if (s_cmp(cncode, "EXP", (ftnlen)8, (ftnlen)3) == 0) {
/* Computing MIN */
	d__1 = 87.5, d__2 = r8_eval__[neval - 1];
	r8_eval__[neval - 1] = exp((min(d__1,d__2)));
/* ....................................................................... */
    } else if (s_cmp(cncode, "LOG", (ftnlen)8, (ftnlen)3) == 0) {
	if (r8_eval__[neval - 1] != 0.) {
	    r8_eval__[neval - 1] = log((d__1 = r8_eval__[neval - 1], abs(d__1)
		    ));
	}
/* ....................................................................... */
    } else if (s_cmp(cncode, "LOG10", (ftnlen)8, (ftnlen)5) == 0) {
	if (r8_eval__[neval - 1] != 0.) {
	    d__2 = (d__1 = r8_eval__[neval - 1], abs(d__1));
	    r8_eval__[neval - 1] = d_lg10(&d__2);
	}
/* ....................................................................... */
    } else if (s_cmp(cncode, "INT", (ftnlen)8, (ftnlen)3) == 0) {
	r8_eval__[neval - 1] = d_int(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "MAX", (ftnlen)8, (ftnlen)3) == 0) {
	--neval;
/* Computing MAX */
	d__1 = r8_eval__[neval - 1], d__2 = r8_eval__[neval];
	r8_eval__[neval - 1] = max(d__1,d__2);
/* ....................................................................... */
    } else if (s_cmp(cncode, "MIN", (ftnlen)8, (ftnlen)3) == 0) {
	--neval;
/* Computing MIN */
	d__1 = r8_eval__[neval - 1], d__2 = r8_eval__[neval];
	r8_eval__[neval - 1] = min(d__1,d__2);
/* ....................................................................... */
    } else if (s_cmp(cncode, "ASIN", (ftnlen)8, (ftnlen)4) == 0) {
	if ((d__1 = r8_eval__[neval - 1], abs(d__1)) <= 1.) {
	    r8_eval__[neval - 1] = asin(r8_eval__[neval - 1]);
	}
/* ....................................................................... */
    } else if (s_cmp(cncode, "ACOS", (ftnlen)8, (ftnlen)4) == 0) {
	if ((d__1 = r8_eval__[neval - 1], abs(d__1)) <= 1.) {
	    r8_eval__[neval - 1] = acos(r8_eval__[neval - 1]);
	}
/* ....................................................................... */
    } else if (s_cmp(cncode, "ATAN", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = atan(r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "ATAN2", (ftnlen)8, (ftnlen)5) == 0) {
	--neval;
	if (r8_eval__[neval - 1] != 0. || r8_eval__[neval] != 0.) {
	    r8_eval__[neval - 1] = atan2(r8_eval__[neval - 1], r8_eval__[
		    neval]);
	}
/* ....................................................................... */
    } else if (s_cmp(cncode, "GRAN", (ftnlen)8, (ftnlen)4) == 0) {
	--neval;
	r8_eval__[neval - 1] = gran_(&r8_eval__[neval - 1], &r8_eval__[neval])
		;
/* ....................................................................... */
    } else if (s_cmp(cncode, "MOD", (ftnlen)8, (ftnlen)3) == 0) {
	--neval;
	r8_eval__[neval - 1] = zzmod_(&r8_eval__[neval - 1], &r8_eval__[neval]
		);
/* ....................................................................... */
    } else if (s_cmp(cncode, "URAN", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = uran_(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "IRAN", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = iran_(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "ERAN", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = eran_(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "LRAN", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = lran_(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "PLEG", (ftnlen)8, (ftnlen)4) == 0) {
	--neval;
	r8_eval__[neval - 1] = legendre_(&r8_eval__[neval - 1], &r8_eval__[
		neval]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "HRFBK4", (ftnlen)8, (ftnlen)6) == 0) {
	--neval;
	r8_eval__[neval - 1] = hrfbk4_(&r8_eval__[neval - 1], &r8_eval__[
		neval]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "HRFBK5", (ftnlen)8, (ftnlen)6) == 0) {
	--neval;
	r8_eval__[neval - 1] = hrfbk5_(&r8_eval__[neval - 1], &r8_eval__[
		neval]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "RHDDC2", (ftnlen)8, (ftnlen)6) == 0) {
	neval += -2;
	r8_eval__[neval - 1] = rhddc2_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "SINH", (ftnlen)8, (ftnlen)4) == 0) {
	if ((d__1 = r8_eval__[neval - 1], abs(d__1)) < 87.5f) {
	    r8_eval__[neval - 1] = sinh(r8_eval__[neval - 1]);
	}
/* ....................................................................... */
    } else if (s_cmp(cncode, "COSH", (ftnlen)8, (ftnlen)4) == 0) {
	if ((d__1 = r8_eval__[neval - 1], abs(d__1)) < 87.5f) {
	    r8_eval__[neval - 1] = cosh(r8_eval__[neval - 1]);
	}
/* ....................................................................... */
    } else if (s_cmp(cncode, "LOGCOSH", (ftnlen)8, (ftnlen)7) == 0) {
	if ((d__1 = r8_eval__[neval - 1], abs(d__1)) < 87.5f) {
	    r8_eval__[neval - 1] = lncosh_(&r8_eval__[neval - 1]);
	}
/* ....................................................................... */
    } else if (s_cmp(cncode, "ACFWXM", (ftnlen)8, (ftnlen)6) == 0) {
	neval += -3;
	r8_eval__[neval - 1] = acfwxm_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1], &r8_eval__[neval + 2]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "GAMP", (ftnlen)8, (ftnlen)4) == 0) {
	--neval;
	r8_eval__[neval - 1] = gamp_(&r8_eval__[neval - 1], &r8_eval__[neval])
		;
/* ....................................................................... */
    } else if (s_cmp(cncode, "GAMQ", (ftnlen)8, (ftnlen)4) == 0) {
	--neval;
	r8_eval__[neval - 1] = gamq_(&r8_eval__[neval - 1], &r8_eval__[neval])
		;
/* ....................................................................... */
    } else if (s_cmp(cncode, "TANH", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = tanh(r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "ASINH", (ftnlen)8, (ftnlen)5) == 0) {
	x = (d__1 = r8_eval__[neval - 1], abs(d__1));
	if (x <= 10.) {
/* Computing 2nd power */
	    d__1 = x;
	    y = x + sqrt(d__1 * d__1 + 1.);
	} else {
/* Computing 2nd power */
	    d__1 = 1. / x;
	    y = x * (sqrt(d__1 * d__1 + 1.) + 1.);
	}
	y = log(y);
	if (r8_eval__[neval - 1] < 0.) {
	    r8_eval__[neval - 1] = -y;
	} else {
	    r8_eval__[neval - 1] = y;
	}
/* ....................................................................... */
    } else if (s_cmp(cncode, "ACOSH", (ftnlen)8, (ftnlen)5) == 0) {
	x = r8_eval__[neval - 1];
	if (x >= 1.) {
	    if (x <= 10.) {
/* Computing 2nd power */
		d__1 = x;
		y = x + sqrt(d__1 * d__1 - 1.);
	    } else {
/* Computing 2nd power */
		d__1 = 1. / x;
		y = x * (sqrt(1. - d__1 * d__1) + 1.);
	    }
	    r8_eval__[neval - 1] = log(y);
	}
/* ....................................................................... */
    } else if (s_cmp(cncode, "ATANH", (ftnlen)8, (ftnlen)5) == 0) {
	x = r8_eval__[neval - 1];
	if (abs(x) < 1.) {
	    r8_eval__[neval - 1] = log((x + 1.) / (1. - x)) * .5;
	}
/* ....................................................................... */
    } else if (s_cmp(cncode, "AI", (ftnlen)8, (ftnlen)2) == 0) {
	r8_eval__[neval - 1] = dai_(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "BI", (ftnlen)8, (ftnlen)2) == 0) {
	r8_eval__[neval - 1] = dbi_(&r8_eval__[neval - 1], &c__1);
/* ....................................................................... */
    } else if (s_cmp(cncode, "ERF", (ftnlen)8, (ftnlen)3) == 0) {
	r8_eval__[neval - 1] = derf_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "ERFC", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = derfc_(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "GAMMA", (ftnlen)8, (ftnlen)5) == 0) {
	r8_eval__[neval - 1] = dgamma_(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "I0", (ftnlen)8, (ftnlen)2) == 0) {
	r8_eval__[neval - 1] = dbesi0_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "I1", (ftnlen)8, (ftnlen)2) == 0) {
	r8_eval__[neval - 1] = dbesi1_(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "J0", (ftnlen)8, (ftnlen)2) == 0) {
	r8_eval__[neval - 1] = dbesj0_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "J1", (ftnlen)8, (ftnlen)2) == 0) {
	r8_eval__[neval - 1] = dbesj1_(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "K0", (ftnlen)8, (ftnlen)2) == 0) {
	r8_eval__[neval - 1] = dbesk0_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "K1", (ftnlen)8, (ftnlen)2) == 0) {
	r8_eval__[neval - 1] = dbesk1_(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "Y0", (ftnlen)8, (ftnlen)2) == 0) {
	r8_eval__[neval - 1] = dbesy0_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "Y1", (ftnlen)8, (ftnlen)2) == 0) {
	r8_eval__[neval - 1] = dbesy1_(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "QG", (ftnlen)8, (ftnlen)2) == 0) {
	r8_eval__[neval - 1] = qg_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "QGINV", (ftnlen)8, (ftnlen)5) == 0) {
	r8_eval__[neval - 1] = qginv_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "BELL2", (ftnlen)8, (ftnlen)5) == 0) {
	r8_eval__[neval - 1] = bell2_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "RECT", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = rect_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "STEP", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = step_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "POSVAL", (ftnlen)8, (ftnlen)6) == 0) {
	r8_eval__[neval - 1] = posval_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "TENT", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = tent_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "BOOL", (ftnlen)8, (ftnlen)4) == 0) {
	r8_eval__[neval - 1] = bool_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "ZTONE", (ftnlen)8, (ftnlen)5) == 0) {
	r8_eval__[neval - 1] = ztone_(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "CDF2STAT", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -4;
	r8_eval__[neval - 1] = cdf2st_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1], &r8_eval__[neval + 2], &
		r8_eval__[neval + 3]);
    } else if (s_cmp(cncode, "STAT2CDF", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -4;
	r8_eval__[neval - 1] = st2cdf_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1], &r8_eval__[neval + 2], &
		r8_eval__[neval + 3]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "NOTZERO", (ftnlen)8, (ftnlen)7) == 0) {
	r8_eval__[neval - 1] = bool_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "ISZERO", (ftnlen)8, (ftnlen)6) == 0 || s_cmp(
	    cncode, "NOT", (ftnlen)8, (ftnlen)3) == 0) {
	r8_eval__[neval - 1] = 1. - bool_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "EQUALS", (ftnlen)8, (ftnlen)6) == 0) {
	--neval;
	d__1 = r8_eval__[neval - 1] - r8_eval__[neval];
	r8_eval__[neval - 1] = 1. - bool_(&d__1);
    } else if (s_cmp(cncode, "ISPOSITI", (ftnlen)8, (ftnlen)8) == 0) {
	r8_eval__[neval - 1] = step_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "ISNEGATI", (ftnlen)8, (ftnlen)8) == 0) {
	d__1 = -r8_eval__[neval - 1];
	r8_eval__[neval - 1] = step_(&d__1);
/* ....................................................................... */
    } else if (s_cmp(cncode, "AND", (ftnlen)8, (ftnlen)3) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = land_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "MEDIAN", (ftnlen)8, (ftnlen)6) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = median_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "MAD", (ftnlen)8, (ftnlen)3) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = mad_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "MEAN", (ftnlen)8, (ftnlen)4) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = mean_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "STDEV", (ftnlen)8, (ftnlen)5) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = stdev_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "SEM", (ftnlen)8, (ftnlen)3) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = sem_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "ORSTAT", (ftnlen)8, (ftnlen)6) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	--ntm;
	itm = (integer) r8_eval__[neval - 1];
	r8_eval__[neval - 1] = orstat_(&itm, &ntm, &r8_eval__[neval]);
    } else if (s_cmp(cncode, "HMODE", (ftnlen)8, (ftnlen)5) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = hmode_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "LMODE", (ftnlen)8, (ftnlen)5) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = lmode_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "OR", (ftnlen)8, (ftnlen)2) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = lor_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "MOFN", (ftnlen)8, (ftnlen)4) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	--ntm;
	itm = (integer) r8_eval__[neval - 1];
	r8_eval__[neval - 1] = lmofn_(&itm, &ntm, &r8_eval__[neval]);
    } else if (s_cmp(cncode, "ASTEP", (ftnlen)8, (ftnlen)5) == 0) {
	--neval;
	if ((d__1 = r8_eval__[neval - 1], abs(d__1)) > r8_eval__[neval]) {
	    r8_eval__[neval - 1] = 1.;
	} else {
	    r8_eval__[neval - 1] = 0.;
	}
    } else if (s_cmp(cncode, "ARGMAX", (ftnlen)8, (ftnlen)6) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = argmax_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "ARGNUM", (ftnlen)8, (ftnlen)6) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = argnum_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "PAIRMAX", (ftnlen)8, (ftnlen)7) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = pairmx_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "PAIRMIN", (ftnlen)8, (ftnlen)7) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = pairmn_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "AMONGST", (ftnlen)8, (ftnlen)7) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = amongf_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "WITHIN", (ftnlen)8, (ftnlen)6) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = withinf_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "MINABOVE", (ftnlen)8, (ftnlen)8) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = minabove_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "MAXBELOW", (ftnlen)8, (ftnlen)8) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = maxbelow_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "EXTREME", (ftnlen)8, (ftnlen)7) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = extreme_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "ABSEXTREME", (ftnlen)8, (ftnlen)10) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	r8_eval__[neval - 1] = absextreme_(&ntm, &r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "CHOOSE", (ftnlen)8, (ftnlen)6) == 0) {
	ntm = (integer) r8_eval__[neval - 1];
	neval -= ntm;
	--ntm;
	itm = (integer) r8_eval__[neval - 1];
	r8_eval__[neval - 1] = choose_(&itm, &ntm, &r8_eval__[neval]);
    } else if (s_cmp(cncode, "IFELSE", (ftnlen)8, (ftnlen)6) == 0) {
	neval += -2;
	if (r8_eval__[neval - 1] != 0.) {
	    r8_eval__[neval - 1] = r8_eval__[neval];
	} else {
	    r8_eval__[neval - 1] = r8_eval__[neval + 1];
	}
/* ....................................................................... */
    } else if (s_cmp(cncode, "FICO_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -3;
	d__2 = (d__1 = r8_eval__[neval - 1], abs(d__1));
	r8_eval__[neval - 1] = ficotp_(&d__2, &r8_eval__[neval], &r8_eval__[
		neval + 1], &r8_eval__[neval + 2]);
    } else if (s_cmp(cncode, "FICO_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -3;
	r8_eval__[neval - 1] = ficopt_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1], &r8_eval__[neval + 2]);
    } else if (s_cmp(cncode, "FICO_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -3;
	r8_eval__[neval - 1] = ficotz_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1], &r8_eval__[neval + 2]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "FITT_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	--neval;
	d__2 = (d__1 = r8_eval__[neval - 1], abs(d__1));
	r8_eval__[neval - 1] = fitttp_(&d__2, &r8_eval__[neval]);
    } else if (s_cmp(cncode, "FITT_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	--neval;
	r8_eval__[neval - 1] = fittpt_(&r8_eval__[neval - 1], &r8_eval__[
		neval]);
    } else if (s_cmp(cncode, "FITT_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	--neval;
	r8_eval__[neval - 1] = fitttz_(&r8_eval__[neval - 1], &r8_eval__[
		neval]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "FIFT_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -2;
	r8_eval__[neval - 1] = fifttp_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1]);
    } else if (s_cmp(cncode, "FIFT_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -2;
	r8_eval__[neval - 1] = fiftpt_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1]);
    } else if (s_cmp(cncode, "FIFT_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -2;
	r8_eval__[neval - 1] = fifttz_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "FIZT_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	d__2 = (d__1 = r8_eval__[neval - 1], abs(d__1));
	r8_eval__[neval - 1] = fizttp_(&d__2);
    } else if (s_cmp(cncode, "FIZT_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	r8_eval__[neval - 1] = fiztpt_(&r8_eval__[neval - 1]);
    } else if (s_cmp(cncode, "FIZT_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	r8_eval__[neval - 1] = fizttz_(&r8_eval__[neval - 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "FICT_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	--neval;
	r8_eval__[neval - 1] = ficttp_(&r8_eval__[neval - 1], &r8_eval__[
		neval]);
    } else if (s_cmp(cncode, "FICT_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	--neval;
	r8_eval__[neval - 1] = fictpt_(&r8_eval__[neval - 1], &r8_eval__[
		neval]);
    } else if (s_cmp(cncode, "FICT_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	--neval;
	r8_eval__[neval - 1] = ficttz_(&r8_eval__[neval - 1], &r8_eval__[
		neval]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "FIBT_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -2;
	r8_eval__[neval - 1] = fibttp_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1]);
    } else if (s_cmp(cncode, "FIBT_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -2;
	r8_eval__[neval - 1] = fibtpt_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1]);
    } else if (s_cmp(cncode, "FIBT_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -2;
	r8_eval__[neval - 1] = fibttz_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "FIBN_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -2;
	r8_eval__[neval - 1] = fibntp_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1]);
    } else if (s_cmp(cncode, "FIBN_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -2;
	r8_eval__[neval - 1] = fibnpt_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1]);
    } else if (s_cmp(cncode, "FIBN_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -2;
	r8_eval__[neval - 1] = fibntz_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "FIGT_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -2;
	r8_eval__[neval - 1] = figttp_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1]);
    } else if (s_cmp(cncode, "FIGT_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -2;
	r8_eval__[neval - 1] = figtpt_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1]);
    } else if (s_cmp(cncode, "FIGT_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	neval += -2;
	r8_eval__[neval - 1] = figttz_(&r8_eval__[neval - 1], &r8_eval__[
		neval], &r8_eval__[neval + 1]);
/* ....................................................................... */
    } else if (s_cmp(cncode, "FIPT_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	--neval;
	r8_eval__[neval - 1] = fipttp_(&r8_eval__[neval - 1], &r8_eval__[
		neval]);
    } else if (s_cmp(cncode, "FIPT_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	--neval;
	r8_eval__[neval - 1] = fiptpt_(&r8_eval__[neval - 1], &r8_eval__[
		neval]);
    } else if (s_cmp(cncode, "FIPT_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	--neval;
	r8_eval__[neval - 1] = fipttz_(&r8_eval__[neval - 1], &r8_eval__[
		neval]);
/* ....................................................................... */
    }
/* ....................................................................... */
    if (ncode < *num_code__) {
	goto L1000;
    }
    ret_val = r8_eval__[neval - 1];
/* ----------------------------------------------------------------------- */
L8000:
    return ret_val;
} /* pareval_ */

#undef r8_val__
#undef c8_val__





/* Subroutine */ int parevec_(integer *num_code__, char *c_code__, doublereal 
	*va, doublereal *vb, doublereal *vc, doublereal *vd, doublereal *ve, 
	doublereal *vf, doublereal *vg, doublereal *vh, doublereal *vi, 
	doublereal *vj, doublereal *vk, doublereal *vl, doublereal *vm, 
	doublereal *vn, doublereal *vo, doublereal *vp, doublereal *vq, 
	doublereal *vr, doublereal *vs, doublereal *vt, doublereal *vu, 
	doublereal *vv, doublereal *vw, doublereal *vx, doublereal *vy, 
	doublereal *vz, integer *lvec, doublereal *vout, ftnlen c_code_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2;
    static doublereal equiv_0[1];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    double d_int(doublereal *), pow_dd(doublereal *, doublereal *), sin(
	    doublereal), cos(doublereal), tan(doublereal), sqrt(doublereal), 
	    exp(doublereal), log(doublereal), d_lg10(doublereal *), asin(
	    doublereal), acos(doublereal), atan(doublereal), atan2(doublereal,
	     doublereal), sinh(doublereal), cosh(doublereal), tanh(doublereal)
	    ;

    /* Local variables */
    extern doublereal legendre_(doublereal *, doublereal *), minabove_(
	    integer *, doublereal *), maxbelow_(integer *, doublereal *);
    static doublereal x, y;
    static integer jf, iv;
    extern doublereal qg_(doublereal *), absextreme_(integer *, doublereal *),
	     dai_(doublereal *), dbi_(doublereal *, integer *), mad_(integer *
	    , doublereal *);
    static integer ibv;
    extern doublereal sem_(integer *, doublereal *);
    static integer itm, jtm;
    extern doublereal lor_(integer *, doublereal *);
    static integer ntm;
    extern doublereal land_(integer *, doublereal *), mean_(integer *, 
	    doublereal *), derf_(doublereal *), eran_(doublereal *), gran_(
	    doublereal *, doublereal *), iran_(doublereal *), bool_(
	    doublereal *), lran_(doublereal *), rect_(doublereal *);
    static doublereal scop[101];
    extern doublereal uran_(doublereal *), tent_(doublereal *), step_(
	    doublereal *), bell2_(doublereal *);
    static doublereal r8val[1664]	/* was [64][26] */;
    extern doublereal derfc_(doublereal *);
    static integer ncode;
    extern doublereal hmode_(integer *, doublereal *), lmode_(integer *, 
	    doublereal *);
    static integer neval;
    extern doublereal lmofn_(integer *, integer *, doublereal *);
    static integer ivbot;
    extern doublereal qginv_(doublereal *), stdev_(integer *, doublereal *);
    static char c2code[8];
    extern doublereal ztone_(doublereal *);
    static integer ivtop;
    extern doublereal zzmod_(doublereal *, doublereal *), dbesi0_(doublereal *
	    ), dbesi1_(doublereal *), dbesj0_(doublereal *), dbesj1_(
	    doublereal *), dbesk0_(doublereal *), dbesk1_(doublereal *);
#define c8_val__ ((char *)equiv_0)
    extern doublereal rhddc2_(doublereal *, doublereal *, doublereal *), 
	    hrfbk4_(doublereal *, doublereal *), hrfbk5_(doublereal *, 
	    doublereal *), cdf2st_(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), dbesy0_(doublereal *), dbesy1_(
	    doublereal *), st2cdf_(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);
#define r8_val__ (equiv_0)
    extern doublereal dgamma_(doublereal *);
    static char cncode[8];
    extern doublereal median_(integer *, doublereal *);
    static integer ialpha;
    extern doublereal cbrtff_(doublereal *), amongf_(integer *, doublereal *),
	     argmax_(integer *, doublereal *), choose_(integer *, integer *, 
	    doublereal *), fibntp_(doublereal *, doublereal *, doublereal *), 
	    fibnpt_(doublereal *, doublereal *, doublereal *), ficotp_(
	    doublereal *, doublereal *, doublereal *, doublereal *), acfwxm_(
	    doublereal *, doublereal *, doublereal *, doublereal *), pairmn_(
	    integer *, doublereal *), lncosh_(doublereal *), ficopt_(
	    doublereal *, doublereal *, doublereal *, doublereal *), argnum_(
	    integer *, doublereal *), ficttp_(doublereal *, doublereal *), 
	    fictpt_(doublereal *, doublereal *), fifttp_(doublereal *, 
	    doublereal *, doublereal *), fiftpt_(doublereal *, doublereal *, 
	    doublereal *), ficotz_(doublereal *, doublereal *, doublereal *, 
	    doublereal *), fibttp_(doublereal *, doublereal *, doublereal *), 
	    pairmx_(integer *, doublereal *), fibtpt_(doublereal *, 
	    doublereal *, doublereal *), fibttz_(doublereal *, doublereal *, 
	    doublereal *), ficttz_(doublereal *, doublereal *), posval_(
	    doublereal *), fibntz_(doublereal *, doublereal *, doublereal *), 
	    fifttz_(doublereal *, doublereal *, doublereal *), figttp_(
	    doublereal *, doublereal *, doublereal *), figtpt_(doublereal *, 
	    doublereal *, doublereal *), figttz_(doublereal *, doublereal *, 
	    doublereal *), fitttp_(doublereal *, doublereal *), fittpt_(
	    doublereal *, doublereal *), orstat_(integer *, integer *, 
	    doublereal *), fipttp_(doublereal *, doublereal *), fiptpt_(
	    doublereal *, doublereal *), fizttp_(doublereal *), fiztpt_(
	    doublereal *), fipttz_(doublereal *, doublereal *), fitttz_(
	    doublereal *, doublereal *), fizttz_(doublereal *);
    static doublereal r8_eval__[6464]	/* was [64][101] */;
    extern doublereal withinf_(integer *, doublereal *), extreme_(integer *, 
	    doublereal *);


/*  Vector version of PAREVAL, where VA..VZ with length LVEC */
/*  are supplied as vectors. */
/*  [Modified by Raoqiong Tong, August 1997] */




/*  14 Jul 1998: add 1D array for stack copy */


/*  Internal library functions */


/*  External library functions */


/*  Statistics functions (01 Mar 1999 - see parser_int.c) */


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

    /* Parameter adjustments */
    c_code__ -= 8;
    --vout;
    --vz;
    --vy;
    --vx;
    --vw;
    --vv;
    --vu;
    --vt;
    --vs;
    --vr;
    --vq;
    --vp;
    --vo;
    --vn;
    --vm;
    --vl;
    --vk;
    --vj;
    --vi;
    --vh;
    --vg;
    --vf;
    --ve;
    --vd;
    --vc;
    --vb;
    --va;

    /* Function Body */
    if (*num_code__ <= 0 || *lvec <= 0) {
	goto L8000;
    }

    ialpha = 'A' - 1;
/* ----------------------------------------------------------------------- */
    i__1 = *lvec - 1;
    for (ibv = 0; ibv <= i__1; ibv += 64) {
	ivbot = ibv + 1;
	ivtop = ibv + 64;
	if (ivtop > *lvec) {
	    ivtop = *lvec;
	}

/* cc         WRITE(*,9802) IVBOT,IVTOP */
/* cc9802     FORMAT('   .. PAREVEC: loop from',I5,' to',I5) */

	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv - 1] = va[iv];
/* L100: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 63] = vb[iv];
/* L101: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 127] = vc[iv];
/* L102: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 191] = vd[iv];
/* L103: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 255] = ve[iv];
/* L104: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 319] = vf[iv];
/* L105: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 383] = vg[iv];
/* L106: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 447] = vh[iv];
/* L107: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 511] = vi[iv];
/* L108: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 575] = vj[iv];
/* L109: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 639] = vk[iv];
/* L110: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 703] = vl[iv];
/* L111: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 767] = vm[iv];
/* L112: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 831] = vn[iv];
/* L113: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 895] = vo[iv];
/* L114: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 959] = vp[iv];
/* L115: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 1023] = vq[iv];
/* L116: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 1087] = vr[iv];
/* L117: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 1151] = vs[iv];
/* L118: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 1215] = vt[iv];
/* L119: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 1279] = vu[iv];
/* L120: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 1343] = vv[iv];
/* L121: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 1407] = vw[iv];
/* L122: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 1471] = vx[iv];
/* L123: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 1535] = vy[iv];
/* L124: */
	}
	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    r8val[iv - ibv + 1599] = vz[iv];
/* L125: */
	}

	neval = 0;
	ncode = 0;

L1000:
	++ncode;
	s_copy(cncode, c_code__ + (ncode << 3), (ftnlen)8, (ftnlen)8);
/* cc         WRITE(*,9803) CNCODE */
/* cc9803     FORMAT('   .. PAREVEC: opcode=',A) */
/* ....................................................................... */
	if (s_cmp(cncode, "PUSHSYM", (ftnlen)8, (ftnlen)7) == 0) {
	    jf = *(unsigned char *)&c_code__[(ncode + 1) * 8] - ialpha;
	    if (ncode + 2 <= *num_code__) {
		s_copy(c2code, c_code__ + (ncode + 2 << 3), (ftnlen)8, (
			ftnlen)8);
	    } else {
		s_copy(c2code, "q", (ftnlen)8, (ftnlen)1);
	    }
	    if (s_cmp(c2code, "+", (ftnlen)8, (ftnlen)1) == 0) {
		ncode += 2;
		i__2 = ivtop;
		for (iv = ivbot; iv <= i__2; ++iv) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] += r8val[iv - ibv 
			    + (jf << 6) - 65];
		}
	    } else if (s_cmp(c2code, "-", (ftnlen)8, (ftnlen)1) == 0) {
		ncode += 2;
		i__2 = ivtop;
		for (iv = ivbot; iv <= i__2; ++iv) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] -= r8val[iv - ibv 
			    + (jf << 6) - 65];
		}
	    } else if (s_cmp(c2code, "*", (ftnlen)8, (ftnlen)1) == 0) {
		ncode += 2;
		i__2 = ivtop;
		for (iv = ivbot; iv <= i__2; ++iv) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] *= r8val[iv - ibv 
			    + (jf << 6) - 65];
		}
	    } else if (s_cmp(c2code, "/", (ftnlen)8, (ftnlen)1) == 0) {
		ncode += 2;
		i__2 = ivtop;
		for (iv = ivbot; iv <= i__2; ++iv) {
		    if (r8val[iv - ibv + (jf << 6) - 65] != 0.) {
			r8_eval__[iv - ibv + (neval << 6) - 65] /= r8val[iv - 
				ibv + (jf << 6) - 65];
		    } else {
			r8_eval__[iv - ibv + (neval << 6) - 65] = 0.;
		    }
		}
	    } else {
		++neval;
		++ncode;
		i__2 = ivtop;
		for (iv = ivbot; iv <= i__2; ++iv) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = r8val[iv - ibv 
			    + (jf << 6) - 65];
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "PUSHNUM", (ftnlen)8, (ftnlen)7) == 0) {
	    s_copy(c8_val__, c_code__ + (ncode + 1 << 3), (ftnlen)8, (ftnlen)
		    8);
	    if (ncode + 2 <= *num_code__) {
		s_copy(c2code, c_code__ + (ncode + 2 << 3), (ftnlen)8, (
			ftnlen)8);
	    } else {
		s_copy(c2code, "q", (ftnlen)8, (ftnlen)1);
	    }
	    if (s_cmp(c2code, "+", (ftnlen)8, (ftnlen)1) == 0) {
		ncode += 2;
		i__2 = ivtop;
		for (iv = ivbot; iv <= i__2; ++iv) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] += *r8_val__;
		}
	    } else if (s_cmp(c2code, "-", (ftnlen)8, (ftnlen)1) == 0) {
		ncode += 2;
		i__2 = ivtop;
		for (iv = ivbot; iv <= i__2; ++iv) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] -= *r8_val__;
		}
	    } else if (s_cmp(c2code, "*", (ftnlen)8, (ftnlen)1) == 0) {
		ncode += 2;
		i__2 = ivtop;
		for (iv = ivbot; iv <= i__2; ++iv) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] *= *r8_val__;
		}
	    } else if (s_cmp(c2code, "/", (ftnlen)8, (ftnlen)1) == 0) {
		ncode += 2;
		if (*r8_val__ != 0.) {
		    *r8_val__ = 1. / *r8_val__;
		    i__2 = ivtop;
		    for (iv = ivbot; iv <= i__2; ++iv) {
			r8_eval__[iv - ibv + (neval << 6) - 65] *= *r8_val__;
		    }
		} else {
		    i__2 = ivtop;
		    for (iv = ivbot; iv <= i__2; ++iv) {
			r8_eval__[iv - ibv + (neval << 6) - 65] = 0.;
		    }
		}
	    } else {
		++ncode;
		++neval;
		i__2 = ivtop;
		for (iv = ivbot; iv <= i__2; ++iv) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = *r8_val__;
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "+", (ftnlen)8, (ftnlen)1) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] += r8_eval__[iv - ibv 
			+ (neval + 1 << 6) - 65];
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "-", (ftnlen)8, (ftnlen)1) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] -= r8_eval__[iv - ibv 
			+ (neval + 1 << 6) - 65];
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "*", (ftnlen)8, (ftnlen)1) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] *= r8_eval__[iv - ibv 
			+ (neval + 1 << 6) - 65];
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "/", (ftnlen)8, (ftnlen)1) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		if (r8_eval__[iv - ibv + (neval + 1 << 6) - 65] != 0.) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] /= r8_eval__[iv - 
			    ibv + (neval + 1 << 6) - 65];
		} else {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = 0.;
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "**", (ftnlen)8, (ftnlen)2) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		if (r8_eval__[iv - ibv + (neval << 6) - 65] > 0. || r8_eval__[
			iv - ibv + (neval << 6) - 65] != 0. && r8_eval__[iv - 
			ibv + (neval + 1 << 6) - 65] == d_int(&r8_eval__[iv - 
			ibv + (neval + 1 << 6) - 65])) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = pow_dd(&
			    r8_eval__[iv - ibv + (neval << 6) - 65], &
			    r8_eval__[iv - ibv + (neval + 1 << 6) - 65]);
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "--", (ftnlen)8, (ftnlen)2) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = -r8_eval__[iv - ibv 
			+ (neval << 6) - 65];
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "SIN", (ftnlen)8, (ftnlen)3) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = sin(r8_eval__[iv - 
			ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "SIND", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = sin(r8_eval__[iv - 
			ibv + (neval << 6) - 65] * .01745329251994);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "COS", (ftnlen)8, (ftnlen)3) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = cos(r8_eval__[iv - 
			ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "COSD", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = cos(r8_eval__[iv - 
			ibv + (neval << 6) - 65] * .01745329251994);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "TAN", (ftnlen)8, (ftnlen)3) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = tan(r8_eval__[iv - 
			ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "TAND", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = tan(r8_eval__[iv - 
			ibv + (neval << 6) - 65] * .01745329251994);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "SQRT", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = sqrt((d__1 = 
			r8_eval__[iv - ibv + (neval << 6) - 65], abs(d__1)));
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "CBRT", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = cbrtff_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "ABS", (ftnlen)8, (ftnlen)3) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
/* cc               WRITE(*,9809) IV */
/* cc9809           FORMAT('     about to ABS #',I5) */
		r8_eval__[iv - ibv + (neval << 6) - 65] = (d__1 = r8_eval__[
			iv - ibv + (neval << 6) - 65], abs(d__1));
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "EXP", (ftnlen)8, (ftnlen)3) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
/* Computing MIN */
		d__1 = 87.5f, d__2 = r8_eval__[iv - ibv + (neval << 6) - 65];
		r8_eval__[iv - ibv + (neval << 6) - 65] = exp((min(d__1,d__2))
			);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "LOG", (ftnlen)8, (ftnlen)3) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		if (r8_eval__[iv - ibv + (neval << 6) - 65] != 0.) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = log((d__1 = 
			    r8_eval__[iv - ibv + (neval << 6) - 65], abs(d__1)
			    ));
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "LOG10", (ftnlen)8, (ftnlen)5) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		if (r8_eval__[iv - ibv + (neval << 6) - 65] != 0.) {
		    d__2 = (d__1 = r8_eval__[iv - ibv + (neval << 6) - 65], 
			    abs(d__1));
		    r8_eval__[iv - ibv + (neval << 6) - 65] = d_lg10(&d__2);
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "INT", (ftnlen)8, (ftnlen)3) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = d_int(&r8_eval__[iv 
			- ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "MAX", (ftnlen)8, (ftnlen)3) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
/* Computing MAX */
		d__1 = r8_eval__[iv - ibv + (neval << 6) - 65], d__2 = 
			r8_eval__[iv - ibv + (neval + 1 << 6) - 65];
		r8_eval__[iv - ibv + (neval << 6) - 65] = max(d__1,d__2);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "MIN", (ftnlen)8, (ftnlen)3) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
/* Computing MIN */
		d__1 = r8_eval__[iv - ibv + (neval << 6) - 65], d__2 = 
			r8_eval__[iv - ibv + (neval + 1 << 6) - 65];
		r8_eval__[iv - ibv + (neval << 6) - 65] = min(d__1,d__2);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "ASIN", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		if ((d__1 = r8_eval__[iv - ibv + (neval << 6) - 65], abs(d__1)
			) <= 1.) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = asin(r8_eval__[
			    iv - ibv + (neval << 6) - 65]);
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "ACOS", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		if ((d__1 = r8_eval__[iv - ibv + (neval << 6) - 65], abs(d__1)
			) <= 1.) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = acos(r8_eval__[
			    iv - ibv + (neval << 6) - 65]);
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "ATAN", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = atan(r8_eval__[iv - 
			ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "ATAN2", (ftnlen)8, (ftnlen)5) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		if (r8_eval__[iv - ibv + (neval << 6) - 65] != 0. || 
			r8_eval__[iv - ibv + (neval + 1 << 6) - 65] != 0.) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = atan2(r8_eval__[
			    iv - ibv + (neval << 6) - 65], r8_eval__[iv - ibv 
			    + (neval + 1 << 6) - 65]);
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "GRAN", (ftnlen)8, (ftnlen)4) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = gran_(&r8_eval__[iv 
			- ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "MOD", (ftnlen)8, (ftnlen)3) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = zzmod_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "URAN", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = uran_(&r8_eval__[iv 
			- ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "IRAN", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = iran_(&r8_eval__[iv 
			- ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "ERAN", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = eran_(&r8_eval__[iv 
			- ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "LRAN", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = lran_(&r8_eval__[iv 
			- ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "PLEG", (ftnlen)8, (ftnlen)4) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = legendre_(&
			r8_eval__[iv - ibv + (neval << 6) - 65], &r8_eval__[
			iv - ibv + (neval + 1 << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "HRFBK4", (ftnlen)8, (ftnlen)6) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = hrfbk4_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "HRFBK5", (ftnlen)8, (ftnlen)6) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = hrfbk5_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "RHDDC2", (ftnlen)8, (ftnlen)6) == 0) {
	    neval += -2;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = rhddc2_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "SINH", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		if ((d__1 = r8_eval__[iv - ibv + (neval << 6) - 65], abs(d__1)
			) < 87.5f) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = sinh(r8_eval__[
			    iv - ibv + (neval << 6) - 65]);
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "COSH", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		if ((d__1 = r8_eval__[iv - ibv + (neval << 6) - 65], abs(d__1)
			) < 87.5f) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = cosh(r8_eval__[
			    iv - ibv + (neval << 6) - 65]);
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "LOGCOSH", (ftnlen)8, (ftnlen)7) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		if ((d__1 = r8_eval__[iv - ibv + (neval << 6) - 65], abs(d__1)
			) < 87.5f) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = lncosh_(&
			    r8_eval__[iv - ibv + (neval << 6) - 65]);
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "ACFWXM", (ftnlen)8, (ftnlen)6) == 0) {
	    neval += -3;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = acfwxm_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65], &r8_eval__[iv - ibv + (neval + 3 << 6) 
			- 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "TANH", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = tanh(r8_eval__[iv - 
			ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "ASINH", (ftnlen)8, (ftnlen)5) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		x = (d__1 = r8_eval__[iv - ibv + (neval << 6) - 65], abs(d__1)
			);
		if (x <= 10.) {
/* Computing 2nd power */
		    d__1 = x;
		    y = x + sqrt(d__1 * d__1 + 1.);
		} else {
/* Computing 2nd power */
		    d__1 = 1. / x;
		    y = x * (sqrt(d__1 * d__1 + 1.) + 1.);
		}
		y = log(y);
		if (r8_eval__[iv - ibv + (neval << 6) - 65] < 0.) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = -y;
		} else {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = y;
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "ACOSH", (ftnlen)8, (ftnlen)5) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		x = r8_eval__[iv - ibv + (neval << 6) - 65];
		if (x >= 1.) {
		    if (x <= 10.) {
/* Computing 2nd power */
			d__1 = x;
			y = x + sqrt(d__1 * d__1 - 1.);
		    } else {
/* Computing 2nd power */
			d__1 = 1. / x;
			y = x * (sqrt(1. - d__1 * d__1) + 1.);
		    }
		    r8_eval__[iv - ibv + (neval << 6) - 65] = log(y);
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "ATANH", (ftnlen)8, (ftnlen)5) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		x = r8_eval__[iv - ibv + (neval << 6) - 65];
		if (abs(x) < 1.) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = log((x + 1.) / (
			    1. - x)) * .5;
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "AI", (ftnlen)8, (ftnlen)2) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = dai_(&r8_eval__[iv 
			- ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "BI", (ftnlen)8, (ftnlen)2) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = dbi_(&r8_eval__[iv 
			- ibv + (neval << 6) - 65], &c__1);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "ERF", (ftnlen)8, (ftnlen)3) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = derf_(&r8_eval__[iv 
			- ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "ERFC", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = derfc_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "GAMMA", (ftnlen)8, (ftnlen)5) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = dgamma_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "I0", (ftnlen)8, (ftnlen)2) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = dbesi0_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "I1", (ftnlen)8, (ftnlen)2) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = dbesi1_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "J0", (ftnlen)8, (ftnlen)2) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = dbesj0_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "J1", (ftnlen)8, (ftnlen)2) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = dbesj1_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "K0", (ftnlen)8, (ftnlen)2) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = dbesk0_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "K1", (ftnlen)8, (ftnlen)2) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = dbesk1_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "Y0", (ftnlen)8, (ftnlen)2) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = dbesy0_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "Y1", (ftnlen)8, (ftnlen)2) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = dbesy1_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "QG", (ftnlen)8, (ftnlen)2) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = qg_(&r8_eval__[iv - 
			ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "QGINV", (ftnlen)8, (ftnlen)5) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = qginv_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "BELL2", (ftnlen)8, (ftnlen)5) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = bell2_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "RECT", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = rect_(&r8_eval__[iv 
			- ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "STEP", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = step_(&r8_eval__[iv 
			- ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "POSVAL", (ftnlen)8, (ftnlen)6) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = posval_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "TENT", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = tent_(&r8_eval__[iv 
			- ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "BOOL", (ftnlen)8, (ftnlen)4) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = bool_(&r8_eval__[iv 
			- ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "ZTONE", (ftnlen)8, (ftnlen)5) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = ztone_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "CDF2STAT", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -4;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = cdf2st_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65], &r8_eval__[iv - ibv + (neval + 3 << 6) 
			- 65], &r8_eval__[iv - ibv + (neval + 4 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "STAT2CDF", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -4;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = st2cdf_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65], &r8_eval__[iv - ibv + (neval + 3 << 6) 
			- 65], &r8_eval__[iv - ibv + (neval + 4 << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "NOTZERO", (ftnlen)8, (ftnlen)7) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = bool_(&r8_eval__[iv 
			- ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "ISZERO", (ftnlen)8, (ftnlen)6) == 0 || 
		s_cmp(cncode, "NOT", (ftnlen)8, (ftnlen)3) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = 1. - bool_(&
			r8_eval__[iv - ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "EQUALS", (ftnlen)8, (ftnlen)6) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		d__1 = r8_eval__[iv - ibv + (neval << 6) - 65] - r8_eval__[iv 
			- ibv + (neval + 1 << 6) - 65];
		r8_eval__[iv - ibv + (neval << 6) - 65] = 1. - bool_(&d__1);
	    }
	} else if (s_cmp(cncode, "ISPOSITI", (ftnlen)8, (ftnlen)8) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = step_(&r8_eval__[iv 
			- ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "ISNEGATI", (ftnlen)8, (ftnlen)8) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		d__1 = -r8_eval__[iv - ibv + (neval << 6) - 65];
		r8_eval__[iv - ibv + (neval << 6) - 65] = step_(&d__1);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "AND", (ftnlen)8, (ftnlen)3) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = land_(&ntm, scop);
	    }
	} else if (s_cmp(cncode, "MEDIAN", (ftnlen)8, (ftnlen)6) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = median_(&ntm, scop);
	    }
	} else if (s_cmp(cncode, "MAD", (ftnlen)8, (ftnlen)3) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = mad_(&ntm, scop);
	    }
	} else if (s_cmp(cncode, "MEAN", (ftnlen)8, (ftnlen)4) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = mean_(&ntm, scop);
	    }
	} else if (s_cmp(cncode, "STDEV", (ftnlen)8, (ftnlen)5) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = stdev_(&ntm, scop);
	    }
	} else if (s_cmp(cncode, "SEM", (ftnlen)8, (ftnlen)3) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = sem_(&ntm, scop);
	    }
	} else if (s_cmp(cncode, "ORSTAT", (ftnlen)8, (ftnlen)6) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    --ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		itm = (integer) r8_eval__[iv - ibv + (neval << 6) - 65];
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm << 6) - 
			    65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = orstat_(&itm, &ntm, 
			scop);
	    }
	} else if (s_cmp(cncode, "HMODE", (ftnlen)8, (ftnlen)5) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = hmode_(&ntm, scop);
	    }
	} else if (s_cmp(cncode, "LMODE", (ftnlen)8, (ftnlen)5) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = lmode_(&ntm, scop);
	    }
	} else if (s_cmp(cncode, "OR", (ftnlen)8, (ftnlen)2) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = lor_(&ntm, scop);
	    }
	} else if (s_cmp(cncode, "MOFN", (ftnlen)8, (ftnlen)4) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    --ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		itm = (integer) r8_eval__[iv - ibv + (neval << 6) - 65];
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm << 6) - 
			    65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = lmofn_(&itm, &ntm, 
			scop);
	    }
	} else if (s_cmp(cncode, "ASTEP", (ftnlen)8, (ftnlen)5) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		if ((d__1 = r8_eval__[iv - ibv + (neval << 6) - 65], abs(d__1)
			) > r8_eval__[iv - ibv + (neval + 1 << 6) - 65]) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = 1.;
		} else {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = 0.;
		}
	    }
	} else if (s_cmp(cncode, "ARGMAX", (ftnlen)8, (ftnlen)6) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = argmax_(&ntm, scop);
	    }
	} else if (s_cmp(cncode, "ARGNUM", (ftnlen)8, (ftnlen)6) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = argnum_(&ntm, scop);
	    }
	} else if (s_cmp(cncode, "PAIRMAX", (ftnlen)8, (ftnlen)7) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = pairmx_(&ntm, scop);
	    }
	} else if (s_cmp(cncode, "PAIRMIN", (ftnlen)8, (ftnlen)7) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = pairmn_(&ntm, scop);
	    }
	} else if (s_cmp(cncode, "AMONGST", (ftnlen)8, (ftnlen)7) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = amongf_(&ntm, scop);
	    }
	} else if (s_cmp(cncode, "WITHIN", (ftnlen)8, (ftnlen)6) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = withinf_(&ntm, scop)
			;
	    }
	} else if (s_cmp(cncode, "MINABOVE", (ftnlen)8, (ftnlen)8) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = minabove_(&ntm, 
			scop);
	    }
	} else if (s_cmp(cncode, "MAXBELOW", (ftnlen)8, (ftnlen)8) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = maxbelow_(&ntm, 
			scop);
	    }
	} else if (s_cmp(cncode, "EXTREME", (ftnlen)8, (ftnlen)7) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = extreme_(&ntm, scop)
			;
	    }
	} else if (s_cmp(cncode, "ABSEXTREME", (ftnlen)8, (ftnlen)10) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm - 1 << 
			    6) - 65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = absextreme_(&ntm, 
			scop);
	    }
	} else if (s_cmp(cncode, "CHOOSE", (ftnlen)8, (ftnlen)6) == 0) {
	    ntm = (integer) r8_eval__[(neval << 6) - 64];
	    neval -= ntm;
	    --ntm;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		itm = (integer) r8_eval__[iv - ibv + (neval << 6) - 65];
		i__3 = ntm;
		for (jtm = 1; jtm <= i__3; ++jtm) {
		    scop[jtm - 1] = r8_eval__[iv - ibv + (neval + jtm << 6) - 
			    65];
		}
		r8_eval__[iv - ibv + (neval << 6) - 65] = choose_(&itm, &ntm, 
			scop);
	    }
	} else if (s_cmp(cncode, "IFELSE", (ftnlen)8, (ftnlen)6) == 0) {
	    neval += -2;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		if (r8_eval__[iv - ibv + (neval << 6) - 65] != 0.) {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = r8_eval__[iv - 
			    ibv + (neval + 1 << 6) - 65];
		} else {
		    r8_eval__[iv - ibv + (neval << 6) - 65] = r8_eval__[iv - 
			    ibv + (neval + 2 << 6) - 65];
		}
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "FICO_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -3;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		d__2 = (d__1 = r8_eval__[iv - ibv + (neval << 6) - 65], abs(
			d__1));
		r8_eval__[iv - ibv + (neval << 6) - 65] = ficotp_(&d__2, &
			r8_eval__[iv - ibv + (neval + 1 << 6) - 65], &
			r8_eval__[iv - ibv + (neval + 2 << 6) - 65], &
			r8_eval__[iv - ibv + (neval + 3 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FICO_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -3;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = ficopt_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65], &r8_eval__[iv - ibv + (neval + 3 << 6) 
			- 65]);
	    }
	} else if (s_cmp(cncode, "FICO_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -3;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = ficotz_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65], &r8_eval__[iv - ibv + (neval + 3 << 6) 
			- 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "FITT_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		d__2 = (d__1 = r8_eval__[iv - ibv + (neval << 6) - 65], abs(
			d__1));
		r8_eval__[iv - ibv + (neval << 6) - 65] = fitttp_(&d__2, &
			r8_eval__[iv - ibv + (neval + 1 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FITT_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fittpt_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FITT_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fitttz_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "FIFT_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -2;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fifttp_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FIFT_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -2;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fiftpt_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FIFT_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -2;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fifttz_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "FIZT_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		d__2 = (d__1 = r8_eval__[iv - ibv + (neval << 6) - 65], abs(
			d__1));
		r8_eval__[iv - ibv + (neval << 6) - 65] = fizttp_(&d__2);
	    }
	} else if (s_cmp(cncode, "FIZT_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fiztpt_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FIZT_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fizttz_(&r8_eval__[
			iv - ibv + (neval << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "FICT_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = ficttp_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FICT_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fictpt_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FICT_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = ficttz_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "FIBT_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -2;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fibttp_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FIBT_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -2;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fibtpt_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FIBT_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -2;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fibttz_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "FIBN_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -2;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fibntp_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FIBN_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -2;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fibnpt_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FIBN_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -2;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fibntz_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "FIGT_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -2;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = figttp_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FIGT_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -2;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = figtpt_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FIGT_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	    neval += -2;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = figttz_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65], &r8_eval__[iv - ibv + (neval + 
			2 << 6) - 65]);
	    }
/* ....................................................................... */
	} else if (s_cmp(cncode, "FIPT_T2P", (ftnlen)8, (ftnlen)8) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fipttp_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FIPT_P2T", (ftnlen)8, (ftnlen)8) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fiptpt_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65]);
	    }
	} else if (s_cmp(cncode, "FIPT_T2Z", (ftnlen)8, (ftnlen)8) == 0) {
	    --neval;
	    i__2 = ivtop;
	    for (iv = ivbot; iv <= i__2; ++iv) {
		r8_eval__[iv - ibv + (neval << 6) - 65] = fipttz_(&r8_eval__[
			iv - ibv + (neval << 6) - 65], &r8_eval__[iv - ibv + (
			neval + 1 << 6) - 65]);
	    }
/* ....................................................................... */
	}
/* ---------------------------------------------------------------------- */
	if (ncode < *num_code__) {
	    goto L1000;
	}

	i__2 = ivtop;
	for (iv = ivbot; iv <= i__2; ++iv) {
	    vout[iv] = r8_eval__[iv - ibv + (neval << 6) - 65];
/* L4990: */
	}

/* L5000: */
    }
/* ----------------------------------------------------------------------- */
L8000:
    return 0;
} /* parevec_ */

#undef r8_val__
#undef c8_val__





doublereal ztone_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double tan(doublereal), tanh(doublereal);

    /* Local variables */
    static doublereal y;

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

    if (*x <= 0.) {
	ret_val = 0.;
    } else if (*x >= 1.f) {
	ret_val = 1.;
    } else {
	y = (*x * 1.6 - .8) * 1.5707963267948966;
	ret_val = (tanh(tan(y)) + .99576486) * .50212657;
    }
    return ret_val;
} /* ztone_ */




doublereal qg_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Local variables */
    extern doublereal derfc_(doublereal *);


/*  Compute the reversed cdf of a Gaussian. */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

    d__1 = *x / 1.414213562373095;
    ret_val = derfc_(&d__1) * .5;
    return ret_val;
} /* qg_ */




/* CC The UNIF() function is now in parser_int.c, */
/* CC where it calls upon the C library to do the dirty work. */

/* CC      FUNCTION UNIF( XJUNK ) */
/* CC      IMPLICIT REAL*8 (A-H,O-Z) */
/* CC      PARAMETER ( IA = 99992 , IB = 12345 , IT = 99991 ) */
/* CC      PARAMETER ( F  = 1.00009D-05 ) */
/* CC      DATA IX / 271 / */
/* CCC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* CC      IX = MOD( IA*IX+IB , IT ) */
/* CC      UNIF = F * IX */
/* CC      RETURN */
/* CC      END */



/* CC      FUNCTION UNIF( XJUNK ) */
/* CC      IMPLICIT REAL*8 (A-H,O-Z) */
/* CCC */
/* CCC     FACTOR - INTEGER OF THE FORM 8*K+5 AS CLOSE AS POSSIBLE */
/* CCC              TO  2**26 * (SQRT(5)-1)/2     (GOLDEN SECTION) */
/* CCC     TWO28  = 2**28  (I.E. 28 SIGNIFICANT BITS FOR DEVIATES) */
/* CCC */
/* CC      PARAMETER ( FACTOR = 41475557.0D+00 , TWO28 = 268435456.0D+00 ) */
/* CCC */
/* CC      DATA R / 0.D+00 / */
/* CCC */
/* CCC     RETURNS SAMPLE U FROM THE  0,1 -UNIFORM DISTRIBUTION */
/* CCC     BY A MULTIPLICATIVE CONGRUENTIAL GENERATOR OF THE FORM */
/* CCC        R := R * FACTOR (MOD 1) . */
/* CCC     IN THE FIRST CALL R IS INITIALIZED TO */
/* CCC        R := IR / 2**28 , */
/* CCC     WHERE IR MUST BE OF THE FORM  IR = 4*K+1. */
/* CCC     THEN R ASSUMES ALL VALUES  0 < (4*K+1)/2**28 < 1 DURING */
/* CCC     A FULL PERIOD 2**26 OF SUNIF. */
/* CCC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* CCC */
/* CC      IF( R .EQ. 0.D+00 ) R = 4000001.D+00 / TWO28 */
/* CC      R    = DMOD(R*FACTOR,1.0D+00) */
/* CC      UNIF = R */
/* CC      RETURN */
/* CC      END */



doublereal iran_(doublereal *top)
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Builtin functions */
    double d_int(doublereal *);

    /* Local variables */
    extern doublereal unif_(doublereal *);


/*  Return an integer uniformly distributed among 0..TOP */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
    d__1 = (*top + 1.) * unif_(&c_b430);
    ret_val = d_int(&d__1);
    return ret_val;
} /* iran_ */




doublereal eran_(doublereal *top)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double log(doublereal);

    /* Local variables */
    static doublereal u1;
    extern doublereal unif_(doublereal *);


/*  Return an exponentially distributed deviate: F(x) = 1-exp(-x/top) */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
L100:
    u1 = unif_(&c_b430);
    if (u1 <= 0.) {
	goto L100;
    }
    ret_val = -(*top) * log(u1);
    return ret_val;
} /* eran_ */




doublereal lran_(doublereal *top)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double log(doublereal);

    /* Local variables */
    static doublereal u1;
    extern doublereal unif_(doublereal *);


/*  Return a logistically distributed deviate: F(x) = 1/[1+exp(-x/top)] */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
L100:
    u1 = unif_(&c_b430);
    if (u1 <= 0. || u1 >= 1.) {
	goto L100;
    }
    ret_val = *top * log(1. / u1 - 1.);
    return ret_val;
} /* lran_ */




doublereal uran_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    extern doublereal unif_(doublereal *);


/*  Return a U(0,X) random variable. */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

    ret_val = *x * unif_(&c_b430);
    return ret_val;
} /* uran_ */




doublereal gran2_(doublereal *b, doublereal *s)
{
    /* Initialized data */

    static integer ip = 0;

    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double log(doublereal), sqrt(doublereal), sin(doublereal), cos(doublereal)
	    ;

    /* Local variables */
    static doublereal u1, u2;
    extern doublereal unif_(doublereal *);


/*  Compute a Gaussian random deviate with mean B and stdev S */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

    if (ip == 0) {
L100:
	u1 = unif_(&c_b430);
	if (u1 <= 0.) {
	    goto L100;
	}
	u2 = unif_(&c_b430);
	ret_val = *b + *s * sqrt(log(u1) * -2.) * sin(u2 * 6.2831853);
	ip = 1;
    } else {
	ret_val = *b + *s * sqrt(log(u1) * -2.) * cos(u2 * 6.2831853);
	ip = 0;
    }
    return ret_val;
} /* gran2_ */




doublereal gran1_(doublereal *b, doublereal *s)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static doublereal g;
    extern doublereal unif_(doublereal *);

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

    g = unif_(&c_b444) - 6. + unif_(&c_b445) + unif_(&c_b446) + unif_(&c_b447)
	     + unif_(&c_b448) + unif_(&c_b449) + unif_(&c_b450) + unif_(&
	    c_b451) + unif_(&c_b452) + unif_(&c_b453) + unif_(&c_b454) + 
	    unif_(&c_b455);
    ret_val = *b + *s * g;
    return ret_val;
} /* gran1_ */




doublereal gran_(doublereal *b, doublereal *s)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static doublereal uu;
    extern doublereal unif_(doublereal *), gran1_(doublereal *, doublereal *),
	     gran2_(doublereal *, doublereal *);

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

    uu = unif_(&c_b430);
    if (uu <= .5) {
	ret_val = gran1_(b, s);
    } else {
	ret_val = gran2_(b, s);
    }
    return ret_val;
} /* gran_ */




doublereal zzmod_(doublereal *a, doublereal *b)
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Builtin functions */
    double d_int(doublereal *);

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

    if (*b != 0.) {
	d__1 = *a / *b;
	ret_val = *a - *b * d_int(&d__1);
    } else {
	ret_val = 0.;
    }
    return ret_val;
} /* zzmod_ */




doublereal qginv_(doublereal *p)
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Builtin functions */
    double log(doublereal), sqrt(doublereal), exp(doublereal);

    /* Local variables */
    static doublereal dp, dq, dt, dx, ddq;
    static integer newt;
    extern doublereal derfc_(doublereal *);


/*  Return x such that Q(x)=P, for 0 < P < 1.  Q=reversed Gaussian cdf. */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

    dp = *p;
    if (dp > .5) {
	dp = 1. - dp;
    }
    if (dp <= 0.) {
	dx = 13.;
	goto L8000;
    }

/*  Step 1:  use 26.2.23 from Abramowitz and Stegun */

    dt = sqrt(log(dp) * -2.);
    dx = dt - ((dt * .010328 + .802853) * dt + 2.525517) / (((dt * .001308 + 
	    .189269) * dt + 1.432788) * dt + 1.);

/*  Step 2:  do 3 Newton steps to improve this */

    for (newt = 1; newt <= 3; ++newt) {
	d__1 = dx / 1.414213562373095;
	dq = derfc_(&d__1) * .5 - dp;
	ddq = exp(dx * -.5 * dx) / 2.506628274631;
	dx += dq / ddq;
/* L100: */
    }

L8000:
    if (*p > .5) {
	ret_val = -dx;
    } else {
	ret_val = dx;
    }

    return ret_val;
} /* qginv_ */




doublereal bell2_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Local variables */
    static doublereal ax;

/* ... */
    ax = abs(*x);
    if (ax <= .5) {
	ret_val = 1. - ax * 1.3333333333333333 * ax;
    } else if (ax <= 1.5) {
/* Computing 2nd power */
	d__1 = 1.5 - ax;
	ret_val = d__1 * d__1 * .666666666666667;
    } else {
	ret_val = 0.;
    }
    return ret_val;
} /* bell2_ */




doublereal rect_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static doublereal ax;

    ax = abs(*x);
    if (ax <= .5) {
	ret_val = 1.;
    } else {
	ret_val = 0.;
    }
    return ret_val;
} /* rect_ */




doublereal step_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    if (*x <= 0.) {
	ret_val = 0.;
    } else {
	ret_val = 1.;
    }
    return ret_val;
} /* step_ */




doublereal posval_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    if (*x <= 0.) {
	ret_val = 0.;
    } else {
	ret_val = *x;
    }
    return ret_val;
} /* posval_ */




doublereal tent_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static doublereal ax;

    ax = abs(*x);
    if (ax >= 1.) {
	ret_val = 0.;
    } else {
	ret_val = 1. - ax;
    }
    return ret_val;
} /* tent_ */




doublereal bool_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    if (*x == 0.) {
	ret_val = 0.;
    } else {
	ret_val = 1.;
    }
    return ret_val;
} /* bool_ */




doublereal land_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer i__;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    ret_val = 0.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (x[i__] == 0.) {
	    return ret_val;
	}
/* L100: */
    }
    ret_val = 1.;
    return ret_val;
} /* land_ */




/* Subroutine */ int bsort_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, it;
    static doublereal tmp;

/* ------------------------------------  Bubble sort */
    /* Parameter adjustments */
    --x;

    /* Function Body */
L50:
    it = 0;
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__ - 1] > x[i__]) {
	    tmp = x[i__];
	    x[i__] = x[i__ - 1];
	    x[i__ - 1] = tmp;
	    it = 1;
	}
/* L100: */
    }
    if (it != 0) {
	goto L50;
    }
    return 0;
} /* bsort_ */




doublereal orstat_(integer *m, integer *n, doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int bsort_(integer *, doublereal *);


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n <= 1) {
	ret_val = x[1];
	return ret_val;
    }

    i__ = *m;
    if (i__ <= 0) {
	i__ = 1;
    } else if (i__ > *n) {
	i__ = *n;
    }
    bsort_(n, &x[1]);
    ret_val = x[i__];
    return ret_val;
} /* orstat_ */




doublereal pairmx_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer i__, m;
    static doublereal pp, tt;


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n <= 2) {
	ret_val = x[2];
	return ret_val;
    }

    m = *n / 2;
    tt = x[1];
    pp = x[m + 1];
    i__1 = m;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] > tt) {
	    tt = x[i__];
	    pp = x[m + i__];
	}
    }
    ret_val = pp;
    return ret_val;
} /* pairmx_ */




doublereal pairmn_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer i__, m;
    static doublereal bb, pp;


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n <= 2) {
	ret_val = x[2];
	return ret_val;
    }

    m = *n / 2;
    bb = x[1];
    pp = x[m + 1];
    i__1 = m;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] < bb) {
	    bb = x[i__];
	    pp = x[m + i__];
	}
    }
    ret_val = pp;
    return ret_val;
} /* pairmn_ */




doublereal amongf_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer i__;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[1] == x[i__]) {
	    ret_val = 1.;
	    return ret_val;
	}
    }
    ret_val = 0.;
    return ret_val;
} /* amongf_ */




doublereal withinf_(integer *n, doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n < 1) {
	ret_val = 0.;
	return ret_val;
    }
    if (x[1] < x[2]) {
	ret_val = 0.;
	return ret_val;
    }
    if (x[1] > x[3]) {
	ret_val = 0.;
	return ret_val;
    }
    ret_val = 1.;
    return ret_val;
} /* withinf_ */




doublereal minabove_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer i__;
    static doublereal aaa, bbb;


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n < 1) {
	ret_val = 0.;
	return ret_val;
    }
    aaa = x[1];
    if (*n == 1) {
	ret_val = aaa;
	return ret_val;
    }
    bbb = 1e38;
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] > aaa && x[i__] < bbb) {
	    bbb = x[i__];
	}
    }
    if (bbb == 1e38) {
	bbb = aaa;
    }
    ret_val = bbb;
    return ret_val;
} /* minabove_ */




doublereal maxbelow_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer i__;
    static doublereal aaa, bbb;


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n < 1) {
	ret_val = 0.;
	return ret_val;
    }
    aaa = x[1];
    if (*n == 1) {
	ret_val = aaa;
	return ret_val;
    }
    bbb = -1e38;
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] < aaa && x[i__] > bbb) {
	    bbb = x[i__];
	}
    }
    if (bbb == -1e38) {
	bbb = aaa;
    }
    ret_val = bbb;
    return ret_val;
} /* maxbelow_ */




doublereal extreme_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val, d__1;

    /* Local variables */
    static integer i__;
    static doublereal aaa, bbb;


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n < 1) {
	ret_val = 0.;
	return ret_val;
    }
    aaa = x[1];
    if (*n == 1) {
	ret_val = aaa;
	return ret_val;
    }
    bbb = 0.f;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if ((d__1 = x[i__], abs(d__1)) > bbb) {
	    bbb = x[i__];
	}
    }
    if (bbb == 0.f) {
	bbb = aaa;
    }
    ret_val = bbb;
    return ret_val;
} /* extreme_ */




doublereal absextreme_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val, d__1, d__2;

    /* Local variables */
    static integer i__;
    static doublereal aaa, bbb;


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n < 1) {
	ret_val = 0.;
	return ret_val;
    }
    aaa = x[1];
    if (*n == 1) {
	ret_val = aaa;
	return ret_val;
    }
    bbb = 0.f;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if ((d__1 = x[i__], abs(d__1)) > bbb) {
	    bbb = (d__2 = x[i__], abs(d__2));
	}
    }
    if (bbb == 0.f) {
	bbb = aaa;
    }
    ret_val = bbb;
    return ret_val;
} /* absextreme_ */




doublereal choose_(integer *m, integer *n, doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*m < 1 || *n < *m) {
	ret_val = 0.;
	return ret_val;
    }
    ret_val = x[*m];
    return ret_val;
} /* choose_ */




doublereal mean_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer it;
    static doublereal tmp;


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n == 1) {
	ret_val = x[1];
	return ret_val;
    } else if (*n == 2) {
	ret_val = (x[1] + x[2]) * .5;
	return ret_val;
    }
    tmp = 0.;
    i__1 = *n;
    for (it = 1; it <= i__1; ++it) {
	tmp += x[it];
    }
    ret_val = tmp / *n;
    return ret_val;
} /* mean_ */




doublereal stdev_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val, d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer it;
    static doublereal tmp, xbar;


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n == 1) {
	ret_val = 0.;
	return ret_val;
    }
    tmp = 0.;
    i__1 = *n;
    for (it = 1; it <= i__1; ++it) {
	tmp += x[it];
    }
    xbar = tmp / *n;
    tmp = 0.;
    i__1 = *n;
    for (it = 1; it <= i__1; ++it) {
/* Computing 2nd power */
	d__1 = x[it] - xbar;
	tmp += d__1 * d__1;
    }
    ret_val = sqrt(tmp / (*n - 1.));
    return ret_val;
} /* stdev_ */




doublereal sem_(integer *n, doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    extern doublereal stdev_(integer *, doublereal *);


    /* Parameter adjustments */
    --x;

    /* Function Body */
    ret_val = stdev_(n, &x[1]) / sqrt(*n + 1e-6);
    return ret_val;
} /* sem_ */




doublereal median_(integer *n, doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static integer it;
    static doublereal tmp;
    extern /* Subroutine */ int bsort_(integer *, doublereal *);


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n == 1) {
	ret_val = x[1];
	return ret_val;
    } else if (*n == 2) {
	ret_val = (x[1] + x[2]) * .5;
	return ret_val;
    } else if (*n == 3) {
	if (x[1] > x[2]) {
	    tmp = x[2];
	    x[2] = x[1];
	    x[1] = tmp;
	}
	if (x[1] > x[3]) {
	    ret_val = x[1];
	} else if (x[2] > x[3]) {
	    ret_val = x[3];
	} else {
	    ret_val = x[2];
	}
	return ret_val;
    }

/* ---  sort it */

    bsort_(n, &x[1]);

/* ---  Even N --> average of middle 2 */
/* ---  Odd  N --> middle 1 */

    it = *n / 2;
    if (it << 1 == *n) {
	ret_val = (x[it] + x[it + 1]) * .5;
    } else {
	ret_val = x[it + 1];
    }
    return ret_val;
} /* median_ */




doublereal mad_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val, d__1;

    /* Local variables */
    static integer it;
    static doublereal tmp;
    extern doublereal median_(integer *, doublereal *);


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n == 1) {
	ret_val = 0.;
	return ret_val;
    } else if (*n == 2) {
	ret_val = (d__1 = x[1] - x[2], abs(d__1)) * .5;
	return ret_val;
    }

    tmp = median_(n, &x[1]);
    i__1 = *n;
    for (it = 1; it <= i__1; ++it) {
	x[it] = (d__1 = x[it] - tmp, abs(d__1));
/* L100: */
    }
    ret_val = median_(n, &x[1]);
    return ret_val;
} /* mad_ */




doublereal argmax_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer i__, it, nz;
    static doublereal tmp;


    /* Parameter adjustments */
    --x;

    /* Function Body */
    tmp = x[1];
    it = 1;
    nz = 0;
    if (tmp == 0.) {
	nz = 1;
    }
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] > tmp) {
	    it = i__;
	    tmp = x[i__];
	}
	if (x[i__] == 0.) {
	    ++nz;
	}
/* L100: */
    }
    if (nz == *n) {
	ret_val = 0.;
    } else {
	ret_val = (doublereal) it;
    }
    return ret_val;
} /* argmax_ */




doublereal argnum_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer i__, nz;


    /* Parameter adjustments */
    --x;

    /* Function Body */
    nz = 0;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (x[i__] != 0.) {
	    ++nz;
	}
/* L100: */
    }
    ret_val = (doublereal) nz;
    return ret_val;
} /* argnum_ */




doublereal hmode_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer i__, ib;
    static doublereal vb;
    static integer iv;
    static doublereal val;
    extern /* Subroutine */ int bsort_(integer *, doublereal *);


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n == 1) {
	ret_val = x[1];
	return ret_val;
    }

    bsort_(n, &x[1]);

    val = x[1];
    iv = 1;
    ib = 0;
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] != val) {
	    if (iv >= ib) {
		vb = val;
		ib = iv;
	    }
	    val = x[i__];
	    iv = 1;
	} else {
	    ++iv;
	}
/* L100: */
    }
    if (iv >= ib) {
	vb = val;
    }
    ret_val = vb;
    return ret_val;
} /* hmode_ */




doublereal lmode_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer i__, ib;
    static doublereal vb;
    static integer iv;
    static doublereal val;
    extern /* Subroutine */ int bsort_(integer *, doublereal *);


    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n == 1) {
	ret_val = x[1];
	return ret_val;
    }

    bsort_(n, &x[1]);

    val = x[1];
    iv = 1;
    ib = 0;
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] != val) {
	    if (iv > ib) {
		vb = val;
		ib = iv;
	    }
	    val = x[i__];
	    iv = 1;
	} else {
	    ++iv;
	}
/* L100: */
    }
    if (iv > ib) {
	vb = val;
    }
    ret_val = vb;
    return ret_val;
} /* lmode_ */




doublereal lor_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer i__;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    ret_val = 1.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (x[i__] != 0.) {
	    return ret_val;
	}
/* L100: */
    }
    ret_val = 0.;
    return ret_val;
} /* lor_ */




doublereal lmofn_(integer *m, integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer c__, i__;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    c__ = 0;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (x[i__] != 0.) {
	    ++c__;
	}
/* L100: */
    }
    if (c__ >= *m) {
	ret_val = 1.;
    } else {
	ret_val = 0.;
    }
    return ret_val;
} /* lmofn_ */




doublereal lncosh_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double exp(doublereal), log(doublereal);

    /* Local variables */
    static doublereal ax;

    ax = abs(*x);
    ret_val = ax + log(exp(ax * -2.) * .5 + .5);
    return ret_val;
} /* lncosh_ */




doublereal dai_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    extern /* Subroutine */ int qqqerr_(void);

    qqqerr_();
    ret_val = 0.;
    return ret_val;
} /* dai_ */

doublereal dbi_(doublereal *x, integer *i__)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    extern /* Subroutine */ int qqqerr_(void);

    qqqerr_();
    ret_val = 0.;
    return ret_val;
} /* dbi_ */

/* cc      REAL*8 FUNCTION  DGAMMA( X ) */
/* cc      REAL*8 X */
/* cc      CALL QQQERR */
/* cc      DGAMMA = 0.D+0 */
/* cc      RETURN */
/* Main program */ int MAIN__(void)
{
    return 0;
} /* MAIN__ */

doublereal dbesi0_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    extern /* Subroutine */ int qqqerr_(void);

    qqqerr_();
    ret_val = 0.;
    return ret_val;
} /* dbesi0_ */

doublereal dbesi1_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    extern /* Subroutine */ int qqqerr_(void);

    qqqerr_();
    ret_val = 0.;
    return ret_val;
} /* dbesi1_ */

/* cc      REAL*8 FUNCTION  DBESJ0( X ) */
/* cc      REAL*8 X */
/* cc      CALL QQQERR */
/* cc      END */
/* cc      REAL*8 FUNCTION  DBESJ1( X ) */
/* cc      REAL*8 X */
/* cc      CALL QQQERR */
/* cc      END */
doublereal dbesk0_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    extern /* Subroutine */ int qqqerr_(void);

    qqqerr_();
    ret_val = 0.;
    return ret_val;
} /* dbesk0_ */

doublereal dbesk1_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    extern /* Subroutine */ int qqqerr_(void);

    qqqerr_();
    ret_val = 0.;
    return ret_val;
} /* dbesk1_ */

/* cc      REAL*8 FUNCTION  DBESY0( X ) */
/* cc      REAL*8 X */
/* cc      CALL QQQERR */
/* cc      END */
/* cc      REAL*8 FUNCTION  DBESY1( X ) */
/* cc      REAL*8 X */
/* cc      CALL QQQERR */
/* cc      END */
/* cc      REAL*8 FUNCTION  DERF( X ) */
/* cc      REAL*8 X */
/* cc      CALL QQQERR */
/* cc      END */
/* cc      REAL*8 FUNCTION  DERFC( X ) */
/* cc      REAL*8 X */
/* cc      CALL QQQERR */
/* cc      END */

/* Subroutine */ int qqqerr_(void)
{
    /* Format strings */
    static char fmt_999[] = "(\002*** PARSER: unimplemented function ***\002)"
	    ;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(void);

    /* Fortran I/O blocks */
    static cilist io___145 = { 0, 6, 0, fmt_999, 0 };


    s_wsfe(&io___145);
    e_wsfe();
    return 0;
} /* qqqerr_ */

