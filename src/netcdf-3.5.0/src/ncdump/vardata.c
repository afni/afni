/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header$
 *********************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#ifndef NO_FLOAT_H
#include <float.h>		/* for FLT_EPSILON, DBL_EPSILON */
#endif /* NO_FLOAT_H */

#include <netcdf.h>
#include "ncdump.h"
#include "dumplib.h"
#include "vardata.h"

static float float_epsilon(void);
static double double_epsilon(void);
static void init_epsilons(void);
static void printbval(char* sout, const char* fmt, const struct ncvar* varp,
		      signed char val);
static void printsval(char* sout, const char* fmt, const struct ncvar* varp,
		      short val);
static void printival(char* sout, const char* fmt, const struct ncvar* varp,
		      int val);
static void printfval(char* sout, const char* fmt, const struct ncvar* varp,
		      float val);
static void printdval(char* sout, const char* fmt, const struct ncvar* varp,
		      double val);
static void lastdelim(boolean  more, boolean lastrow);
static void annotate(const struct ncvar* vp, const struct fspec* fsp,
		     const size_t* cor, long iel);
static void pr_tvals(const struct ncvar *vp, size_t len, const char *fmt,
		     boolean more, boolean lastrow, const char *vals,
		     const struct fspec* fsp, const size_t *cor);
static void pr_bvals(const struct ncvar *vp, size_t len, const char *fmt,
		     boolean more, boolean lastrow, const signed char *vals,
		     const struct fspec* fsp, const size_t *cor);
static void pr_svals(const struct ncvar *vp, size_t len, const char *fmt,
		     boolean more, boolean lastrow, const short *vals,
		     const struct fspec* fsp, const size_t *cor);
static void pr_ivals(const struct ncvar *vp, size_t len, const char *fmt,
		     boolean more, boolean lastrow, const int *vals,
		     const struct fspec* fsp, const size_t *cor);
static void pr_fvals(const struct ncvar *vp, size_t len, const char *fmt,
		     boolean more, boolean lastrow, const float *vals,
		     const struct fspec* fsp, const size_t *cor);
static void pr_dvals(const struct ncvar *vp, size_t len, const char *fmt,
		     boolean more, boolean lastrow, const double *vals,
		     const struct fspec* fsp, const size_t *cor);
static int  upcorner(const size_t* dims, int ndims, size_t* odom,
		     const size_t* add);
static void lastdelim2 (boolean more, boolean lastrow);

#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

static float float_eps;
static double double_eps;

static float
float_epsilon(void)
{
    float float_eps;
#ifndef NO_FLOAT_H
    float_eps = FLT_EPSILON;
#else /* NO_FLOAT_H */
    {
	float etop, ebot, eps;
	float one = 1.0;
	float two = 2.0;
	etop = 1.0;
	ebot = 0.0;
	eps = ebot + (etop - ebot)/two;
	while (eps != ebot && eps != etop) {
	    float epsp1;

	    epsp1 = one + eps;
	    if (epsp1 > one)
		etop = eps;
	    else
		ebot = eps;
	    eps = ebot + (etop - ebot)/two;
	}
	float_eps = two * etop;
    }
#endif /* NO_FLOAT_H */
    return float_eps;
}


static double
double_epsilon(void)
{
    double double_eps;
#ifndef NO_FLOAT_H
    double_eps = DBL_EPSILON;
#else /* NO_FLOAT_H */
    {
	double etop, ebot, eps;
	double one = 1.0;
	double two = 2.0;
	etop = 1.0;
	ebot = 0.0;
	eps = ebot + (etop - ebot)/two;
	while (eps != ebot && eps != etop) {
	    double epsp1;

	    epsp1 = one + eps;
	    if (epsp1 > one)
		etop = eps;
	    else
		ebot = eps;
	    eps = ebot + (etop - ebot)/two;
	}
	double_eps = two * etop;
    }
#endif /* NO_FLOAT_H */
    return double_eps;
}


static void
init_epsilons(void)
{
    float_eps = float_epsilon();
    double_eps = double_epsilon();
}

/*
 * Output a value of a byte variable, except if there is a fill value for
 * the variable and the value is the fill value, print the fill-value string
 * instead.
 */
static void
printbval(
    char *sout,			/* string where output goes */
    const char *fmt,		/* printf format used for value */
    const struct ncvar *varp,	/* variable */
    signed char val		/* value */
    )
{
    if (varp->has_fillval) {
	double fillval = varp->fillval;
	if(fillval == val) {
	    (void) sprintf(sout, FILL_STRING);
	    return;
	}
    }
    (void) sprintf(sout, fmt, val);
}

/*
 * Output a value of a short variable, except if there is a fill value for
 * the variable and the value is the fill value, print the fill-value string
 * instead.
 */
static void
printsval(
    char *sout,			/* string where output goes */
    const char *fmt,		/* printf format used for value */
    const struct ncvar *varp,		/* variable */
    short val			/* value */
    )
{
    if (varp->has_fillval) {
	double fillval = varp->fillval;
	if(fillval == val) {
	    (void) sprintf(sout, FILL_STRING);
	    return;
	}
    }
    (void) sprintf(sout, fmt, val);
}


/*
 * Output a value of an int variable, except if there is a fill value for
 * the variable and the value is the fill value, print the fill-value string
 * instead.
 */
static void
printival(
    char *sout,			/* string where output goes */
    const char *fmt,		/* printf format used for value */
    const struct ncvar *varp,		/* variable */
    int val			/* value */
    )
{
    if (varp->has_fillval) {
	int fillval = (int)varp->fillval;
	if(fillval == val) {
	    (void) sprintf(sout, FILL_STRING);
	    return;
	}
    }
    (void) sprintf(sout, fmt, val);
}


#define absval(x)  ( (x) < 0 ? -(x) : (x) )

/*
 * Output a value of a float variable, except if there is a fill value for
 * the variable and the value is the fill value, print the fill-value string
 * instead.  Floating-point fill values need only be within machine epsilon of
 * defined fill value.
 */
static void
printfval(
    char *sout,			/* string where output goes */
    const char *fmt,		/* printf format used for value */
    const struct ncvar *varp,		/* variable */
    float val			/* value */
    )
{
    if(varp->has_fillval) {
	double fillval = varp->fillval;
	if((val > 0) == (fillval > 0) && /* prevents potential overflow */
	   (absval(val - fillval) <= absval(float_eps * fillval))) {
	    (void) sprintf(sout, FILL_STRING);
	    return;
	}
    }
    (void) sprintf(sout, fmt, val);
}


/*
 * Output a value of a double variable, except if there is a fill value for
 * the variable and the value is the fill value, print the fill-value string
 * instead.  Floating-point fill values need only be within machine epsilon of
 * defined fill value.
 */
static void
printdval(
    char *sout,			/* string where output goes */
    const char *fmt,		/* printf format used for value */
    const struct ncvar *varp,		/* variable */
    double val			/* value */
    )
{
    if(varp->has_fillval) {
	double fillval = varp->fillval;
	if((val > 0) == (fillval > 0) && /* prevents potential overflow */
	   (absval(val - fillval) <= absval(double_eps * fillval))) {
	    (void) sprintf(sout, FILL_STRING);
	    return;
	}
    }
    (void) sprintf(sout, fmt, val);
}


/*
 * print last delimiter in each line before annotation (, or ;)
 */
static void
lastdelim (boolean more, boolean lastrow)
{
    if (more) {
	Printf(", ");
    } else {
	if(lastrow) {
	    Printf(";");
	} else {
	    Printf(",");
	}
    }
}

/*
 * print last delimiter in each line before annotation (, or ;)
 */
static void
lastdelim2 (boolean more, boolean lastrow)
{
    if (more) {
	lput(", ");
    } else {
	if(lastrow) {
	    lput(" ;");
	    lput("\n");
	} else {
	    lput(",\n");
	    lput("  ");
	}
    }
}


/*
 * Annotates a value in data section with var name and indices in comment
 */
static void
annotate(
     const struct ncvar *vp,	/* variable */
     const struct fspec* fsp,	/* formatting specs */
     const size_t *cor,		/* corner coordinates */
     long iel			/* which element in current row */
     )
{
    int vrank = vp->ndims;
    int id;
    
    /* print indices according to data_lang */
    (void) printf("  // %s(", vp->name);
    switch (fsp->data_lang) {
      case LANG_C:
	/* C variable indices */
	for (id = 0; id < vrank-1; id++)
	  Printf("%lu,", (unsigned long) cor[id]);
	Printf("%lu", (unsigned long) cor[id] + iel);
	break;
      case LANG_F:
	/* Fortran variable indices */
	Printf("%lu", (unsigned long) cor[vrank-1] + iel + 1);
	for (id = vrank-2; id >=0 ; id--) {
	    Printf(",%lu", 1 + (unsigned long) cor[id]);
	}
	break;
    }
    Printf(")\n    ");
}


/*
 * Print a number of char variable values, where the optional comments
 * for each value identify the variable, and each dimension index.
 */
static void
pr_tvals(
     const struct ncvar *vp,		/* variable */
     size_t len,		/* number of values to print */
     const char *fmt,		/* printf format used for each value.  If
				 * nc_type is NC_CHAR and this is NULL,
				 * character arrays will be printed as
				 * strings enclosed in quotes.  */
     boolean more,		/* true if more data for this row will
				 * follow, so add trailing comma */
     boolean lastrow,		/* true if this is the last row for this
				 * variable, so terminate with ";" instead
				 * of "," */
     const char *vals,		/* pointer to block of values */
     const struct fspec* fsp,	/* formatting specs */
     const size_t *cor		/* corner coordinates */
     )
{
    long iel;
    const char *sp;
    unsigned char uc;
    char sout[100];		/* temporary string for each encoded output */

    if (fmt == 0 || STREQ(fmt,"%s") || STREQ(fmt,"")) { /* as string */
	Printf("\"");
	/* adjust len so trailing nulls don't get printed */
	sp = vals + len;
	while (len != 0 && *--sp == '\0')
	    len--;
	for (iel = 0; iel < len; iel++)
	    switch (uc = *vals++ & 0377) {
	    case '\b':
		Printf("\\b");
		break;
	    case '\f':
		Printf("\\f");
		break;
	    case '\n':	/* generate linebreaks after new-lines */
		Printf("\\n\",\n    \"");
		break;
	    case '\r':
		Printf("\\r");
		break;
	    case '\t':
		Printf("\\t");
		break;
	    case '\v':
		Printf("\\v");
		break;
	    case '\\':
		Printf("\\\\");
		break;
	    case '\'':
		Printf("\\\'");
		break;
	    case '\"':
		Printf("\\\"");
		break;
	    default:
		if (isprint(uc))
		    Printf("%c",uc);
		else
		    Printf("\\%.3o",uc);
		break;
	    }
	Printf("\"");
	if (fsp->full_data_cmnts) {
	    Printf("\"");
	    lastdelim (more, lastrow);
	    annotate (vp, fsp,  (size_t *)cor, 0L);
	}
    } else {		/* use format from C_format attribute */
	for (iel = 0; iel < len-1; iel++) {
	    if (fsp->full_data_cmnts) {
		Printf(fmt, *vals++);
		Printf(", ");
		annotate (vp, fsp,  (size_t *)cor, iel);
	    } else {
		(void) sprintf(sout, fmt, *vals++);
		(void) strcat(sout, ", ");
		lput(sout);
	    }
	}
	if (fsp->full_data_cmnts) {
	    Printf(fmt, *vals++);
	    lastdelim (more, lastrow);
	    annotate (vp, fsp,  (size_t *)cor, iel);
	} else {
	    (void) sprintf(sout, fmt, *vals++);
	    lput(sout);
	}
    }
    if (!fsp->full_data_cmnts) {
	lastdelim2 (more, lastrow);
    }
}


/*
 * Print a number of byte variable values, where the optional comments
 * for each value identify the variable, and each dimension index.
 */
static void
pr_bvals(
     const struct ncvar *vp,	/* variable */
     size_t len,		/* number of values to print */
     const char *fmt,		/* printf format used for each value.  If
				 * nc_type is NC_CHAR and this is NULL,
				 * character arrays will be printed as
				 * strings enclosed in quotes.  */
     boolean more,		/* true if more data for this row will
				 * follow, so add trailing comma */
     boolean lastrow,		/* true if this is the last row for this
				 * variable, so terminate with ";" instead
				 * of "," */
     const signed char *vals,	/* pointer to block of values */
     const struct fspec* fsp,	/* formatting specs */
     const size_t *cor		/* corner coordinates */
     )
{
    long iel;
    char sout[100];		/* temporary string for each encoded output */

    for (iel = 0; iel < len-1; iel++) {
	printbval(sout, fmt, vp, *vals++);
	if (fsp->full_data_cmnts) {
	    Printf(sout);
	    Printf(",");
	    annotate (vp, fsp, cor, iel);
	} else {
	    (void) strcat(sout, ", ");
	    lput(sout);
	}
    }
    printbval(sout, fmt, vp, *vals++);
    if (fsp->full_data_cmnts) {
	Printf(sout);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
    } else {
	lput(sout);
	lastdelim2 (more, lastrow);
    }
}


/*
 * Print a number of short variable values, where the optional comments
 * for each value identify the variable, and each dimension index.
 */
static void
pr_svals(
     const struct ncvar *vp,		/* variable */
     size_t len,		/* number of values to print */
     const char *fmt,		/* printf format used for each value.  If
				 * nc_type is NC_CHAR and this is NULL,
				 * character arrays will be printed as
				 * strings enclosed in quotes.  */
     boolean more,		/* true if more data for this row will
				 * follow, so add trailing comma */
     boolean lastrow,		/* true if this is the last row for this
				 * variable, so terminate with ";" instead
				 * of "," */
     const short *vals,		/* pointer to block of values */
     const struct fspec* fsp,	/* formatting specs */
     const size_t *cor		/* corner coordinates */
     )
{
    long iel;
    char sout[100];		/* temporary string for each encoded output */

    for (iel = 0; iel < len-1; iel++) {
	printsval(sout, fmt, vp, *vals++);
	if (fsp->full_data_cmnts) {
	    Printf(sout);
	    Printf(",");
	    annotate (vp, fsp, cor, iel);
	} else {
	    (void) strcat(sout, ", ");
	    lput(sout);
	}
    }
    printsval(sout, fmt, vp, *vals++);
    if (fsp->full_data_cmnts) {
	Printf(sout);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
    } else {
	lput(sout);
	lastdelim2 (more, lastrow);
    }
}




/*
 * Print a number of int variable values, where the optional comments
 * for each value identify the variable, and each dimension index.
 */
static void
pr_ivals(
     const struct ncvar *vp,		/* variable */
     size_t len,		/* number of values to print */
     const char *fmt,		/* printf format used for each value.  If
				 * nc_type is NC_CHAR and this is NULL,
				 * character arrays will be printed as
				 * strings enclosed in quotes.  */
     boolean more,		/* true if more data for this row will
				 * follow, so add trailing comma */
     boolean lastrow,		/* true if this is the last row for this
				 * variable, so terminate with ";" instead
				 * of "," */
     const int *vals,		/* pointer to block of values */
     const struct fspec* fsp,	/* formatting specs */
     const size_t *cor		/* corner coordinates */
     )
{
    long iel;
    char sout[100];		/* temporary string for each encoded output */

    for (iel = 0; iel < len-1; iel++) {
	printival(sout, fmt, vp, *vals++);
	if (fsp->full_data_cmnts) {
	    Printf(sout);
	    Printf(",");
	    annotate (vp, fsp, cor, iel);
	} else {
	    (void) strcat(sout, ", ");
	    lput(sout);
	}
    }
    printival(sout, fmt, vp, *vals++);
    if (fsp->full_data_cmnts) {
	Printf(sout);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
    } else {
	lput(sout);
	lastdelim2 (more, lastrow);
    }
}


/*
 * Print a number of float variable values, where the optional comments
 * for each value identify the variable, and each dimension index.
 */
static void
pr_fvals(
     const struct ncvar *vp,		/* variable */
     size_t len,			/* number of values to print */
     const char *fmt,		/* printf format used for each value.  If
				 * nc_type is NC_CHAR and this is NULL,
				 * character arrays will be printed as
				 * strings enclosed in quotes.  */
     boolean more,		/* true if more data for this row will
				 * follow, so add trailing comma */
     boolean lastrow,		/* true if this is the last row for this
				 * variable, so terminate with ";" instead
				 * of "," */
     const float *vals,		/* pointer to block of values */
     const struct fspec* fsp,	/* formatting specs */
     const size_t *cor		/* corner coordinates */
     )
{
    long iel;
    char sout[100];		/* temporary string for each encoded output */

    for (iel = 0; iel < len-1; iel++) {
	printfval(sout, fmt, vp, *vals++);
	if (fsp->full_data_cmnts) {
	    Printf(sout);
	    Printf(",");
	    annotate (vp, fsp, cor, iel);
	} else {
	    (void) strcat(sout, ", ");
	    lput(sout);
	}
    }
    printfval(sout, fmt, vp, *vals++);
    if (fsp->full_data_cmnts) {
	Printf(sout);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
    } else {
	lput(sout);
	lastdelim2 (more, lastrow);
    }
}


/*
 * Print a number of double variable values, where the optional comments
 * for each value identify the variable, and each dimension index.
 */
static void
pr_dvals(
     const struct ncvar *vp,		/* variable */
     size_t len,			/* number of values to print */
     const char *fmt,		/* printf format used for each value.  If
				 * nc_type is NC_CHAR and this is NULL,
				 * character arrays will be printed as
				 * strings enclosed in quotes.  */
     boolean more,		/* true if more data for this row will
				 * follow, so add trailing comma */
     boolean lastrow,		/* true if this is the last row for this
				 * variable, so terminate with ";" instead
				 * of "," */
     const double *vals,	/* pointer to block of values */
     const struct fspec* fsp,	/* formatting specs */
     const size_t *cor		/* corner coordinates */
     )
{
    long iel;
    char sout[100];		/* temporary string for each encoded output */

    for (iel = 0; iel < len-1; iel++) {
	printdval(sout, fmt, vp, *vals++);
	if (fsp->full_data_cmnts) {
	    Printf(sout);
	    Printf(",");
	    annotate (vp, fsp, cor, iel);
	} else {
	    (void) strcat(sout, ", ");
	    lput(sout);
	}
    }
    printdval(sout, fmt, vp, *vals++);
    if (fsp->full_data_cmnts) {
	Printf(sout);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
    } else {
	lput(sout);
	lastdelim2 (more, lastrow);
    }
}


/*
 * Updates a vector of ints, odometer style.  Returns 0 if odometer
 * overflowed, else 1.
 */
static int
upcorner(
     const size_t *dims,	/* The "odometer" limits for each dimension */
     int ndims,			/* Number of dimensions */
     size_t* odom,		/* The "odometer" vector to be updated */
     const size_t* add		/* A vector to "add" to odom on each update */
     )
{
    int id;
    int ret = 1;

    for (id = ndims-1; id > 0; id--) {
	odom[id] += add[id];
	if(odom[id] >= dims[id]) {
	    odom[id-1]++;
	    odom[id] -= dims[id];
	}
    }
    odom[0] += add[0];
    if (odom[0] >= dims[0])
      ret = 0;
    return ret;
}


/* Output the data for a single variable, in CDL syntax. */
int
vardata(
     const struct ncvar *vp,	/* variable */
     size_t vdims[],		/* variable dimension sizes */
     int ncid,			/* netcdf id */
     int varid,			/* variable id */
     const struct fspec* fsp	/* formatting specs */
     )
{
    size_t cor[NC_MAX_DIMS];	/* corner coordinates */
    size_t edg[NC_MAX_DIMS];	/* edges of hypercube */
    size_t add[NC_MAX_DIMS];	/* "odometer" increment to next "row"  */
#define VALBUFSIZ 1000
    double vals[VALBUFSIZ] ; /* aligned buffer */

    int gulp = VALBUFSIZ;

    int id;
    int ir;
    size_t nels;
    size_t ncols;
    size_t nrows;
    int vrank = vp->ndims;
    static int initeps = 0;

    /* printf format used to print each value */
    char *fmt = get_fmt(ncid, varid, vp->type);

    if (!initeps) {		/* make sure epsilons get initialized */
	init_epsilons();
	initeps = 1;
    }

    nels = 1;
    for (id = 0; id < vrank; id++) {
	cor[id] = 0;
	edg[id] = 1;
	nels *= vdims[id];	/* total number of values for variable */
    }

    if (vrank <= 1) {
	Printf("\n %s = ", vp->name);
	set_indent ((int)strlen(vp->name) + 4);
    } else {
	Printf("\n %s =\n  ", vp->name);
	set_indent (2);
    }

    if (vrank < 1) {
	ncols = 1;
    } else {
	ncols = vdims[vrank-1];	/* size of "row" along last dimension */
	edg[vrank-1] = vdims[vrank-1];
	for (id = 0; id < vrank; id++)
	  add[id] = 0;
	if (vrank > 1)
	  add[vrank-2] = 1;
    }
    nrows = nels/ncols;		/* number of "rows" */
    
    for (ir = 0; ir < nrows; ir++) {
	/*
	 * rather than just printing a whole row at once (which might exceed
	 * the capacity of MSDOS platforms, for example), we break each row
	 * into smaller chunks, if necessary.
	 */
	size_t corsav;
	int left = (int)ncols;
	boolean lastrow;

	if (vrank > 0) {
	    corsav = cor[vrank-1];
	    if (fsp->brief_data_cmnts != false
		&& vrank > 1
		&& left > 0) {	/* print brief comment with indices range */
		Printf("// %s(",vp->name);
		switch (fsp->data_lang) {
		  case LANG_C:
		    /* print brief comment with C variable indices */
		    for (id = 0; id < vrank-1; id++)
		      Printf("%lu,", (unsigned long)cor[id]);
		    if (vdims[vrank-1] == 1)
		      Printf("0");
		    else
		      Printf(" 0-%lu", (unsigned long)vdims[vrank-1]-1);
		    break;
		  case LANG_F:
		    /* print brief comment with Fortran variable indices */
		    if (vdims[vrank-1] == 1)
		      Printf("1");
		    else
		      Printf("1-%lu ", (unsigned long)vdims[vrank-1]);
		    for (id = vrank-2; id >=0 ; id--) {
			Printf(",%lu", (unsigned long)(1 + cor[id]));
		    }
		    break;
		}
		Printf(")\n    ");
		set_indent(4);
	    }
	}
	lastrow = (boolean)(ir == nrows-1);
	while (left > 0) {
	    size_t toget = left < gulp ? left : gulp;
	    if (vrank > 0)
	      edg[vrank-1] = toget;
	    switch(vp->type) {
	    case NC_CHAR:
		NC_CHECK(
		    nc_get_vara_text(ncid, varid, cor, edg, (char *)vals) );
	        pr_tvals(vp, toget, fmt, left > toget, lastrow,
			 (char *) vals, fsp, cor);
		break;
	    case NC_BYTE:
		NC_CHECK(
		    nc_get_vara_schar(ncid, varid, cor, edg, (signed char *)vals) );
	        pr_bvals(vp, toget, fmt, left > toget, lastrow,
			 (signed char *) vals, fsp, cor);
		break;
	    case NC_SHORT:
		NC_CHECK(
		    nc_get_vara_short(ncid, varid, cor, edg, (short *)vals) );
	        pr_svals(vp, toget, fmt, left > toget, lastrow,
			 (short *) vals, fsp, cor);
		break;
	    case NC_INT:
		NC_CHECK(
		    nc_get_vara_int(ncid, varid, cor, edg, (int *)vals) );
	        pr_ivals(vp, toget, fmt, left > toget, lastrow,
			 (int *) vals, fsp, cor);
		break;
	    case NC_FLOAT:
		NC_CHECK(
		    nc_get_vara_float(ncid, varid, cor, edg, (float *)vals) );
	        pr_fvals(vp, toget, fmt, left > toget, lastrow,
			 (float *) vals, fsp, cor);
		break;
	    case NC_DOUBLE:
		NC_CHECK(
		    nc_get_vara_double(ncid, varid, cor, edg, (double *)vals) );
	        pr_dvals(vp, toget, fmt, left > toget, lastrow,
			 (double *) vals, fsp, cor);
		break;
	    default:
		error("vardata: bad type");
	    }
	    left -= toget;
	    if (vrank > 0)
	      cor[vrank-1] += toget;
	}
	if (vrank > 0)
	  cor[vrank-1] = corsav;
	if (ir < nrows-1)
	  if (!upcorner(vdims,vp->ndims,cor,add))
	    error("vardata: odometer overflowed!");
	set_indent(2);
    }

    return 0;
}
