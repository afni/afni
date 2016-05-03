dnl This is m4 source.
dnl Process using m4 to produce 'C' language file.
dnl
dnl If you see this line, you can ignore the next one.
/* Do not edit this file. It is produced from the corresponding .m4 source */
dnl
/*
 *	Copyright 1996, University Corporation for Atmospheric Research
 *	See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */

/*
 * This program tests the aggregate external representation conversion
 * functions "ncx_[pad_]{put,get}n_*()" declared in ncx.h.
 * Unlike t_ncx.c, it only checks self consistancy,
 * not consistancy with the xdr library.
 *
 * Link like this: 
 * cc t_ncxx.c ncx.o -o t_nxc
 * (The xdr library is not needed.)
 *
 * If an assertion fails, there is a problem.
 * Otherwise, the program is silent and has exit code 0.
 */ 

#undef NDEBUG   /* always active assert() in this file */

#include <stdio.h>
#include <string.h>
#include <limits.h>
/* alias poorly named limits.h macros */
#define  SHORT_MAX  SHRT_MAX
#define  SHORT_MIN  SHRT_MIN
#define USHORT_MAX USHRT_MAX
#include <assert.h>
#include "ncx.h"
#define X_SIZEOF_SCHAR X_SIZEOF_CHAR
#define X_LONG_MAX X_INT_MAX
#define X_LONG_MIN X_INT_MIN

#define XBSZ 1024

char ncxb[XBSZ];
char lbuf[XBSZ];

#define ArraySize(thang) (sizeof(thang)/sizeof(thang[0]))
#define eSizeOf(thang) ((size_t)(sizeof(thang[0])))

/*
 * Some test data
 * The ideas is that ncx_putn_type_type(...., types)
 * should not return NC_ERANGE.
 */

#if SCHAR_MAX == X_SCHAR_MAX && SCHAR_MIN == X_SCHAR_MIN
static schar schars[] = {
	SCHAR_MIN, SCHAR_MIN +1,
	-1, 0, 1,
	SCHAR_MAX - 1, SCHAR_MAX
};
#else
/* The implementation and this test assume 8 bit bytes. */
#error "Not 8 bit bytes ??"
#endif

static short shorts[] = {
#if SHORT_MAX <= X_SHORT_MAX
	SHORT_MIN, SHORT_MIN + 1,
#  if SCHAR_MAX < X_SHORT_MAX
	SCHAR_MIN - 1, SCHAR_MIN, SCHAR_MIN + 1,
#  endif
	-1, 0, 1,
#  if SCHAR_MAX < X_SHORT_MAX
	SCHAR_MAX - 1, SCHAR_MAX, SCHAR_MAX + 1,
#  endif
	SHORT_MAX - 1, SHORT_MAX
#else
	X_SHORT_MIN, X_SHORT_MIN + 1,
#  if SCHAR_MAX < X_SHORT_MAX
	SCHAR_MIN - 1, SCHAR_MIN, SCHAR_MIN + 1,
#  endif
	-1, 0, 1,
#  if SCHAR_MAX < X_SHORT_MAX
	SCHAR_MAX - 1, SCHAR_MAX, SCHAR_MAX + 1,
#  endif
	X_SHORT_MAX - 1, X_SHORT_MAX
#endif
};

static int ints[] = {
#if INT_MAX <= X_INT_MAX
	INT_MIN, INT_MIN +1,
#  if SHORT_MAX < X_INT_MAX
	SHORT_MIN -1, SHORT_MIN, SHORT_MIN + 1,
#  endif
#  if SCHAR_MAX < X_INT_MAX
	SCHAR_MIN - 1, SCHAR_MIN, SCHAR_MIN + 1,
#  endif
	-1, 0, 1,
#  if SCHAR_MAX < X_INT_MAX
	SCHAR_MAX - 1, SCHAR_MAX, SCHAR_MAX + 1,
#  endif
#  if SHORT_MAX < X_INT_MAX
	SHORT_MAX - 1, SHORT_MAX, SHORT_MAX +1,
#  endif
	INT_MAX - 1, INT_MAX
#else
	X_INT_MIN, X_INT_MIN +1,
#  if SHORT_MAX < X_INT_MAX
	SHORT_MIN -1, SHORT_MIN, SHORT_MIN + 1,
#  endif
#  if SCHAR_MAX < X_INT_MAX
	SCHAR_MIN - 1, SCHAR_MIN, SCHAR_MIN + 1,
#  endif
	-1, 0, 1,
#  if SCHAR_MAX < X_INT_MAX
	SCHAR_MAX - 1, SCHAR_MAX, SCHAR_MAX + 1,
#  endif
#  if SHORT_MAX < X_INT_MAX
	SHORT_MAX - 1, SHORT_MAX, SHORT_MAX +1,
#  endif
	X_INT_MAX - 1, X_INT_MAX
#endif /* INT */
};


/* N.B. only testing longs over X_INT range for now */
static long longs[] = {
#if LONG_MAX <= X_INT_MAX
	LONG_MIN, LONG_MIN +1,
#  if INT_MAX < X_INT_MAX
	INT_MIN -1, INT_MIN, INT_MIN + 1,
#  endif
#  if SHORT_MAX < X_INT_MAX
	SHORT_MIN -1, SHORT_MIN, SHORT_MIN + 1,
#  endif
#  if SCHAR_MAX < X_INT_MAX
	SCHAR_MIN - 1, SCHAR_MIN, SCHAR_MIN + 1,
#  endif
	-1, 0, 1,
#  if SCHAR_MAX < X_INT_MAX
	SCHAR_MAX - 1, SCHAR_MAX, SCHAR_MAX + 1,
#  endif
#  if SHORT_MAX < X_INT_MAX
	SHORT_MAX - 1, SHORT_MAX, SHORT_MAX +1,
#  endif
#  if INT_MAX < X_INT_MAX
	INT_MAX -1, INT_MAX, INT_MAX + 1,
#  endif
	LONG_MAX - 1, LONG_MAX
#else
	X_INT_MIN, X_INT_MIN +1,
#  if SHORT_MAX < X_INT_MAX
	SHORT_MIN -1, SHORT_MIN, SHORT_MIN + 1,
#  endif
#  if SCHAR_MAX < X_INT_MAX
	SCHAR_MIN - 1, SCHAR_MIN, SCHAR_MIN + 1,
#  endif
	-1, 0, 1,
#  if SCHAR_MAX < X_INT_MAX
	SCHAR_MAX - 1, SCHAR_MAX, SCHAR_MAX + 1,
#  endif
#  if SHORT_MAX < X_INT_MAX
	SHORT_MAX - 1, SHORT_MAX, SHORT_MAX +1,
#  endif
	X_INT_MAX - 1, X_INT_MAX
#endif
};

static float floats[] = {
	-1.E9F,
	-16777215, -16777214,
	-999999,
	-32769, -32768, -32767,
	-129, -128, 127,
	-1, 0, 1,
	126, 127, 128,
	32766, 32767, 32768,
	999999,
	16777214, 16777215,	/* 2^24 -1 */
	1.E9F
};

static double doubles[] = {
	-1.E20,
	-4503599627370495., -4503599627370494.,
	-999999999999999.,
	-1.E9,
	-16777215, -16777214,
	-999999,
	-32769, -32768, -32767,
	-129, -128, 127,
	-1, 0, 1,
	126, 127, 128,
	32766, 32767, 32768,
	999999,
	16777214, 16777215,	/* 2^24 -1 */
	1.E9,
	999999999999999.,
	4503599627370494., 4503599627370495.,	/* 2^53 -1 */
	1.E20
};

static uchar uchars[] = {
	0, 1,
	UCHAR_MAX/2 -1, UCHAR_MAX/2, UCHAR_MAX/2 +1,
	UCHAR_MAX - 1, UCHAR_MAX
};

/* End of test data */

dnl dnl dnl
dnl
dnl Macros
dnl
dnl dnl dnl
dnl
dnl Upcase(str)
dnl
define(`Upcase',dnl
`dnl
translit($1, abcdefghijklmnopqrstuvwxyz, ABCDEFGHIJKLMNOPQRSTUVWXYZ)')dnl
dnl dnl dnl
dnl
dnl Xsizeof(Xtype)
dnl
define(`Xsizeof', ``X_SIZEOF_'Upcase($1)')dnl
dnl dnl dnl
define(`XMin', ``X_'Upcase($1)`_MIN'')dnl
define(`XMax', ``X_'Upcase($1)`_MAX'')dnl
dnl dnl dnl
dnl
dnl T_PUTN(XType, Type)
dnl
define(`T_PUTN',dnl
`dnl
static void 
t_putn_$1_$2(char *const buf)
{
	char *xp = buf; 
	const $2 *tp = `$2's;
	size_t nelems = ArraySize(`$2's);
	int status = ncx_putn_$1_$2((void **)&xp, nelems, tp);
	assert(xp == buf + nelems * Xsizeof($1));
	{
		size_t ii;
		for(ii = 0; ii < nelems; ii++)
		{
			if((double) tp[ii] > XMax($1))
			{
				assert(status == NC_ERANGE);
				return;
			}
			if((double) tp[ii] < XMin($1))
			{
				assert(status == NC_ERANGE);
				return;
			}
		}
		assert(status == 0);
	}
}
')dnl
dnl dnl dnl
dnl
dnl T_PUTN_U(XType, Type)
dnl Doesn't make signed comparisons to unsigned type
dnl
define(`T_PUTN_U',dnl
`dnl
static void 
t_putn_$1_$2(char *const buf)
{
	char *xp = buf; 
	const $2 *tp = `$2's;
	size_t nelems = ArraySize(`$2's);
	int status = ncx_putn_$1_$2((void **)&xp, nelems, tp);
	assert(xp == buf + nelems * Xsizeof($1));
	{
		size_t ii;
		for(ii = 0; ii < nelems; ii++)
		{
			if((double) tp[ii] > XMax($1))
			{
				assert(status == NC_ERANGE);
				return;
			}
		}
		assert(status == 0);
	}
}
')dnl
dnl dnl dnl
dnl
dnl T_PAD_PUTN(XType, Type)
dnl
define(`T_PAD_PUTN',dnl
`dnl
static void 
t_pad_putn_$1_$2(char *const buf)
{
	char *xp = buf; 
	const $2 *tp = `$2's;
	size_t nelems = ArraySize(`$2's);
	const char *end = buf + nelems * Xsizeof($1);
	int status = ncx_pad_putn_$1_$2((void **)&xp, nelems, tp);
	assert(xp >= end);
	assert((xp - end)  < 4);
	assert((xp - buf)%4 == 0);
	{
		size_t ii;
		for(ii = 0; ii < nelems; ii++)
		{
			if((double) tp[ii] > XMax($1))
			{
				assert(status == NC_ERANGE);
				return;
			}
			if((double) tp[ii] < XMin($1))
			{
				assert(status == NC_ERANGE);
				return;
			}
		}
		assert(status == 0);
	}
}
')dnl
dnl dnl dnl
dnl
dnl T_GETN(XType, Type)
dnl
define(`T_GETN',dnl
`dnl
static void 
t_getn_$1_$2(const char *const buf)
{
	const char *xp = buf; 
	const $2 *tp = `$2's;
	$2 *lp = ($2 *)lbuf;
	size_t nelems = ArraySize(`$2's);
	int status = ncx_getn_$1_$2((const void **)&xp, nelems, lp);
	assert(xp == buf + nelems * Xsizeof($1));
	assert(status == 0);

	{
		size_t ii;
		for(ii = 0; ii < nelems; ii++)
		{
			if(((double)tp[ii] <= XMax($1))
				&& ((double)tp[ii] >= XMin($1)))
			{
				assert(tp[ii] == lp[ii]);
			}
		}
	}
}
')dnl
dnl dnl dnl
dnl
dnl T_GETN_U(XType, Type)
dnl Doesn't make signed comparisons to unsigned type
dnl
define(`T_GETN_U',dnl
`dnl
static void 
t_getn_$1_$2(const char *const buf)
{
	const char *xp = buf; 
	const $2 *tp = `$2's;
	$2 *lp = ($2 *)lbuf;
	size_t nelems = ArraySize(`$2's);
	int status = ncx_getn_$1_$2((const void **)&xp, nelems, lp);
	assert(xp == buf + nelems * Xsizeof($1));
	assert(status == 0);

	{
		size_t ii;
		for(ii = 0; ii < nelems; ii++)
		{
			if((double) tp[ii] <= XMax($1))
			{
				assert(tp[ii] == lp[ii]);
			}
		}
	}
}
')dnl
dnl dnl dnl
dnl
dnl T_PAD_GETN(XType, Type)
dnl
define(`T_PAD_GETN',dnl
`dnl
static void 
t_pad_getn_$1_$2(const char *const buf)
{
	const char *xp = buf; 
	const $2 *tp = `$2's;
	$2 *lp = ($2 *)lbuf;
	size_t nelems = ArraySize(`$2's);
	const char *end = buf + nelems * Xsizeof($1);
	int status = ncx_pad_getn_$1_$2((const void **)&xp, nelems, lp);
	assert(xp >= end);
	assert((xp - end)  < 4);
	assert((xp - buf)%4 == 0);
	assert(status == 0);

	{
		size_t ii;
		for(ii = 0; ii < nelems; ii++)
		{
			if(((double) tp[ii] <= XMax($1))
				&& ((double) tp[ii] >= XMin($1)))
			{
				assert(tp[ii] == lp[ii]);
			}
		}
	}
}
')dnl

dnl dnl dnl
dnl
dnl Declare & define test routines
dnl
dnl dnl dnl

T_PUTN(schar, schar)
dnl T_PUTN(schar, uchar) replaced by special case code.
dnl - we don't return conversion errors putting uchar to schar.
static void 
t_putn_schar_uchar(char *const buf)
{
	char *xp = buf; 
	const uchar *tp = uchars;
	size_t nelems = ArraySize(schars);
	int status = ncx_putn_schar_uchar((void **)&xp, nelems, tp);
	assert(xp == buf + nelems * X_SIZEOF_SCHAR);
	assert(status == 0);
}

T_PUTN(schar, short)
T_PUTN(schar, int)
T_PUTN(schar, long)
T_PUTN(schar, float)
T_PUTN(schar, double)

T_PAD_PUTN(schar, schar)
dnl T_PAD_PUTN(schar, uchar) replaced by special case code.
dnl - we don't return conversion errors putting uchar to schar.
static void 
t_pad_putn_schar_uchar(char *const buf)
{
	char *xp = buf; 
	const uchar *tp = uchars;
	size_t nelems = ArraySize(uchars);
	const char *end = buf + nelems * X_SIZEOF_SCHAR;
	int status = ncx_pad_putn_schar_uchar((void **)&xp, nelems, tp);
	assert(xp >= end);
	assert((xp - end)  < 4);
	assert((xp - buf)%4 == 0);
	assert(status == 0);
}

T_PAD_PUTN(schar, short)
T_PAD_PUTN(schar, int)
T_PAD_PUTN(schar, long)
T_PAD_PUTN(schar, float)
T_PAD_PUTN(schar, double)

T_PUTN(short, schar)
T_PUTN_U(short, uchar)
T_PUTN(short, short)
T_PUTN(short, int)
T_PUTN(short, long)
T_PUTN(short, float)
T_PUTN(short, double)

T_PAD_PUTN(short, schar)
dnl T_PAD_PUTN(short, uchar)
dnl Don't make signed comparisons to usigned type
static void 
t_pad_putn_short_uchar(char *const buf)
{
	char *xp = buf; 
	const uchar *tp = uchars;
	size_t nelems = ArraySize(uchars);
	const char *end = buf + nelems * X_SIZEOF_SHORT;
	int status = ncx_pad_putn_short_uchar((void **)&xp, nelems, tp);
	assert(xp >= end);
	assert((xp - end)  < 4);
	assert((xp - buf)%4 == 0);
	{
		size_t ii;
		for(ii = 0; ii < nelems; ii++)
		{
			if(tp[ii] > X_SHORT_MAX)
			{
				assert(status == NC_ERANGE);
				return;
			}
		}
		assert(status == 0);
	}
}

T_PAD_PUTN(short, short)
T_PAD_PUTN(short, int)
T_PAD_PUTN(short, long)
T_PAD_PUTN(short, float)
T_PAD_PUTN(short, double)

T_PUTN(int, schar)
T_PUTN_U(int, uchar)
T_PUTN(int, short)
T_PUTN(int, int)
T_PUTN(int, long)
T_PUTN(int, float)
T_PUTN(int, double)

T_PUTN(float, schar)
T_PUTN_U(float, uchar)
T_PUTN(float, short)
T_PUTN(float, int)
T_PUTN(float, long)
T_PUTN(float, float)
T_PUTN(float, double)

T_PUTN(double, schar)
T_PUTN_U(double, uchar)
T_PUTN(double, short)
T_PUTN(double, int)
T_PUTN(double, long)
T_PUTN(double, float)
T_PUTN(double, double)



T_GETN(schar, schar)
dnl T_GETN(schar, uchar)
dnl - we don't return conversion errors gettin schar to uchar.
static void 
t_getn_schar_uchar(const char *const buf)
{
	const char *xp = buf; 
	const uchar *tp = uchars;
	uchar *lp = (uchar *)lbuf;
	size_t nelems = ArraySize(schars);
	int status = ncx_getn_schar_uchar((const void **)&xp, nelems, lp);
	assert(xp == buf + nelems * X_SIZEOF_SCHAR);
	assert(status == 0);

	{
		size_t ii;
		for(ii = 0; ii < nelems; ii++)
		{
			assert(tp[ii] == lp[ii]);
		}
	}
}

T_GETN(schar, short)
T_GETN(schar, int)
T_GETN(schar, long)
T_GETN(schar, float)
T_GETN(schar, double)

T_PAD_GETN(schar, schar)
dnl T_PAD_GETN(schar, uchar)
dnl - we don't return conversion errors gettin schar to uchar.
static void 
t_pad_getn_schar_uchar(const char *const buf)
{
	const char *xp = buf; 
	const uchar *tp = uchars;
	uchar *lp = (uchar *)lbuf;
	size_t nelems = ArraySize(schars);
	const char *end = buf + nelems * X_SIZEOF_SCHAR;
	int status = ncx_pad_getn_schar_uchar((const void **)&xp, nelems, lp);
	assert(xp >= end);
	assert((xp - end)  < 4);
	assert((xp - buf)%4 == 0);
	assert(status == 0);

	{
		size_t ii;
		for(ii = 0; ii < nelems; ii++)
		{
			assert(tp[ii] == lp[ii]);
		}
	}
}

T_PAD_GETN(schar, short)
T_PAD_GETN(schar, int)
T_PAD_GETN(schar, long)
T_PAD_GETN(schar, float)
T_PAD_GETN(schar, double)

T_GETN(short, schar)
T_GETN_U(short, uchar)
T_GETN(short, short)
T_GETN(short, int)
T_GETN(short, long)
T_GETN(short, float)
T_GETN(short, double)

T_PAD_GETN(short, schar)
dnl T_PAD_GETN(short, uchar)
dnl Don't make signed comparisons to usigned type
static void 
t_pad_getn_short_uchar(const char *const buf)
{
	const char *xp = buf; 
	const uchar *tp = uchars;
	uchar *lp = (uchar *)lbuf;
	size_t nelems = ArraySize(uchars);
	const char *end = buf + nelems * X_SIZEOF_SHORT;
	int status = ncx_pad_getn_short_uchar((const void **)&xp, nelems, lp);
	assert(xp >= end);
	assert((xp - end)  < 4);
	assert((xp - buf)%4 == 0);
	assert(status == 0);

	{
		size_t ii;
		for(ii = 0; ii < nelems; ii++)
		{
			if((tp[ii] <= X_SHORT_MAX))
			{
				assert(tp[ii] == lp[ii]);
			}
		}
	}
}

T_PAD_GETN(short, short)
T_PAD_GETN(short, int)
T_PAD_GETN(short, long)
T_PAD_GETN(short, float)
T_PAD_GETN(short, double)

T_GETN(int, schar)
T_GETN_U(int, uchar)
T_GETN(int, short)
T_GETN(int, int)
T_GETN(int, long)
T_GETN(int, float)
T_GETN(int, double)

T_GETN(float, schar)
T_GETN_U(float, uchar)
T_GETN(float, short)
dnl T_GETN(float, int)
dnl Exact conversion of int to x_float is limited by external float mantissa
static void 
t_getn_float_int(const char *const buf)
{
	const char *xp = buf; 
	const int *tp = ints;
	int *lp = (int *)lbuf;
	size_t nelems = ArraySize(ints);
	int status = ncx_getn_float_int((const void **)&xp, nelems, lp);
	assert(xp == buf + nelems * X_SIZEOF_FLOAT);
	/* If the system rounds up can get NC_ERANGE */
	assert(status == 0 || status == NC_ERANGE);

	{
		size_t ii;
		for(ii = 0; ii < nelems; ii++)
		{
			/* limited by x_float mantissa nbits */
			if((tp[ii] <= 16777215)
				&& (tp[ii] >= -16777215))
			{
				assert(tp[ii] == lp[ii]);
			}
		}
	}
}

dnl T_GETN(float, long)
dnl Exact conversion of long to x_float is limited by external float mantissa
static void 
t_getn_float_long(const char *const buf)
{
	const char *xp = buf; 
	const long *tp = longs;
	long *lp = (long *)lbuf;
	size_t nelems = ArraySize(longs);
	int status = ncx_getn_float_long((const void **)&xp, nelems, lp);
	assert(xp == buf + nelems * X_SIZEOF_FLOAT);
	/* If the system rounds up can get NC_ERANGE */
	assert(status == 0 || status == NC_ERANGE);

	{
		size_t ii;
		for(ii = 0; ii < nelems; ii++)
		{
			/* limited by x_float mantissa nbits */
			if((tp[ii] <= 16777215)
				&& (tp[ii] >= 16777215))
			{
				if(tp[ii] != lp[ii])
		(void) fprintf(stderr,
				"%.9e != %.9e float_float (diff %.9e)\n",
				(double)tp[ii], (double)lp[ii],
				(double)(tp[ii] - lp[ii]));
			}
		}
	}
}
T_GETN(float, float)
dnl T_GETN(float, double)
dnl Exact conversion of double to x_float is limited by external float mantissa
static void 
t_getn_float_double(const char *const buf)
{
	const char *xp = buf; 
	const double *tp = doubles;
	double *lp = (double *)lbuf;
	size_t nelems = ArraySize(doubles);
	int status = ncx_getn_float_double((const void **)&xp, nelems, lp);
	assert(xp == buf + nelems * X_SIZEOF_FLOAT);
	assert(status == 0);

	{
		size_t ii;
		for(ii = 0; ii < nelems; ii++)
		{
			if((tp[ii] <= X_FLOAT_MAX)
				&& (tp[ii] >= X_FLOAT_MIN))
			{
				if(((float)tp[ii]) != lp[ii])
				{
	if(tp[ii] != 0)
	{
		double eps = (tp[ii] - lp[ii])/tp[ii];
		if(eps > 1.19209290E-07F) /* X_FLT_EPSILON */
		{
			(void) fprintf(stderr,
				"%.9e != %.9e float_double (eps %.9e)\n",
				tp[ii], lp[ii], eps);
		}
	}
	else
	{
		(void) fprintf(stderr,
				"%.9e != %.9e float_double (diff %.9e)\n",
				tp[ii], lp[ii], tp[ii] - lp[ii]);
					
	}
				}
			}
		}
	}
}


T_GETN(double, schar)
T_GETN_U(double, uchar)
T_GETN(double, short)
T_GETN(double, int)
T_GETN(double, long)
T_GETN(double, float)
T_GETN(double, double)


#if defined(_CRAYIEEE) && !defined(_CRAYMPP) /* T90 */
#include <signal.h>
#endif /* T90 */

int
main(int ac, char *av[])
{

#if defined(_CRAYIEEE) && !defined(_CRAYMPP) /* T90 */
	/*
	 * Some of the extreme test assignments in this program trigger
         * floating point exceptions on CRAY T90
	 */
	(void) signal(SIGFPE, SIG_IGN);
#endif /* T90 */

  /* x_schar */
	t_putn_schar_schar(ncxb);
	t_getn_schar_schar(ncxb);

	t_putn_schar_uchar(ncxb);
	t_getn_schar_uchar(ncxb);

	t_putn_schar_short(ncxb);
	t_getn_schar_short(ncxb);

	t_putn_schar_int(ncxb);
	t_getn_schar_int(ncxb);

	t_putn_schar_long(ncxb);
	t_getn_schar_long(ncxb);

	t_putn_schar_float(ncxb);
	t_getn_schar_float(ncxb);

	t_putn_schar_double(ncxb);
	t_getn_schar_double(ncxb);

  /* pad x_schar */
	t_pad_putn_schar_schar(ncxb);
	t_getn_schar_schar(ncxb);
	t_pad_getn_schar_schar(ncxb);

	t_pad_putn_schar_uchar(ncxb);
	t_getn_schar_uchar(ncxb);
	t_pad_getn_schar_uchar(ncxb);

	t_pad_putn_schar_short(ncxb);
	t_getn_schar_short(ncxb);
	t_pad_getn_schar_short(ncxb);

	t_pad_putn_schar_int(ncxb);
	t_getn_schar_int(ncxb);
	t_pad_getn_schar_int(ncxb);

	t_pad_putn_schar_long(ncxb);
	t_getn_schar_long(ncxb);
	t_pad_getn_schar_long(ncxb);

	t_pad_putn_schar_float(ncxb);
	t_getn_schar_float(ncxb);
	t_pad_getn_schar_float(ncxb);

	t_pad_putn_schar_double(ncxb);
	t_getn_schar_double(ncxb);
	t_pad_getn_schar_double(ncxb);

  /* x_short */
	t_putn_short_schar(ncxb);
	t_getn_short_schar(ncxb);

	t_putn_short_uchar(ncxb);
	t_getn_short_uchar(ncxb);

	t_putn_short_short(ncxb);
	t_getn_short_short(ncxb);

	t_putn_short_int(ncxb);
	t_getn_short_int(ncxb);

	t_putn_short_long(ncxb);
	t_getn_short_long(ncxb);

	t_putn_short_float(ncxb);
	t_getn_short_float(ncxb);

	t_putn_short_double(ncxb);
	t_getn_short_double(ncxb);

  /* pad x_short */
	t_pad_putn_short_schar(ncxb);
	t_getn_short_schar(ncxb);
	t_pad_getn_short_schar(ncxb);

	t_pad_putn_short_uchar(ncxb);
	t_getn_short_uchar(ncxb);
	t_pad_getn_short_uchar(ncxb);

	t_pad_putn_short_short(ncxb);
	t_getn_short_short(ncxb);
	t_pad_getn_short_short(ncxb);

	t_pad_putn_short_int(ncxb);
	t_getn_short_int(ncxb);
	t_pad_getn_short_int(ncxb);

	t_pad_putn_short_long(ncxb);
	t_getn_short_long(ncxb);
	t_pad_getn_short_long(ncxb);

	t_pad_putn_short_float(ncxb);
	t_getn_short_float(ncxb);
	t_pad_getn_short_float(ncxb);

	t_pad_putn_short_double(ncxb);
	t_getn_short_double(ncxb);
	t_pad_getn_short_double(ncxb);

  /* x_int */
	t_putn_int_schar(ncxb);
	t_getn_int_schar(ncxb);

	t_putn_int_uchar(ncxb);
	t_getn_int_uchar(ncxb);

	t_putn_int_short(ncxb);
	t_getn_int_short(ncxb);

	t_putn_int_int(ncxb);
	t_getn_int_int(ncxb);

	t_putn_int_long(ncxb);
	t_getn_int_long(ncxb);

	t_putn_int_float(ncxb);
	t_getn_int_float(ncxb);

	t_putn_int_double(ncxb);
	t_getn_int_double(ncxb);

  /* x_float */
	t_putn_float_schar(ncxb);
	t_getn_float_schar(ncxb);

	t_putn_float_uchar(ncxb);
	t_getn_float_uchar(ncxb);

	t_putn_float_short(ncxb);
	t_getn_float_short(ncxb);

	t_putn_float_int(ncxb);
	t_getn_float_int(ncxb);

	t_putn_float_long(ncxb);
	t_getn_float_long(ncxb);

	t_putn_float_float(ncxb);
	t_getn_float_float(ncxb);

	t_putn_float_double(ncxb);
	t_getn_float_double(ncxb);

  /* x_double */
	t_putn_double_schar(ncxb);
	t_getn_double_schar(ncxb);

	t_putn_double_uchar(ncxb);
	t_getn_double_uchar(ncxb);

	t_putn_double_short(ncxb);
	t_getn_double_short(ncxb);

	t_putn_double_int(ncxb);
	t_getn_double_int(ncxb);

	t_putn_double_long(ncxb);
	t_getn_double_long(ncxb);

	t_putn_double_float(ncxb);
	t_getn_double_float(ncxb);

	t_putn_double_double(ncxb);
	t_getn_double_double(ncxb);

	return 0;
}
