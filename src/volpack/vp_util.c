/*
 * vp_util.c
 *
 * Miscellaneous utility routines.
 *
 * Copyright (c) 1994 The Board of Trustees of The Leland Stanford
 * Junior University.  All rights reserved.
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice and this permission notice appear in
 * all copies of this software and that you do not sell the software.
 * Commercial licensing is available by contacting the author.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Author:
 *    Phil Lacroute
 *    Computer Systems Laboratory
 *    Electrical Engineering Dept.
 *    Stanford University
 */

/*
 * $Date$
 * $Revision$
 */

#include "vp_global.h"

/*
 * vpRamp
 *
 * Fill a float array with a ramp (a piecewise linear function).
 */

vpResult
vpRamp(dst, stride, num_points, ramp_x, ramp_y)
float *dst;	/* array to store ramp into */
int stride;	/* stride (in bytes) for dst */
int num_points;	/* number of linear segment endpoints */
int *ramp_x;	/* x coordinates of the endpoints (and indexes into dst) */
float *ramp_y;	/* y coordinates of the endpoints */
{
    int i, x;
    int lastx, nextx;
    double lasty, nexty, dydx, y;

    if (num_points < 1)
	return(VPERROR_BAD_VALUE);
    for (i = 1; i < num_points; i++)
	if (ramp_x[i] <= ramp_x[i-1])
	    return(VPERROR_BAD_VALUE);
    dst[*ramp_x * stride/sizeof(float)] = *ramp_y;
    if (num_points == 1)
	return(VP_OK);
    for (i = 1; i < num_points; i++) {
	lastx = ramp_x[i-1];
	lasty = ramp_y[i-1];
	nextx = ramp_x[i];
	nexty = ramp_y[i];
	dydx = (double)(nexty - lasty) / (double)(nextx - lastx);
	for (x = lastx+1, y = lasty+dydx; x < nextx; x++, y += dydx)
	    dst[x * stride/sizeof(float)] = y;
	dst[x * stride/sizeof(float)] = nexty;
    }
    return(VP_OK);
}

/*
 * VPBug
 *
 * Print an error message and exit.  The argument is a printf-style format
 * string followed by parameters. 
 */

#ifdef ANSI_C
void
VPBug(char *fmt, ...)
#else
void
VPBug(fmt, va_alist)
char *fmt;
va_dcl
#endif
{
    va_list args;
    extern void exit ANSI_ARGS((int status));

    fprintf(stderr, "BUG: ");
#ifdef ANSI_C
    va_start(args, fmt);
#else
    va_start(args);
#endif
#ifdef HAVE_VPRINTF
    vfprintf(stderr, fmt, args);
#else
#ifdef HAVE_DOPRNT
    _doprnt(fmt, args, stderr);
#else
    fprintf(stderr, "unrecoverable error");
#endif
#endif
    va_end(args);
    fprintf(stderr, "\n");
    exit(1);
}

#ifdef DEBUG
/*
 * VPDebug
 *
 * Print a debugging message.
 *    VPDebug(debug_code, format, parameters, ...)
 */

#ifdef ANSI_C
void
VPDebug(vpContext *vpc, int debug_code, char *fmt, ...)
#else
void
VPDebug(vpc, debug_code, fmt, va_alist)
vpContext *vpc;
int debug_code;
char *fmt;
va_dcl
#endif
{
    va_list args;

    if (vpc->debug_enable[debug_code]) {
#ifdef ANSI_C
	va_start(args, fmt);
#else
	va_start(args);
#endif
#ifdef HAVE_VPRINTF
	vfprintf(stdout, fmt, args);
#else
#ifdef HAVE_DOPRNT
	_doprnt(fmt, args, stdout);
#else
	printf("unrecoverable error");
#endif
#endif
	va_end(args);
    }
}
#endif /* DEBUG */
