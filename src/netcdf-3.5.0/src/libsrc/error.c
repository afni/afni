/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: error.c,v 1.14 90/02/23 16:08:55 davis Exp */

/*LINTLIBRARY*/

#include "ncconfig.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include "netcdf.h"


#ifndef NO_STRERROR
#include <string.h> /* contains prototype for ansi libc function strerror() */
#else
/* provide a strerror function for older unix systems */
static char *
strerror(int errnum)
{
    extern int sys_nerr;
    extern char *sys_errlist[];

    if(errnum < 0 || errnum >= sys_nerr) return NULL;
    /* else */
    return sys_errlist[errnum];
}
#endif /* NO_STRERROR */


#ifdef vms
/* UNTESTED */
/*
 * On the vms system, when a system error occurs which is not
 * mapped into the unix styled errno values, errno is set EVMSERR
 * and a VMS error code is set in vaxc$errno.
 * This routine prints the systems message associated with status return
 * from a system services call.
 */

#include <errno.h>
#include <descrip.h>
#include <ssdef.h>

static const char *
vms_strerror( int status )
{
	short msglen;
	static char msgbuf[256];
	$DESCRIPTOR(message, msgbuf);
	register ret;

	msgbuf[0] = 0;
	ret = SYS$GETMSG(status, &msglen, &message, 15, 0);
	
	if(ret != SS$_BUFFEROVF && ret != SS$_NORMAL) {
		(void) strcpy(msgbuf, "EVMSERR");
	}
	return(msgbuf);
}
#endif /* vms */


static char unknown[] = "Unknown Error";


const char *
nc_strerror(int err)
{

#ifdef vms 
	if(err == EVMSERR)
	{
		return vms_strerror(err);
	}	
	/* else */
#endif /* vms */

	if(NC_ISSYSERR(err))
	{
		const char *cp = (const char *) strerror(err);
		if(cp == NULL)
			return unknown;
		/* else */
		return cp;
	}
	/* else */

	switch (err) {
	case NC_NOERR:
	    return "No error";
	case NC_EBADID:
	    return "Not a netCDF id";
	case NC_ENFILE:
	    return "Too many netCDF files open";
	case NC_EEXIST:
	    return "netCDF file exists && NC_NOCLOBBER";
	case NC_EINVAL:
	    return "Invalid argument";
	case NC_EPERM:
	    return "Write to read only";
	case NC_ENOTINDEFINE:
	    return "Operation not allowed in data mode";
	case NC_EINDEFINE:
	    return "Operation not allowed in define mode";
	case NC_EINVALCOORDS:
	    return "Index exceeds dimension bound";
	case NC_EMAXDIMS:
	    return "NC_MAX_DIMS exceeded";
	case NC_ENAMEINUSE:
	    return "String match to name in use";
	case NC_ENOTATT:
	    return "Attribute not found";
	case NC_EMAXATTS:
	    return "NC_MAX_ATTRS exceeded";
	case NC_EBADTYPE:
	    return "Not a netCDF data type or _FillValue type mismatch";
	case NC_EBADDIM:
	    return "Invalid dimension id or name";
	case NC_EUNLIMPOS:
	    return "NC_UNLIMITED in the wrong index";
	case NC_EMAXVARS:
	    return "NC_MAX_VARS exceeded";
	case NC_ENOTVAR:
	    return "Variable not found";
	case NC_EGLOBAL:
	    return "Action prohibited on NC_GLOBAL varid";
	case NC_ENOTNC:
	    return "Not a netCDF file";
	case NC_ESTS:
	    return "In Fortran, string too short";
	case NC_EMAXNAME:
	    return "NC_MAX_NAME exceeded";
	case NC_EUNLIMIT:
	    return "NC_UNLIMITED size already in use";
	case NC_ENORECVARS:
	    return "nc_rec op when there are no record vars";
	case NC_ECHAR:
	    return "Attempt to convert between text & numbers";
	case NC_EEDGE:
	    return "Edge+start exceeds dimension bound";
	case NC_ESTRIDE:
	    return "Illegal stride";
	case NC_EBADNAME:
	    return "Attribute or variable name contains illegal characters";
	case NC_ERANGE:
	    return "Numeric conversion not representable";
	case NC_ENOMEM:
	    return "Memory allocation (malloc) failure";
	}
	/* default */
	return unknown;
}
