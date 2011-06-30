#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* debug.c : debug initialization routines.
*
* This file Version	$Revision$
*
* Creation date:		Fri Oct 18 23:57:23 GMT+0100 1996
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
* (C)Copyright 1995-1996 Ripley Software Development
* All Rights Reserved
*
* This file is part of the XmHTML Widget Library.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Library General Public
* License as published by the Free Software Foundation; either
* version 2 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Library General Public License for more details.
*
* You should have received a copy of the GNU Library General Public
* License along with this library; if not, write to the Free
* Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*****/
/*****
* ChangeLog 
* $Log$
* Revision 1.1  2011/06/30 16:10:37  rwcox
* Cadd
*
* Revision 1.11  1998/04/27 06:58:45  newt
* made completely tka.
*
* Revision 1.10  1998/04/06 22:22:39  newt
* converted to a generic package: removed all references to XmHTML
*
* Revision 1.9  1998/04/04 06:28:03  newt
* XmHTML Beta 1.1.3
*
* Revision 1.8  1997/10/23 00:24:53  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.7  1997/08/30 00:46:59  newt
* Added support for storing debug output in a file.
*
* Revision 1.6  1997/08/01 12:58:10  newt
* Changed function names to accomdate exclusion of this file when building
* the XmHTML library.
*
* Revision 1.5  1997/04/29 14:25:07  newt
* Removed XmHTMLP.h
*
* Revision 1.4  1997/03/02 23:16:01  newt
* added XmHTMLP.h include file
*
* Revision 1.3  1997/02/11 02:06:56  newt
* potential buffer overruns eliminated
*
* Revision 1.2  1997/01/09 06:55:35  newt
* expanded copyright marker
*
* Revision 1.1  1996/12/19 02:17:08  newt
* Initial Revision
*
*****/ 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifndef NDEBUG
#include <stdlib.h>
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <stdio.h>	/* must follow stdarg or varargs on LynxOS */
#include <string.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#include <unistd.h>		/* getpid() */
#include <errno.h>
#include <time.h>		/* time(), ctime() */

/* Local includes */
#include "debug.h"

/* undefine these while in debug.c itself */
#undef _XmHTMLInitDebug
#undef _XmHTMLSelectDebugLevels
#undef _XmHTMLSetDebugLevels

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/
int __rsd__debug_levels_defined[MAX_DEBUG_LEVELS+1];
int __rsd__debug_full;
int debug_disable_warnings = 0;

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/

/*** Private Variable Declarations ***/
static FILE *__rsd_debug_file = stdout;

/*****
* When debug output is send to a file, we register an exit func to close
* the output file. Most systems have atexit(), but some do not and have
* on_exit() instead. If you get undefined references to atexit(), try
* defining HAVE_ON_EXIT. If you haven't got either atexit() or on_exit(),
* you will have to comment out the call to these functions in
* __rsd_setDebugLevels() below.
*****/
static void
#ifdef HAVE_ON_EXIT
__rsd_at_exit(int exit_num, void *call_data)
#else
__rsd_at_exit(void)
#endif
{
	/* close output file */
	fclose(__rsd_debug_file);
}

/*****
* Name: 		__rsd_fprintf
* Return Type: 	void
* Description: 	printf that flushes the given message to the requested output
*				file (which can be stdout).
* In: 
*	fmt:		message format;
*	...:		args to fmt;
* Returns:
*	nothing.
*****/
void
#ifdef __STDC__
__rsd_fprintf(char *fmt, ...) 
{
	va_list arg_list;
	va_start(arg_list, fmt);

#else /* ! __STDC__ */

__rsd_fprintf(fmt, va_alist)
    char *fmt;
    va_dcl
{
	va_list arg_list;
	va_start(arg_list);
#endif /* __STDC__ */

	/* flush to file */
	vfprintf(__rsd_debug_file, fmt, arg_list);
	va_end(arg_list);

	fflush(__rsd_debug_file);
}

/*****
* Name: 		__rsd_initDebug
* Return Type: 	void
* Description: 	initialise the global debug variables.
* In: 
*	initial:	initial debug level to set
* Returns:
*	nothing.
*****/
void
__rsd_initDebug(int initial)
{
	int i;

	for(i = 0 ; i < MAX_DEBUG_LEVELS+1; i++)
		__rsd__debug_levels_defined[i] = 0;

	/* select initial debug level (if initial is valid) */
	if(initial > 0 && initial < MAX_DEBUG_LEVELS)
		__rsd__debug_levels_defined[initial] = 1;

	/* or select all when requested */
	if(initial == MAX_DEBUG_LEVELS)
	{
		for(i = 1; i < MAX_DEBUG_LEVELS; i++)
			__rsd__debug_levels_defined[i] = 1;
	}
}

/*****
* Name:			__rsd_selectDebugLevels
* Return Type:	int
* Description:	selects a number of debug levels. Only picks out -dall
*				and -d<number>,<number>,.. Doesn't touch anything
*				else.
* In:
*	levels:		debuglevels to select
* Returns:
*	1 when level is ok, False if not.
*****/
int
__rsd_selectDebugLevels(char *levels)
{
	char *chPtr, *text;
	int i;
	int ret_val = 0;

	/* leave debuglevels alone if levels is not defined */
	if(levels == NULL)
		return(0);

	if(!(strncmp(levels, "-d", 2)))
		text = strdup(levels+2);
	else
		text = strdup(levels);

	if(!(strcasecmp(text, "all")))
	{
		fprintf(stderr, "All debug levels enabled\n");
		for(i = 1; i < MAX_DEBUG_LEVELS; i++)
			__rsd__debug_levels_defined[i] = 1;
		free(text);
		return(1);
	}

	if(!(strcasecmp(text, "full")))
	{
		fprintf(stderr, "Full debug output enabled\n");
		__rsd__debug_full = 1;
		free(text);
		return(1);
	}

	for(chPtr = strtok(text, ","); chPtr != NULL; chPtr = strtok(NULL, ","))
	{
		i = 0;
		i = atoi(chPtr);		
		if(i && i <= MAX_DEBUG_LEVELS)
		{
			fprintf(stderr, "__rsd_selectDebugLevels: selecting level %i\n", 
				i);
			__rsd__debug_levels_defined[i] = 1;
			ret_val = 1;
		}
		else	/* not one of ours */
			ret_val = 0;
	}
	free(text);
	return(ret_val);
}

/* remove an argument from the array of command line options */
#define REMOVE_ARG do { \
	for(k = i; k < *argc; k++) argv[k] = argv[k+1]; \
	*argc = *argc - 1; \
	i--; \
}while(0)

/*****
* Name:			__rsd_setDebugLevels
* Return Type:	void
* Description:	selects a number of debug levels
* In:
*	*argc:		number of command line options
*	***argv:	command line strings
* Returns:
*	nothing, but if any -d args are found, they are selected and removed
*	from the command line options.
*****/
void
__rsd_setDebugLevels(int *argc, char **argv)
{
	char *levels = NULL;
	int i, k;

	/* initialise everything to zero */
	for(i = 0 ; i < MAX_DEBUG_LEVELS+1; i++)
		__rsd__debug_levels_defined[i] = 0;

	/* Scan given command line options for any -d settings */
	for(i = 1 ; i < *argc; i++)
	{
		if(argv[i][0] == '-' && argv[i][1] == 'd')
		{
			/* output to <pid>.debug */
			if(!strncmp(argv[i], "-dfile:", 7))
			{
				char tmp[128];
				char *chPtr;
				if((chPtr = strstr(argv[i], ":")) != NULL)
				{
					/* close any existing output file */
					if(__rsd_debug_file != stdout)
						fclose(__rsd_debug_file);

					chPtr++;	/* skip : */
					if(!strcmp(chPtr, "pid"))	/* <pid>.out */
						sprintf(tmp, "%i.out", (int)getpid());
					else
					{
						strncpy(tmp, chPtr, 128);
						if(strlen(chPtr) > 127)
							tmp[127] = '\0';	/* NULL terminate */
					}
					if((__rsd_debug_file = fopen(tmp, "w")) != NULL)
					{
						time_t curr_time;
						int a;
						fprintf(stderr, "__rsd_setDebugLevels: writing debug "
							"output to %s\n", tmp);
						fprintf(__rsd_debug_file, "Debug output file "
							"for process %i\n", getpid());
						fprintf(__rsd_debug_file, "Command line arguments: ");
						for(a = 0; a < *argc; a++)
							fprintf(__rsd_debug_file, "%s ", argv[a]);
						curr_time = time(NULL);
						fprintf(__rsd_debug_file, "\nCreated on %s\n\n",
							ctime(&curr_time));
#ifdef HAVE_ON_EXIT
						on_exit(__rsd_at_exit, NULL);
#else
						atexit(__rsd_at_exit);
#endif
						__rsd__debug_levels_defined[MAX_DEBUG_LEVELS] = 1;
					}
					else
					{
						fprintf(stderr, "__rsd_setDebugLevels: failed to open "
							"output file %s (errno = %i), reverting to "
							"stdout\n", tmp, errno);
						__rsd_debug_file = stdout;
					}
				}
				else
				{
					fprintf(stderr, "__rsd_setDebugLevels: missing arg to "
						"-dfile:, reverting to stdout\n");
					__rsd_debug_file = stdout;
				}
				/* remove from cmd line */
				REMOVE_ARG;
			}
			else
			{
				levels = argv[i];

				/* select levels */
				if(__rsd_selectDebugLevels(levels))
					REMOVE_ARG;
			}
		}
	}
}
#endif /* NDEBUG */
