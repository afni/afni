/*****
* XmHTMLfuncs.h : memory allocation macros.
*
* This file Version	$Revision$
*
* Creation date:		Tue Dec  3 15:00:14 GMT+0100 1996
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
* $Source$
*****/
/*****
* ChangeLog 
* $Log$
* Revision 1.1  2011/06/30 16:08:41  rwcox
* Cadd
*
* Revision 1.18  1998/04/27 06:58:02  newt
* Added a few more default values
*
* Revision 1.17  1998/04/04 06:27:56  newt
* XmHTML Beta 1.1.3
*
* Revision 1.16  1997/10/23 00:24:48  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.15  1997/08/30 00:43:02  newt
* HashTable stuff. Changed proto's for almost every routine in here.
*
* Revision 1.14  1997/08/01 12:56:02  newt
* Progressive image loading changes. Changes to debug memory alloc protos.
*
* Revision 1.13  1997/05/28 01:43:34  newt
* Added protos and defines for debug memory allocation functions.
*
* Revision 1.12  1997/04/29 14:23:32  newt
* Moved all XmHTML private functions to XmHTMLP.h
*
* Revision 1.11  1997/04/03 05:32:52  newt
* ImageInfoShared macro. _XmHTMLLoadBodyImage proto
*
* Revision 1.10  1997/03/28 07:06:42  newt
* Frame interface prototypes from frames.c
*
* Revision 1.9  1997/03/20 08:07:25  newt
* added external html_tokens definition, _XmHTMLReplaceOrUpdateImage
*
* Revision 1.8  1997/03/11 19:52:05  newt
* ImageBuffer; XmHTMLImage and XmImageInfo macros; new protos for animated Gifs
*
* Revision 1.7  1997/03/04 18:47:01  newt
* _XmHTMLDrawImagemapSelection proto added
*
* Revision 1.6  1997/03/04 00:57:29  newt
* Delayed Image Loading: _XmHTMLReplaceImage and _XmHTMLUpdateImage
*
* Revision 1.5  1997/03/02 23:14:13  newt
* malloc defines; function proto's for all private image/imagemap routines
*
* Revision 1.4  1997/02/11 02:02:57  newt
* Changes for NEED_STRCASECMP
*
* Revision 1.3  1997/01/09 06:55:59  newt
* expanded copyright marker
*
* Revision 1.2  1997/01/09 06:48:43  newt
* updated function definitions
*
* Revision 1.1  1996/12/19 02:17:18  newt
* Initial Revision
*
*****/ 

#ifndef _XmHTMLfuncs_h_
#define _XmHTMLfuncs_h_

#include <errno.h>
#include <X11/IntrinsicP.h>		/* for Widget definition & fast macros */

/*****
* Using #if !defined(DMALLOC) && !defined(DEBUG) seems to trigger a bug
* on SparcWorks CPP, so we use the old #ifndef combi's to work around it.
* Fix 10/27/97-01, shl.
*****/
#ifndef DMALLOC
#ifndef DEBUG

/* Normal builds use Xt memory functions */
#define malloc(SZ)			XtMalloc((SZ))
#define calloc(N,SZ)		XtCalloc((N),(SZ))
#define realloc(PTR,SZ)		XtRealloc((char*)(PTR),(SZ))
#define free(PTR)			XtFree((char*)(PTR))
#define strdup(STR)			XtNewString((STR))

#else	/* DEBUG defined */

/* debug builds use asserted functions unless DMALLOC is defined */
extern char *__rsd_malloc(size_t size, char *file, int line);
extern char *__rsd_calloc(size_t nmemb, size_t size, char *file, int line);
extern char *__rsd_realloc(void *ptr, size_t size, char *file, int line);
extern char *__rsd_strdup(const char *s1, char *file, int line);
extern void  __rsd_free(void *ptr, char *file, int line);

/* redefine the real functions to use our private ones */
#define malloc(SZ)			__rsd_malloc((SZ), __FILE__, __LINE__)
#define calloc(N,SZ)		__rsd_calloc((N),(SZ), __FILE__, __LINE__)
#define realloc(PTR,SZ)		__rsd_realloc((PTR),(SZ), __FILE__, __LINE__)
#define free(PTR)			__rsd_free((PTR), __FILE__, __LINE__)
#define strdup(STR)			__rsd_strdup((STR), __FILE__, __LINE__)

#endif /* DEBUG */
#else /* DMALLOC */

/* let dmalloc.h define it all */
#include <dmalloc.h>

#endif /* DMALLOC */

/* Don't add anything after this endif! */
#endif /* _XmHTMLfuncs_h_ */
