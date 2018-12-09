/*****
* XCCP.h : XCC.c private header file
*
* This file Version	$Revision$
*
* Creation date:		Mon Mar  3 00:28:18 GMT+0100 1997
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				John L. Cwikla
*
* Copyright 1994,1995 John L. Cwikla
* Copyright (C) 1997 by Ripley Software Development
* All Rights Reserved
*
* This file is part of the XmHTML Widget Library.
*
* See below for John L. Cwikla's original copyright notice and distribution
* Policy.
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
* Id: XCCP.h,v 1.5 1995/08/04 17:46:17 cwikla
*
* Copyright 1994,1995 John L. Cwikla
*
* Permission to use, copy, modify, distribute, and sell this software
* and its documentation for any purpose is hereby granted without fee,
* provided that the above copyright notice appears in all copies and that
* both that copyright notice and this permission notice appear in
* supporting documentation, and that the name of John L. Cwikla or
* Wolfram Research, Inc not be used in advertising or publicity
* pertaining to distribution of the software without specific, written
* prior permission.  John L. Cwikla and Wolfram Research, Inc make no
* representations about the suitability of this software for any
* purpose.  It is provided "as is" without express or implied warranty.
*
* John L. Cwikla and Wolfram Research, Inc disclaim all warranties with
* regard to this software, including all implied warranties of
* merchantability and fitness, in no event shall John L. Cwikla or
* Wolfram Research, Inc be liable for any special, indirect or
* consequential damages or any damages whatsoever resulting from loss of
* use, data or profits, whether in an action of contract, negligence or
* other tortious action, arising out of or in connection with the use or
* performance of this software.
*
* Author:
*  John L. Cwikla
*  X Programmer
*  Wolfram Research Inc.
*
*  cwikla@wri.com
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
* Revision 1.10  1998/04/04 06:27:50  newt
* XmHTML Beta 1.1.3
*
* Revision 1.9  1997/08/30 00:32:27  newt
* Fixed palette preparations & color HashTable changes.
*
* Revision 1.8  1997/05/28 01:35:16  newt
* Added some additional comments.
*
* Revision 1.7  1997/04/03 05:29:41  newt
* corrected a typo
*
* Revision 1.6  1997/03/02 23:44:30  newt
* Expanded copyright marker
*
*****/
#ifndef _xccp_h_
#define _xccp_h_

#include <XmHTML/XCC.h>

#define MODE_UNDEFINED	0	/* UNKNOWN */
#define MODE_BW			1	/* Default B+W */
#define MODE_STDCMAP	2	/* Has a stdcmap */
#define MODE_TRUE		3	/* Is a TrueColor/DirectColor visual */
#define MODE_MY_GRAY	4	/* my grayramp */
#define MODE_PALETTE	5	/* has pre-allocated palette */

typedef struct XCCDither{
	int fast_rgb[32][32][32];	/* quick lookuptable for faster rendering */
	int fast_err[32][32][32];	/* internal RGB error information */
	int fast_erg[32][32][32];
	int fast_erb[32][32][32];
}XCCDither;

typedef struct _HashEntry{
	struct _HashEntry *nptr;
	struct _HashEntry *pptr;    /* linked list */
	unsigned long key;
	unsigned long data;
#ifdef DEBUG
	int ncoll;					/* no of collisions for this entry */
#endif
	struct _HashEntry *next;		/* next on the linked-list for collisions */
}HashEntry;

/* A generic hash table structure */
typedef struct _HashTable{
	int elements;				/* elements stored in the table */
	int size;					/* size of the table */
	HashEntry **table;
	HashEntry *last;			/* last on the linked list */
#ifdef DEBUG
	int requests, hits, misses, puts, collisions;
#endif
}HashTable;

struct _XColorContext
{
	Display *dpy;
	Visual *visual;
	Colormap colormap;
	XVisualInfo *visualInfo;

	int numColors;					/* available no of colors in colormap */
	int maxColors;					/* maximum no of colors */
	int numAllocated;				/* no of allocated colors */

	char mode;
	char needToFreeColormap;
	Atom stdCmapAtom;

	XStandardColormap stdCmap;
	unsigned long *CLUT;			/* Color LookUp Table */
	XColor *CMAP;					/* colormap */

	HashTable *color_hash;			/* hashtable of allocated colors */
	XColor *palette;				/* preallocated palette */
	int num_palette;				/* size of palette */

	XCCDither	*fast_dither;		/* fast dither matrix */

	struct
	{
		int red;
		int green;
		int blue;
	} shifts;
	struct
	{
		unsigned long red;
		unsigned long green;
		unsigned long blue;
	} masks;
	struct
	{
		int red;
		int green;
		int blue;
	} bits;
	unsigned long maxEntry;

	unsigned long blackPixel, whitePixel;
};

/* initialize the given hashtable. */
extern HashTable *_XCCHashInit(HashTable *table);

/* put a new entry in the hashtable */
extern void _XCCHashPut(HashTable *table, unsigned long key,
	unsigned long data);

/* get an entry from the hashtable */
extern Boolean _XCCHashGet(HashTable *table, unsigned long key,
	unsigned long *data);

/* delete an entry from the hashtable */
extern void _XCCHashDelete(HashTable *table,
	unsigned long key);

/* completely wipe the given hashtable */
extern void _XCCHashDestroy(HashTable *table);

/* Don't add anything after this endif! */
#endif /* _xccp_h_ */
