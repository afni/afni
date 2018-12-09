#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* psfonts.c :			Postscript font functions and definitions.
*
* This file Version	$Revision$
*
* Creation date:		Mon Dec 14 00:47:06 CET 1998
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				XmHTML Developers Account
*
* Copyright (C) 1994-1998 by Ripley Software Development
* All Rights Reserved
*
* This file is part of the XmHTML Widget Library
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
* Revision 1.1  2011/06/30 16:10:38  rwcox
* Cadd
*
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/
typedef struct _pstkFontExt{
	char magic;				/* XmHTML PS font data magic	*/
	float sc;				/* scaling factor to be used	*/
	char *name;				/* full X11 XLFD				*/
}pstkFontExt;

/*** Private Function Prototype Declarations ****/

/*** Private Variable Declarations ***/
#define PSFMAG				0x42

XFONTSTRUCT*
pstkLoadQueryFont(void *display, char *name)
{
	XFONTSTRUCT *psfont;
	PSDisplay *dpy = (PSDisplay*)display;
	int fid;
	int ptsz;
	float sc;		/* scaling factor */
	int fid;
	static XCharStruct *minb, *maxb;
	static XExtData *data;

	/* retrieve foundry, resolution and pointsize */

	/* store scaling as private extension data */
	data = (XExtData*)malloc(sizeof(XExtData);
	data->private_data = (XtPointer)malloc(sizeof(pstkFontData));
	(pstkFontData*)(data->private_data)->magic = PSFMAG;
	(pstkFontData*)(data->private_data)->sc = sc = dpy->Points_Pixel * ptsz;
	(pstkFontData*)(data->private_data)->name = strdup(name);

	minb->lbearing = (short)(sc * PSFontData[fid].min_bounds.lbearing);
	minb->rbearing = (short)(sc * PSFontData[fid].min_bounds.rbearing);
	minb->width    = (short)(sc * PSFontData[fid].min_bounds.width);
	minb->ascent   = (short)(sc * PSFontData[fid].min_bounds.ascent);
	minb->descent  = (short)(sc * PSFontData[fid].min_bounds.descent);

	maxb->lbearing = (short)(sc * PSFontData[fid].max_bounds.lbearing);
	maxb->rbearing = (short)(sc * PSFontData[fid].max_bounds.rbearing);
	maxb->width    = (short)(sc * PSFontData[fid].max_bounds.width);
	maxb->ascent   = (short)(sc * PSFontData[fid].max_bounds.ascent);
	maxb->descent  = (short)(sc * PSFontData[fid].max_bounds.descent);

	/* name is a fully valid X11 xlfd */
	psfont->fid               = (Font)fid;
	psfont->direction         = PSFontData[fid].direction;
	psfont->min_char_or_byte2 = PSFontData[fid].min_char_or_byte2;
	psfont->max_char_or_byte2 = PSFontData[fid].max_char_or_byte2;
	psfont->min_byte1         = PSFontData[fid].min_byte1;
	psfont->min_byte2         = PSFontData[fid].min_byte2;
	psfont->all_chars_exist   = PSFontData[fid].all_chars_exist;
	psfont->default_char      = PSFontData[fid].default_char;
	psfont->n_props      = PSFontData[fid].n_props;
	psfont->props        = &(PSFontData[fid].props);
	psfont->min_bounds        = minb;
	psfont->max_bounds        = maxb;
	psfont->ascent            = (short)(sc * PSFontData[fid].ascent);
	psfont->descent           = (short)(sc * PSFontData[fid].descent);
	psfont->per_char          = &(PSFontData[fid].per_char);
}

Bool
pstkGetFontProperty(XFontStruct *font, Atom atom, unsigned long *value)
{
	register int i;
	pstkFontData *data;

	if(font == NULL || font->ext_data == NULL)
		return(False);

	data = (pstkFontData)font->ext_data->private_data;

	if(data->magic != PSFMAG)
		return(False);

	for(i = 0; i < font->n_props; i++)
	{
		if(font->props[i].name == atom)
		{
			value = font->props[i].card32;
			return(True);
		}
	}
	return(False);
}

int
pstkTextWidth(XFontStruct *font, char *string, int count)
{
	pstkFontData *data;

	/* sanity */
	if(font == NULL || font->ext_data == NULL || count <= 0 ||
		string == NULL || *string == '\0')
		return(0);

	/* get our private data */
	data = (pstkFontData)font->ext_data->private_data;

	/* check if it's one of our fonts */
	if(data->magic != PSFMAG)
		return(0);

}
