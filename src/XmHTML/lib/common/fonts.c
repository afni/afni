#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* fonts.c : XmHTML font loading & caching routines.
*
* This file Version	$Revision$
*
* Creation date:		Mon Sep 22 09:46:00 GMT+0100 1997
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
*
* Copyright (C) 1994-1997 by Ripley Software Development
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
* Revision 1.3  1998/04/27 06:59:15  newt
* tka stuff
*
* Revision 1.2  1998/04/04 06:28:06  newt
* XmHTML Beta 1.1.3
*
* Revision 1.1  1997/10/23 00:23:09  newt
* Initial Revision
*
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>		/* isspace */

#include "toolkit.h"
#include XmHTMLPrivateHeader

/* i18n & fontset currently only supported for Motif */
#ifdef I18N
# ifndef Motif
#  undef I18N
#  define _I18N_WARNING 1		/* do a warning message instead */
# endif
#endif

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/
int xmhtml_fn_sizes[8], xmhtml_basefont_sizes[7], xmhtml_fn_fixed_sizes[2];

/*** Private Datatype Declarations ****/
/*****
* A single font cache entry.
* The font cache is a binary tree, sorted according to the name of a font.
* The name field points to a field in the font field.
*****/
typedef struct _fontCacheEntry{
	XmHTMLfont *font;				/* font data */
	String name;					/* font name */
	Boolean is_map;					/* true when this is a font mapping */
	XmHTMLfont *map_to;				/* ptr to real font data */
	struct _fontCacheEntry *left;
	struct _fontCacheEntry *right;
}fontCacheEntry;

/*****
* Definition of a display-bound font cache.
* This structure contains the ID of the display this cache is used for,
* the actual font cache, the default font for this display and a list of
* widgets referencing this cache.
*****/
typedef struct _fontCache{
	Display *dpy;					/* display were fonts come from */
	int res_x;						/* horizontal screen resolution */
	int res_y;						/* vertical screen resolution */
	fontCacheEntry *cache;			/* cached fonts for this display */
	XmHTMLfont *default_font;		/* default font */
	int nwidgets;					/* no of widgets referring this cache */
	WidgetList widgets;				/* array of widget referring this cache */
	struct _fontCache *next;		/* ptr to next cache */
	int nentries;					/* no of cached fonts */
	int nmaps;						/* no of mapped fonts */
	int nlookups;					/* no of search actions */
	int requests;					/* no of requests made */
	int hits;						/* no of hits */
	int misses;						/* no of missed requests */
}fontCache;

/*** Private Function Prototype Declarations ****/
/* create a fully valid XLFD */
static String makeFontName(String name, String foundry, String family,
	String weight, String slant, int points, String charset, String fam_return,
	Boolean *i18n);

static fontCacheEntry *insertFont(fontCacheEntry *entry, String name,
	XmHTMLfont *font, XmHTMLfont *map_to);

/* get a font from the cache */
static XmHTMLfont *getFont(fontCacheEntry *entry, String name, Byte style);

/* create a map for the given font */
static XmHTMLfont *mapFont(XmHTMLfont *font, String name);

/* allocate a new XmHTMLfont entry containing a single font */
static XmHTMLfont *allocFont(ToolkitAbstraction *tka, void *lfont,
	String name, String family, Byte style, Boolean i18n);

static XmHTMLfont *loadQueryFont(XmHTMLWidget html, String name, String family,
	int ptsz, Byte style, Boolean *loaded);

#ifdef I18N
/* allocate a new XmHTMLfont entry containg a set of fonts (I18N support) */
static XmHTMLfont *allocFontSet(ToolkitAbstraction *tka, void *lfontset,
	String name, String family, Byte style);
#endif

/* load or get a font from the cache */
static XmHTMLfont *loadAndCacheFont(ToolkitAbstraction *tka, String name,
	String family, Byte style, Boolean i18n);

/* free all cached fonts for the given display */
static void freeFontEntries(ToolkitAbstraction *tka, fontCacheEntry *fonts);

/* (re-)initialize a font cache */
static void initializeFontSizeLists(XmHTMLWidget html);

/*** Private Variable Declarations ***/
static fontCache *master_cache;		/* master font cache */
static fontCache *curr_cache;		/* current font cache */

/* Backup lists when sizes are not specified */
static int def_fn_sizes[8] = {140,80,240,180,160,140,120,100};
static int def_fn_fixed_sizes[2] = {120,80};

/*****
* Name: 		makeFontName
* Return Type: 	String
* Description: 	creates a full 14 field font name from the given args.
* In:
*	name:		font base name, required
*	foundry:	font foundry, optional
*	family:		font family, optional
*	weight:		font weight, required
*	slant:		font slant, required
*	points:		font pointsize (tenths of a point), required
*	charset:	current ISO character set encoding
*	fam_return: XmHTML fontFamily spec, updated upon return.
* Returns:
*	the composed font name
* Note:
*	The name argument contains the following string:
*		foundry-family-width-spacing
*	The charset contains the last two fields of a valid font name:
*		iso9959-1
*	When family is specified, foundry will be replaced by a wildcard
*
*	this routine will compose the following fontname given the basename:
*	-foundry-family-weight-slant-width--*-points-res_x-res_y-spacing-*-charset
*****/
static String
makeFontName(String name, String foundry, String family, String weight,
	String slant, int points, String charset,
	String fam_return, Boolean *i18n)
{
	int i, namelen = 0, buflen = 0;
	static char *fontfam, *new_name;
	char *famPtr, *setPtr;
	String fndry, fam, wd, sp;
	char fontbuf[1024], privbuf[1024];

	/* copy name, it will get modified */
	fontfam = my_strndup(name, strlen(name));
	famPtr = fontfam;
	setPtr = charset;
	fam_return[0] = '\0';

	/* initialize return buffer */
	buflen = 1024;
	new_name = (char*)malloc(buflen*sizeof(char));
	new_name[0] = '\0';		/* make strcat work */

	while(True)
	{
		/* foundry */
		fndry = famPtr;

		/* family */
		for(; *famPtr != '\0' && *famPtr != '-'; famPtr++);
		*(famPtr++) = '\0';
		fam = famPtr;

		/* set width */
		for(; *famPtr != '\0' && *famPtr != '-'; famPtr++);
		*(famPtr++) = '\0';
		wd = famPtr;

		/* spacing */
		for(; *famPtr != '\0' && *famPtr != '-'; famPtr++);
		*(famPtr++) = '\0';
		sp = famPtr;

#ifdef I18N
		/* scan for end or next fontset */
		for(; *famPtr != '\0' && *famPtr != ',' ; famPtr++);
		if(*famPtr != '\0')
			*(famPtr++) = '\0';
#else
		/* scan for end or next fontset and ignore them */
		for(; *famPtr != '\0' && *famPtr != ','; famPtr++);
		if(*famPtr != '\0')
			*famPtr = '\0';
#endif /* I18N */

#if 0
		/* foundry */
		fndry = &famPtr[0];

		/* family */
		for(i = 0; famPtr[i] != '\0' && famPtr[i] != '-'; i++);
		famPtr[i++] = '\0';
		fam = &famPtr[i];

		/* set width */
		for(; famPtr[i] != '\0' && famPtr[i] != '-'; i++);
		famPtr[i++] = '\0';
		wd = &famPtr[i];

		/* spacing */
		for(; famPtr[i] != '\0' && famPtr[i] != '-'; i++);
		famPtr[i++] = '\0';
		sp = &famPtr[i];

		/* scan for end or next fontset and ignore them */
		for(; famPtr[i] != '\0' && famPtr[i] != ','; i++);
		if(famPtr[i] != '\0')
			famPtr[i++] = '\0';
#endif

		_XmHTMLFullDebug(8, ("fonts.c: makeFontName, split fontFamily "
			"value %s:\nfoundry : %s\nfamily : %s\nwidth : %s\nspacing : %s\n",
			name, fndry, fam, wd, sp));

		/* screen resolutions are stored in the display-bound font cache */
		namelen += sprintf(fontbuf, "-%s-%s-%s-%s-%s-*-*-%i-%i-%i-%s-*-%s",
			(foundry != NULL ? foundry : fndry),
			(family != NULL ? family : fam),
			weight, slant, wd, points, curr_cache->res_x, curr_cache->res_y,
			sp, setPtr);

		if(namelen > buflen)
		{
			buflen += 1024;
			new_name = (char*)realloc((void*)new_name, buflen * sizeof(char));
		}
		strcat(new_name, fontbuf);

		/* create XmHTML fontFamily spec for this font */
		sprintf(privbuf, "%s-%s-%s-%s", (foundry != NULL ? foundry : fndry),
			(family != NULL ? family : fam), wd, sp);

		if((strlen(privbuf) + strlen(fam_return)) < 1023)
			strcat(fam_return, privbuf);

#ifdef I18N
		if(*famPtr != '\0')
		{
			char *ptr;

			/* this is a fontset */
			*i18n = True;
			/* famPtr already points to the beginning of the next fontset */
			if((ptr = strstr(setPtr, ",")) != NULL)
				setPtr = ptr++;
			/* else keep last charset */
		}
		else
		{
			*i18n = False;
			break;
		}
#else
		*i18n = False;
		break;
#endif
	}

	/* no longer needed */
	free(fontfam);

	/* make it all lowercase */
	my_locase(new_name);

	return(new_name);
}

/*****
* Name: 		insertFont
* Return Type: 	fontCacheEntry
* Description: 	inserts the given font in the given font cache.
* In:
*	entry:		current font cache;
*	name:		name of font to insert;
*	font:		font data to be inserted;
*	map_to:		original font data if this is a mapping;
* Returns:
*	updated cache entry;
*****/
static fontCacheEntry*
insertFont(fontCacheEntry *entry, String name, XmHTMLfont *font,
	XmHTMLfont *map_to)
{
	if(entry == NULL)
	{
		/* allocate new font entry */
		entry = (fontCacheEntry*)malloc(sizeof(fontCacheEntry));
		entry->name   = font->font_name;
		entry->font   = font;
		entry->is_map = map_to != NULL;
		entry->map_to = map_to;
		entry->left   = (fontCacheEntry*)NULL;
		entry->right  = (fontCacheEntry*)NULL;
	}
	else
	{
		int ret_val = strncmp(name, entry->name, strlen(name));

		/* duplicate font entries can exist, so check the style as well */
		if(ret_val == 0 && entry->font->style == font->style)
			return(entry);
		if(ret_val < 0)
			entry->left = insertFont(entry->left, name, font, map_to);
		else
			entry->right = insertFont(entry->right, name, font, map_to);
	}
	return(entry);
}

/*****
* Name:			getFont
* Return Type: 	XmHTMLfont
* Description: 	looks for a font in the fontcache;
* In:
*	entry:		current font cache;
*	name:		name of font to locat;
* Returns:
*	a valid font if name is found, NULL if not found.
*****/
static XmHTMLfont*
getFont(fontCacheEntry *entry, String name, Byte style)
{
	if(entry != NULL)
	{
		int ret_val = strncmp(name, entry->name, strlen(name));
		curr_cache->nlookups++;

		/*****
		* We want the styles to match as well, loadQueryFont is
		* a bit too smart sometimes.
		*****/
		if(ret_val == 0 && style == entry->font->style)
		{
			_XmHTMLDebug(8,("already cached.\n"));

			if(entry->map_to)
			{
				_XmHTMLDebug(8, ("\t(mapped to %s)\n",
					entry->map_to->font_name));
				return(entry->map_to);
			}
			else
				return(entry->font);
		}
		if(ret_val < 0)
			return(getFont(entry->left, name, style));
		else
			return(getFont(entry->right, name, style));
	}
	return((XmHTMLfont*)NULL);
}

/*****
* Name: 		mapFont
* Return Type:	XmHTMLfont*
* Description: 	creates a font mapping;
* In:
*	font:		data to which ``name'' is mapped;
*	name:		name of font
* Returns:
*	a new font entry;
*****/
static XmHTMLfont*
mapFont(XmHTMLfont *font, String name)
{
	static XmHTMLfont *map;

	map = (XmHTMLfont*)malloc(sizeof(XmHTMLfont));

	/* copy everything */
	memcpy(map, font, sizeof(XmHTMLfont));

	/* override name */
	map->font_name = strdup(name);
	return(map);
}

/*****
* Name: 		allocFont
* Return Type: 	XmHTMLfont
* Description: 	allocates a new font entry and retrieves all required
*				font properties;
* In:
*	lfont:		ptr to an X font;
*	name:		name of this font;
*	family:		family to which this font belongs;
*	style:		style of this font, see the FONT_ defines in XmHTMLP.h
* Returns:
*	a new font entry;
*****/
static XmHTMLfont*
allocFont(ToolkitAbstraction *tka, void *lfont, String name,
	String family, Byte style, Boolean i18n)
{
	static XmHTMLfont *font;
	unsigned long value = 0;
	XFONTSTRUCT *xfont = (XFONTSTRUCT*)lfont;

	font = (XmHTMLfont*)malloc(sizeof(XmHTMLfont));

#ifdef I18N
	if(i18n)
		return(allocFontSet(tka, lfont, name, family, style));
#endif /* I18N */

	/* default items */
	font->type  = XmHTML_FONT;
	font->xfont = xfont;
	font->font_name = strdup(name);
	font->font_family = strdup(family);
	font->style = style;

	/* suggested lineheight */
	font->lineheight = TkaFontLineheight(xfont);

	/* maximum left & right bearing */
	font->m_lbearing = TkaFontLeftBearing(xfont);
	font->m_rbearing = TkaFontRightBearing(xfont);
	font->m_width    = TkaFontWidth(xfont);
	font->m_ascent   = TkaFontMaxAscent(xfont);
	font->m_descent  = TkaFontMaxDescent(xfont);
	font->ascent     = TkaFontAscent(xfont);
	font->descent    = TkaFontDescent(xfont);

	/* size of largest character */
	font->height = font->m_ascent + font->m_descent;

	/* now go get a bunch of properties */

	/* normal interword spacing */
	if((tka->GetFontProperty(xfont, XA_NORM_SPACE, &value)) == True)
		font->isp = (Cardinal)value;
	else
	{
		/* use width of a single space */
		font->isp = tka->TextWidth(font, " ", 1);
		/* sanity for non-iso fonts */
		if(font->isp < 0)
			font->isp = 3;
	}

	/* additional end-of-line spacing */
	if((tka->GetFontProperty(xfont, XA_END_SPACE, &value)) == True)
		font->eol_sp = (Cardinal)value;
	else
		font->eol_sp = 0;

	/* superscript x-offset */
	if((tka->GetFontProperty(xfont, XA_SUPERSCRIPT_X, &value)) == True)
		font->sup_xoffset = (int)value;
	else
		font->sup_xoffset = 0;

	/* superscript y-offset */
	if((tka->GetFontProperty(xfont, XA_SUPERSCRIPT_Y, &value)) == True)
		font->sup_yoffset = (int)value;
	else
		font->sup_yoffset = (int)(font->m_ascent  * -.4);

	/* subscript x-offset */
	if((tka->GetFontProperty(xfont, XA_SUBSCRIPT_X, &value)) == True)
		font->sub_xoffset = (int)value;
	else
		font->sub_xoffset = 0;

	/* subscript y-offset */
	if((tka->GetFontProperty(xfont, XA_SUBSCRIPT_Y, &value)) == True)
		font->sub_yoffset = (int)value;
	else
		font->sub_yoffset = (int)(font->m_descent * .8);

	/* underline offset */
	if((tka->GetFontProperty(xfont, XA_UNDERLINE_POSITION, &value)) == True)
		font->ul_offset = (int)value;
	else
		font->ul_offset = (int)(font->m_descent-2);

	/* underline thickness */
	if((tka->GetFontProperty(xfont, XA_UNDERLINE_THICKNESS, &value)) == True)
		font->ul_thickness = (Cardinal)value;
	else
		font->ul_thickness = (Cardinal)1;

	/* strikeout offset */
	if((tka->GetFontProperty(xfont, XA_STRIKEOUT_ASCENT, &value)) == True)
	{
		/*****
		* strikeout_ascent gives the upper limit for a *bounding box*,
		* while strikeout_descent gives the lower limit. We simply
		* add them up and then divide by 2 to get the baseline offset
		* for strikeouts.
		*****/
		font->st_offset = (int)value;
		if((tka->GetFontProperty(xfont, XA_STRIKEOUT_DESCENT, &value)) == True)
		{
			font->st_offset += (int)value;
			font->st_offset *= 0.5;
		}
		else
			font->st_offset = (int)(0.5*(font->height));
	}
	else
		font->st_offset = (int)(0.5*(font->height));

	/* strikeout thickness. No font property for this one */
	font->st_thickness = font->ul_thickness;

	return(font);
}

#ifdef I18N
/*****
* Name: 		allocFontSet
* Return Type: 	XmHTMLfont
* Description: 	allocates a new font entry and retrieves all required
*				font properties;
* In:
*	xfont:		ptr to an X fontSet;
*	name:		name of this font;
*	family:		family to which this font belongs;
*	style:		style of this font, see the FONT_ defines in XmHTMLP.h
* Returns:
*	a new font entry;
*****/
static XmHTMLfont*
allocFontSet(ToolkitAbstraction *tka, void *lfontset, String name,
	String family, Byte style)
{
	static XmHTMLfont *font;
	unsigned long value = 0;
	XFONTSET xfontset = (XFONTSET)lfontset;
	XFONTSTRUCT *xfont, **allfonts;
	String *allnames;
	int nfonts, i;

	/* get the complete set of xfonts making up this fontset */
	if((nfonts = XFontsOfFontSet(xfontset, &allfonts, &allnames)) == 0)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "allocFontSet"),
			"Empty fontset!\n");
		return(NULL);
	}

	font = (XmHTMLfont*)calloc(1, sizeof(XmHTMLfont));

	/* default items */
	font->type  = XmHTML_FONTSET;
	font->xfont = (XtPointer)xfontset;
	font->font_name = strdup(name);
	font->font_family = strdup(family);
	font->style = style;

	/* check each font and get the maximum dimensions & properties */
	for(i = 0; i < nfonts; i++)
	{
		xfont = allfonts[i];

		/* suggested lineheight */
		if(font->lineheight < TkaFontLineheight(xfont))
		font->lineheight = TkaFontLineheight(xfont);

		/* maximum left & right bearing */
		if(font->m_lbearing < TkaFontLeftBearing(xfont))
			font->m_lbearing = TkaFontLeftBearing(xfont);

		if(font->m_rbearing < TkaFontRightBearing(xfont))
			font->m_rbearing = TkaFontRightBearing(xfont);

		if(font->m_width < TkaFontWidth(xfont))
			font->m_width = TkaFontWidth(xfont);

		if(font->m_ascent < TkaFontMaxAscent(xfont))
			font->m_ascent = TkaFontMaxAscent(xfont);

		if(font->m_descent < TkaFontMaxDescent(xfont))
			font->m_descent = TkaFontMaxDescent(xfont);

		if(font->ascent < TkaFontAscent(xfont))
			font->ascent = TkaFontAscent(xfont);

		if(font->descent < TkaFontDescent(xfont));
			font->descent = TkaFontDescent(xfont);

		/* size of largest character */
		if(font->height < (font->m_ascent + font->m_descent))
			font->height = font->m_ascent + font->m_descent;

		/* now go get a bunch of properties */

		/* normal interword spacing */
		if((tka->GetFontProperty(xfont, XA_NORM_SPACE, &value)) == True)
		{
			if(font->isp < (Cardinal)value)
				font->isp = (Cardinal)value;
		}
		else
		{
			/* use width of a single space */
			value = tka->TextWidth(font, " ", 1);
			/* sanity for non-iso fonts */
			if(value < 0)
				value = 3;
			if(font->isp < value)
				font->isp = value;
		}

		/* additional end-of-line spacing */
		if((tka->GetFontProperty(xfont, XA_END_SPACE, &value)) == True)
			if(font->eol_sp < (Cardinal)value)
				font->eol_sp = (Cardinal)value;

		/* superscript x-offset */
		if((tka->GetFontProperty(xfont, XA_SUPERSCRIPT_X, &value)) == True)
			if(font->sup_xoffset < (int)value)
				font->sup_xoffset = (int)value;

		/* superscript y-offset */
		if((tka->GetFontProperty(xfont, XA_SUPERSCRIPT_Y, &value)) == True)
		{
			if(font->sup_yoffset < (int)value)
				font->sup_yoffset = (int)value;
		}
		else
		{
			if(font->sup_yoffset < (int)(font->m_ascent  * -.4))
				font->sup_yoffset = (int)(font->m_ascent  * -.4);
		}

		/* subscript x-offset */
		if((tka->GetFontProperty(xfont, XA_SUBSCRIPT_X, &value)) == True)
			if(font->sub_xoffset < (int)value)
				font->sub_xoffset = (int)value;


		/* subscript y-offset */
		if((tka->GetFontProperty(xfont, XA_SUBSCRIPT_Y, &value)) == True)
		{
			if(font->sub_yoffset < (int)value)
				font->sub_yoffset = (int)value;
		}
		else
			if(font->sub_yoffset < (int)(font->m_descent * .8))
				font->sub_yoffset = (int)(font->m_descent * .8);

		/* underline offset */
		if((tka->GetFontProperty(xfont, XA_UNDERLINE_POSITION, &value)) == True)
		{
			if(font->ul_offset < (int)value)
				font->ul_offset = (int)value;
		}
		else
		{
			if(font->ul_offset < (int)(font->m_descent-2))
				font->ul_offset = (int)(font->m_descent-2);
		}

		/* underline thickness */
		if((tka->GetFontProperty(xfont, XA_UNDERLINE_THICKNESS,
			&value)) == True)
		{
			if(font->ul_thickness < (Cardinal)value)
				font->ul_thickness = (Cardinal)value;
		}
		else
			font->ul_thickness = (Cardinal)1;

		/* strikeout offset */
		if((tka->GetFontProperty(xfont, XA_STRIKEOUT_ASCENT, &value)) == True)
		{
			int st_offset;
			/*****
			* strikeout_ascent gives the upper limit for a *bounding box*,
			* while strikeout_descent gives the lower limit. We simply
			* add them up and then divide by 2 to get the baseline offset
			* for strikeouts.
			*****/
			font->st_offset = (int)value;
			st_offset = (int)value;
			if((tka->GetFontProperty(xfont, XA_STRIKEOUT_DESCENT,
				&value)) == True)
			{
				st_offset += (int)value;
				st_offset *= 0.5;
				if(font->st_offset < st_offset)
					font->st_offset = st_offset;
			}
			else
			{
				if(font->st_offset < (int)(0.5*(font->height)))
					font->st_offset = (int)(0.5*(font->height));
			}
		}
		else
		{
			if(font->st_offset < (int)(0.5*(font->height)))
				font->st_offset = (int)(0.5*(font->height));
		}
	}

	/* strikeout thickness. No font property for this one */
	font->st_thickness = font->ul_thickness;

	return(font);
}
#endif

/*****
* Name:			loadAndCacheFont
* Return Type: 	XmHTMLfont
* Description: 	retrieves a font from the cache or loads one when it isn't
*				already available.
* In:
*	tka:		toolkit abstraction to be used;
*	name:		name of font to be loaded;
*	family:		family to which this font belongs;
*	style:		style of this font, see the FONT_ defines in XmHTMLP.h;
* Returns:
*	a valid font if the font was loaded successfully, NULL if not (which
*	should never happen);
*****/
static XmHTMLfont*
loadAndCacheFont(ToolkitAbstraction *tka, String name, String family,
	Byte style, Boolean i18n)
{
	XmHTMLfont *font;
	void *xfont;

	_XmHTMLDebug(8,( "fonts.c: loadAndCacheFont: checking fontcache for\n%s: ",
		name));

	curr_cache->requests++;

	/* check if we have loaded this font before */
	if((font = getFont(curr_cache->cache, name, style)) != NULL)
	{
		free(name);	/* not needed */
		curr_cache->hits++;
		return(font);
	}
	curr_cache->misses++;

	_XmHTMLDebug(8,( "not cached,\ntrying to load..."));

	/* A new font, try to load it */
#ifdef I18N
	if(i18n)
	{
		char **missingchars;
		int misscount = 0;
		xfont = (void*)XCreateFontSet(tka->dpy, name, &missingchars,
			&misscount, NULL);

		if(misscount != 0)
			XFreeStringList(missingchars);

		_XmHTMLDebug(8,( "fonts.c: loadAndCacheFont: loaded a fontset\n"));
	}
	else
#endif /* I18N */
		xfont = (void*)tka->LoadQueryFont(tka->dpy, name);

	/* store it if successfull */
	if(xfont != NULL)
	{
		_XmHTMLDebug(8,( "found.\n"));

		/* get a new fontentry */
		font = allocFont(tka, xfont, name, family, style, i18n);

		/* store in the cache */
		curr_cache->nentries++;
		curr_cache->cache = insertFont(curr_cache->cache, name, font, NULL);

		free(name);	/* no longer needed */

		/* return the new font */
		return(font);
	}
#ifdef DEBUG
	else
		_XmHTMLDebug(8,( "failed.\n"));
#endif
	free(name);	/* not needed */
	return((XmHTMLfont*)NULL);
}

/*****
* Name:			loadQueryFont
* Return Type:	XFontStruct*
* Description:	loads a font from the given family in given size, weight and
*				slant. Loaded fonts are cached to minimize the overhead spent
*				in XLoadQueryFont().
* In:
*	w:			Widget for which this font is to be loaded.
*	name:		XmHTML fontFamily spec. Only used when family is NULL.
*	family:		font family name of the font to load. When non-null this
*				contains the typeface of the font, e.i.: helvetica, symbol,
*				etc...
*	ptsz:		size of font to load, in tenths of a point
*	style:		style of this font.
*	*loaded:	indicates whether the requested font was loaded or the current
*				font was returned. When loaded is initially True, a warning
*				message is displayed if the font can't be loaded.
* Returns:
*	A XFontStruct* for the font in the requested family/size and loaded
*	set to True or the current font and loaded set to False.
* Note:
*	This routine was based on the LoadQueryScalableFont() routine found in
*	O'Reilly's Xlib Programming Manual by Adrian Nye, but that's no longer
*	recognizable...
*
*	This routine goes through *GREAT* lengths to find the requested font, and
*	it will almost never fail (unless the XmNfontFamily/XmNcharset resources
*	form an invalid pair, but then XmHTML will give up on startup immediatly).
*****/
static XmHTMLfont*
loadQueryFont(XmHTMLWidget html, String name, String family, int ptsz,
	Byte style, Boolean *loaded)
{
	String weight, slant, fontname = NULL, charset = NULL;
	char fontfamily[1024], font_mapping[1024];
	XmHTMLfont *font = NULL;
	ToolkitAbstraction *tka = HTML_ATTR(tka);
	Boolean i18n = False;

	font_mapping[0] = '\0';

	/*****
	* Okay, now we are going to try and load a font.
	* Check weight & slant styles. The order in which they are treated
	* is important: bold overrides any FONT_MEDIUM settings and italic
	* overrides any FONT_REGULAR settings.
	* First attempts are all made with given charset. If no font is found
	* we wildcard it and try all over again.
	*****/
	if(style & FONT_BOLD)
	{
		int num_total = 0;

		while(num_total != 2 && font == NULL)
		{
			int num_outer = 0;

			charset = (num_total == 0 ? HTML_ATTR(charset) : "*-*");

			num_total++;

			while(num_outer != 4 && font == NULL)
			{
				int num_inner = 0;

				/* weight can vary between bold, demibold, medium, regular */
				switch(num_outer)
				{
					case 0 : weight = "bold"    ; break;
					case 1 : weight = "demibold"; break;
					case 2 : weight = "medium"  ; break;
					default: weight = "regular" ; break;
				}

				num_outer++;

				/* slant can vary between italic, oblique and roman */
				if(style & FONT_ITALIC)
				{
					while(num_inner < 3 && font == NULL)
					{
						switch(num_inner)
						{
							case 0 : slant = "i"; break; /* italic */
							case 1 : slant = "o"; break; /* oblique */
							default: slant = "r"; break; /* roman */
						}

						num_inner++;

						fontname = makeFontName(name, family ? "*" : NULL,
							family, weight, slant, ptsz, charset, fontfamily,
							&i18n);

						font = loadAndCacheFont(tka, fontname, fontfamily,
								style, i18n);
						if(font == NULL && font_mapping[0] == '\0')
						{
							strcpy(font_mapping, fontname);
							font_mapping[strlen(fontname)] = '\0';
						}
					}
				}
				else
				{
					slant = "r"; /* roman */

					fontname = makeFontName(name, family ? "*" : NULL,
						family, weight, slant, ptsz, charset, fontfamily,
						&i18n);

					font = loadAndCacheFont(tka, fontname, fontfamily, style,
								i18n);
					if(font == NULL && font_mapping[0] == '\0')
					{
						strcpy(font_mapping, fontname);
						font_mapping[strlen(fontname)] = '\0';
					}
				}
			}
		}
	}
	/* regular font style */
	else
	{
		int num_total = 0;

		while(num_total != 2 && font == NULL)
		{
			int num_outer = 0;

			charset = (num_total == 0 ? HTML_ATTR(charset) : "*-*");

			num_total++;

			while(num_outer != 2 && font == NULL)
			{
				int num_inner = 0;

				/* weight can vary between medium and regular */
				if(num_outer == 0)
					weight = "medium";
				else
					weight = "regular";

				num_outer++;

				/* slant can vary between italic, oblique and roman */
				if(style & FONT_ITALIC)
				{
					while(num_inner < 3 && font == NULL)
					{
						switch(num_inner)
						{
							case 0 : slant = "i"; break; /* italic */
							case 1 : slant = "o"; break; /* oblique */
							default: slant = "r"; break; /* roman */
						}

						num_inner++;

						fontname = makeFontName(name, family ? "*" : NULL,
							family, weight, slant, ptsz, charset, fontfamily,
							&i18n);

						font = loadAndCacheFont(tka, fontname, fontfamily,
								style, i18n);
						if(font == NULL && font_mapping[0] == '\0')
						{
							strcpy(font_mapping, fontname);
							font_mapping[strlen(fontname)] = '\0';
						}
					}
				}
				else
				{
					slant = "r"; /* roman */

					fontname = makeFontName(name, family ? "*" : NULL,
						family, weight, slant, ptsz, charset, fontfamily,
						&i18n);

					font = loadAndCacheFont(tka, fontname, fontfamily, style,
							i18n);
					if(font == NULL && font_mapping[0] == '\0')
					{
						strcpy(font_mapping, fontname);
						font_mapping[strlen(fontname)] = '\0';
					}
				}
			}
		}
	}

	if(font)
	{
		/*****
		* If the requested font was mapped to another font, store the
		* mapping as well since it will be the same for all subsequent
		* requests for this font.
		* loaded is False only when we are just attempting to load a font
		* and we want this thing to fail, so we sure as hell don't want a
		* default mapping then.
		*****/
		if(font_mapping[0] != '\0' && *loaded == False)
		{
			XmHTMLfont *map = mapFont(font, font_mapping);
			curr_cache->nentries++;
			curr_cache->nmaps++;
			curr_cache->cache = insertFont(curr_cache->cache, font_mapping,
									map, font);
		}
		/* we have the font */
		*loaded = True;

		/* store point size (incoming is tenth's of a point) */
		font->ptsize = ptsz/10;
		/* return the new font */
		return(font);
	}

	/* we don't have the font */
	if(*loaded)
	{
		_XmHTMLWarning(__WFUNC__((Widget)html, "loadQueryFont"),
			XMHTML_MSG_35, fontname);
	}

	*loaded = False;

	/* fix 02/03/07-01, dp */
	return(curr_cache->default_font);
}

/*****
* Name:			_XmHTMLaddFontMapping
* Return Type: 	void
* Description: 	add a fontmapping for the the given font. The name of the
*				font to be mapped is created in such a way that it will
*				be the very first match when the loadQueryFont routine
*				is called. It's primary use is to reduce loading times when
*				switching between documents (we already know which font we will
*				get).
* In:
*	font:		actual font.
* Returns:
*	nothing.
*****/
void
_XmHTMLaddFontMapping(XmHTMLWidget html, String name, String family,
	int ptsz, Byte style, XmHTMLfont *font)
{
	String fontname = NULL;
	char fontfamily[1024];
	XmHTMLfont *map;
	Boolean i18n = False;

	/*****
	* Create an XLFD that will match on the first run of loadQueryFont.
	* !!!INCREDIBLE SPEEDUP!!!
	*****/
	fontname = makeFontName(name, family ? "*" : NULL, family,
			style & FONT_BOLD ? "bold" : "medium",
			style & FONT_ITALIC ? "i"  : "r", ptsz, HTML_ATTR(charset),
			fontfamily, &i18n);

	/* add a mapping */
	map = mapFont(font, fontname);

	/* no longer needed */
	free(fontname);

	curr_cache->nentries++;
	curr_cache->nmaps++;
	curr_cache->cache = insertFont(curr_cache->cache, fontname, map, font);
}

/*****
* Name:			freeFontEntries
* Return Type: 	void
* Description: 	releases all fonts in the given cache;
* In:
*	dpy:		display on which the fonts were allocated;
*	fonts:		cache to be freed;
* Returns:
*	nothing.
*****/
static void
freeFontEntries(ToolkitAbstraction *tka, fontCacheEntry *fonts)
{
	if(fonts)
	{
		freeFontEntries(tka, fonts->left);
		freeFontEntries(tka, fonts->right);

		_XmHTMLDebug(8, ("fonts.c: freeFontEntries, releasing font\n\t%s\n",
			fonts->font->font_name));

		/* only free the font if it isn't a mapped one */
		if(!fonts->is_map)
		{
			if(fonts->font->type == XmHTML_FONT)
				tka->FreeFont(tka->dpy, fonts->font->xfont);
			else
				fprintf(stderr, "No code to release a fontset!\n");
			free(fonts->font->font_family);
		}

		/* free all allocated strings */
		free(fonts->font->font_name);

		/* free XmHTMLfont entry */
		free(fonts->font);

		/* free cache entry */
		free(fonts);
	}
}

/*****
* Name: 		initializeFontSizeLists
* Return Type: 	void
* Description: 	fills all arrays of font sizes.
* In:
*	w:			widget containing font size specs.
* Returns:
*	nothing, but the font lists are updated to reflect the new sizes.
*
* The static size lists can cause unexpected results when multiple instances
* of the Widget with different sizelists are being used.
*****/
static void
initializeFontSizeLists(XmHTMLWidget html)
{
	char *chPtr;
	char size_list[64];
	int i;
	Boolean ok;

#ifdef _I18N_WARNING
	static Boolean warning_given;
	if(!warning_given)
	{
		_XmHTMLWarning(__WFUNC__(html, "initializeFontSizeLists"),
			"This library was compiled with internationalization and "
			"multilingual\n    support enabled, which is only support for "
			"Motif versions of this library.");
	}
	warning_given = True;
#endif

	_XmHTMLDebug(8,( "fonts.c: initializeFontSizeLists Start\n"));

	/*** Scalable font size list ***/

	/* copy name, it gets destroyed */
	(void)memset(&size_list, 0, 64);
	strncpy(size_list, HTML_ATTR(font_sizes), 63);

	/* This list has 8 elements */
	for(chPtr = strtok(size_list, ","), i = 0; i < 8 && chPtr != NULL;
		chPtr = strtok(NULL, ","), i++)
	{
		if((xmhtml_fn_sizes[i] = 10*atoi(chPtr)) == 0)
			xmhtml_fn_sizes[i] = def_fn_sizes[i];
	}
	/* fill up list if it is not complete */
	if(i != 8)
	{
		for(; i < 8; i++)
			xmhtml_fn_sizes[i] = def_fn_sizes[i];
	}

#ifdef DEBUG
	_XmHTMLDebug(8, ( "fonts.c: initializeFontSizeLists, scalable font "
		"size list:\n"));
	for(i = 0 ; i < 8 ; i++)
		_XmHTMLDebug(8,("%i ", xmhtml_fn_sizes[i]));
#endif

	/*** Fixed font size list ***/

	/* copy name, it gets destroyed */
	(void)memset(&size_list, 0, 64);
	strncpy(size_list, HTML_ATTR(font_sizes_fixed), 63);

	/* This list has 2 elements */
	for(chPtr = strtok(size_list, ","), i = 0; i < 2 && chPtr != NULL;
		chPtr = strtok(NULL, ","), i++)
	{
		if((xmhtml_fn_fixed_sizes[i] = 10*atoi(chPtr)) == 0)
			xmhtml_fn_fixed_sizes[i] = def_fn_fixed_sizes[i];
	}
	/* fill up list if it is not complete */
	if(i != 2)
	{
		for(; i < 2; i++)
			xmhtml_fn_fixed_sizes[i] = def_fn_fixed_sizes[i];
	}

	_XmHTMLDebug(8, ( "\nfonts.c: initializeFontSizeLists, fixed font "
		"size list:\n"));
	_XmHTMLDebug(8, ( "%i %i", xmhtml_fn_fixed_sizes[0],
		xmhtml_fn_fixed_sizes[1]));

	/* list of possible font de/increments using the <FONT SIZE=""> element */
	xmhtml_basefont_sizes[0] = xmhtml_fn_sizes[1];	/* sub/superscript size */
	xmhtml_basefont_sizes[1] = xmhtml_fn_sizes[7];	/* H6 size */
	xmhtml_basefont_sizes[2] = xmhtml_fn_sizes[6];	/* H5 size */
	xmhtml_basefont_sizes[3] = xmhtml_fn_sizes[5];	/* H4 size */
	xmhtml_basefont_sizes[4] = xmhtml_fn_sizes[4];	/* H3 size (def font size)*/
	xmhtml_basefont_sizes[5] = xmhtml_fn_sizes[3];	/* H2 size */
	xmhtml_basefont_sizes[6] = xmhtml_fn_sizes[2];	/* H1 size */

#ifdef DEBUG
	_XmHTMLDebug(8, ( "\nfonts.c: initializeFontSizeLists, fallback "
		"font size list:\n"));
	for(i = 0 ; i < 7 ; i++)
		_XmHTMLDebug(8,( "%i ", xmhtml_basefont_sizes[i]));
	_XmHTMLDebug(8, ("\n"));
#endif

	/* First try to load the default font as specified by the resources */
	ok = False;

	HTML_ATTR(default_font) = loadQueryFont(html,
		HTML_ATTR(font_family), NULL, xmhtml_fn_sizes[0],
		FONT_SCALABLE|FONT_REGULAR|FONT_MEDIUM, &ok);

	/*****
	* We can't load the default font, try again with a wildcarded family.
	* This time die if it fails
	*****/
	if(HTML_ATTR(default_font) == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "initializeFontSizeLists"),
			XMHTML_MSG_36, HTML_ATTR(font_family));

		ok = True;
		HTML_ATTR(default_font) = loadQueryFont(html,
			HTML_ATTR(font_family), "*", xmhtml_fn_sizes[0],
			FONT_SCALABLE|FONT_REGULAR|FONT_MEDIUM, &ok);

		/* too bad, we absolutely need it */
		if(ok == False)
		{
			/* die */
			_XmHTMLError(__WFUNC__(html,"initializeFontSizeLists"),
				"Failed to find a default font for %s\n    Check previous "
				"messages and adjust default font", HTML_ATTR(font_family));
		}
	}

	_XmHTMLDebug(8,( "\nfonts.c: initializeFontSizeLists end.\n"));
}

/*****
* Name:			_XmHTMLSelectFontCache
* Return Type: 	XmHTMLfont*
* Description: 	selects a cache according to the display a widget is
*				being displayed on (or creates one if it isn't present yet)
* In:
*	html:		XmHTMLWidget id;
*	reset:		hard reset flag (used by SetValues when any of the font
*				resources changes);
* Returns:
*	the id of the default font for this cache;
*****/
XmHTMLfont*
_XmHTMLSelectFontCache(XmHTMLWidget html, Boolean reset)
{
	ToolkitAbstraction *tka = HTML_ATTR(tka);
	fontCache *cache;

	_XmHTMLDebug(8, ("fonts.c: _XmHTMLSelectFontCache start\n"));

	for(cache = master_cache; cache != NULL && cache->dpy != tka->dpy;
		cache = cache->next);

	if(cache == NULL)
	{
		cache = (fontCache*)malloc(sizeof(fontCache));
		cache->dpy = tka->dpy;
		cache->cache = (fontCacheEntry*)NULL;
		cache->default_font = (XmHTMLfont*)NULL;
		cache->nwidgets = 1;
		cache->widgets = (WidgetList)malloc(sizeof(Widget));
		cache->widgets[0] = (Widget)html;
		cache->next = (fontCache*)NULL;

		/*****
		* Determine screen resolution. The user can override these by
		* providing the horizontal and vertical resolution of the
		* display area.
		*****/
		/* adjust resolutions */
		if((cache->res_x = HTML_ATTR(res_x)) == 0)
		{
			cache->res_x = tka->width/(tka->widthMM/25.4);
			cache->res_x = (cache->res_x < 87 ? 75 : 100);
		}

		if((cache->res_y = HTML_ATTR(res_y)) == 0)
		{
			cache->res_y = tka->height/(tka->heightMM/25.4);
			cache->res_y = (cache->res_y < 87 ? 75 : 100);
		}

		/*****
		* We allow for skewed fonts if the user specified different
		* resolutions in x and y direction.
		*****/
		if(cache->res_x != cache->res_y &&
			HTML_ATTR(res_x) == HTML_ATTR(res_y))
		{
			if(cache->res_x > cache->res_y)
				cache->res_y = cache->res_x;
			else
				cache->res_x = cache->res_y;
		}

		cache->nentries = 0;
		cache->nmaps    = 0;
		cache->nlookups = 0;
		cache->requests = 0;
		cache->hits     = 0;
		cache->misses   = 0;

		if(master_cache)
		{
			fontCache *tmp;
			for(tmp = master_cache; tmp->next != NULL; tmp = tmp->next);
			tmp->next = cache;
		}
		else
			master_cache = cache;
		_XmHTMLDebug(8, ("fonts.c: _XmHTMLSelectFontCache, created first "
			"entry.\n"));
	}
	else
	{
		int i;

		/* see if we have got a reference for this widget */
		for(i = 0; i < cache->nwidgets && cache->widgets[i] != (Widget)html;
			i++);

		if(i == cache->nwidgets)
		{
			_XmHTMLDebug(8, ("fonts.c: _XmHTMLSelectFontCache, adding "
				"reference entry for widget %s.\n", XtName((Widget)html)));

			cache->widgets = (WidgetList)realloc(cache->widgets,
				(cache->nwidgets+1)*sizeof(Widget));
			cache->widgets[cache->nwidgets++] = (Widget)html;
		}
	}

	/*****
	* Only initialize font lists if the cache has changed, when we
	* are forced to do a reset or we haven't got a default font.
	*****/
	if(curr_cache != cache || reset || HTML_ATTR(default_font) == NULL)
	{
		curr_cache = cache;
		initializeFontSizeLists(html);
	}
	curr_cache->default_font = HTML_ATTR(default_font);

	return(curr_cache->default_font);
}

/*****
* Name: 		_XmHTMLLoadFont
* Return Type: 	XmHTMLfont*
* Description: 	loads a new font, with the style determined by the current
*				font: if current font is bold, and new is italic then a
*				bold-italic font will be returned.
* In:
*	w:			Widget for which to load a font
*	font_id:	id describing type of font to load.
*	size:		size of font to load. Only used for HT_FONT.
*	curr_font:	current font, required for propagating font style info.
* Returns:
*	the loaded font.
*****/
XmHTMLfont*
_XmHTMLLoadFont(XmHTMLWidget html, htmlEnum font_id, int size,
	XmHTMLfont *curr_font)
{
	XmHTMLfont *new_font = NULL;
	String family;
	int ptsz;
	Byte new_style = (Byte)0, font_style;
	Boolean ok = True;	/* enable font warnings */

	/* curr_font *must* always have a value as it references a cached font */
	my_assert(curr_font != NULL);

	/* pick up style of the current font */
	font_style = curr_font->style;

	_XmHTMLDebug(8,("_XmHTMLLoadFont: current font is %s %s %s.\n",
		(font_style & FONT_FIXED  ? "fixed"  : "scalable"),
		(font_style & FONT_BOLD   ? "bold"   : "medium"),
		(font_style & FONT_ITALIC ? "italic" : "regular")));

	/* See if we need to proceed with bold font */
	if(font_style & FONT_BOLD)
		new_style = FONT_BOLD;
	else
		new_style &= ~FONT_BOLD;

	/* See if we need to proceed with italic font */
	if(font_style & FONT_ITALIC)
		new_style |= FONT_ITALIC;
	else
		new_style &= ~FONT_ITALIC;

	/* See if we need to proceed with a fixed font */
	if(font_style & FONT_FIXED)
	{
		new_style |= FONT_FIXED;
		family = HTML_ATTR(font_family_fixed);
		ptsz = xmhtml_fn_fixed_sizes[0];
	}
	else
	{
		new_style &= ~FONT_FIXED;
		family = curr_font->font_family;
		ptsz = xmhtml_fn_sizes[0];
	}

	_XmHTMLDebug(8,("_XmHTMLLoadFont: next font is %s %s %s (inherited).\n",
		(new_style & FONT_FIXED  ? "fixed"  : "scalable"),
		(new_style & FONT_BOLD   ? "bold"   : "medium"),
		(new_style & FONT_ITALIC ? "italic" : "regular")));

	switch(font_id)
	{
		case HT_CITE:
		case HT_I:
		case HT_EM:
		case HT_DFN:
		case HT_ADDRESS:
			new_font = loadQueryFont(html, family, NULL,
				xmhtml_basefont_sizes[size-1], new_style|FONT_ITALIC, &ok);
			break;
		case HT_STRONG:
		case HT_B:
		case HT_CAPTION:
			new_font = loadQueryFont(html, family, NULL,
				xmhtml_basefont_sizes[size-1], new_style|FONT_BOLD, &ok);
			break;

		/*****
		* Fixed fonts always use the font specified by the value of the
		* fontFamilyFixed resource.
		*****/
		case HT_SAMP:
		case HT_TT:
		case HT_VAR:
		case HT_CODE:
		case HT_KBD:
 		case HT_PRE:	/* fix 01/20/97-03, kdh */
			new_font = loadQueryFont(html,
				HTML_ATTR(font_family_fixed), NULL, xmhtml_fn_fixed_sizes[0],
				new_style|FONT_FIXED, &ok);
			break;

		/* The <FONT> element is useable in *every* state */
		case HT_FONT:
			new_font = loadQueryFont(html, family, NULL, size,
				new_style, &ok);
			break;

		/*****
		* Since HTML Headings may not occur inside a <font></font> declaration,
		* they *must* use the specified document font, and not derive their
		* true font from the current font.
		*****/
		case HT_H1:
			new_font = loadQueryFont(html,
				HTML_ATTR(font_family), NULL, xmhtml_fn_sizes[2],
				FONT_SCALABLE|FONT_BOLD, &ok);
			break;
		case HT_H2:
			new_font = loadQueryFont(html,
				HTML_ATTR(font_family), NULL, xmhtml_fn_sizes[3],
				FONT_SCALABLE|FONT_BOLD, &ok);
			break;
		case HT_H3:
			new_font = loadQueryFont(html,
				HTML_ATTR(font_family), NULL, xmhtml_fn_sizes[4],
				FONT_SCALABLE|FONT_BOLD, &ok);
			break;
		case HT_H4:
			new_font = loadQueryFont(html,
				HTML_ATTR(font_family), NULL, xmhtml_fn_sizes[5],
				FONT_SCALABLE|FONT_BOLD, &ok);
			break;
		case HT_H5:
			new_font = loadQueryFont(html,
				HTML_ATTR(font_family), NULL, xmhtml_fn_sizes[6],
				FONT_SCALABLE|FONT_BOLD, &ok);
			break;
		case HT_H6:
			new_font = loadQueryFont(html,
				HTML_ATTR(font_family), NULL, xmhtml_fn_sizes[7],
				FONT_SCALABLE|FONT_BOLD, &ok);
			break;

		/* should never be reached */
		default:
#ifdef PEDANTIC
			_XmHTMLWarning(__WFUNC__(html, "_XmHTMLLoadFont"), XMHTML_MSG_37);
#endif /* PEDANTIC */
			/* this will always succeed */
			ok = False;
			new_font = loadQueryFont(html, family, NULL, ptsz,
				FONT_SCALABLE|FONT_REGULAR|FONT_MEDIUM, &ok);
			break;
	}
	return(new_font);
}

/*****
* Name: 		_XmHTMLLoadFontWithFace
* Return Type: 	XmHTMLfont*
* Description: 	load a new font with given pixelsize and face.
*				Style is determined by the current font: if current font
*				is bold, and new is italic then a bold-italic font will be
*				returned.
* In:
*	w:			Widget for which to load a font
*	size:		size of font to load. Only used for HT_FONT.
*	face:		a comma separated list of font faces to use, contents are
*				destroyed when this function returns.
*	curr_font:	current font, required for propagating font style info.
* Returns:
*	A new font with a face found in the list of faces given upon success
*	or the default font on failure.
*****/
XmHTMLfont*
_XmHTMLLoadFontWithFace(XmHTMLWidget html, int size, String face,
	XmHTMLfont *curr_font)
{
	XmHTMLfont *new_font = NULL;
	String chPtr, family, all_faces, first_face = NULL;
	Byte new_style = (Byte)0, font_style;
	int tries;

	/* curr_font *must* always have a value as it references a cached font */
	my_assert(curr_font != NULL);

	/* pick up style of the current font */
	font_style = curr_font->style;

	/* See if we need to proceed with bold font */
	if(font_style & FONT_BOLD)
		new_style = FONT_BOLD;
	else
		new_style &= ~FONT_BOLD;

	/* See if we need to proceed with italic font */
	if(font_style & FONT_ITALIC)
		new_style |= FONT_ITALIC;
	else
		new_style &= ~FONT_ITALIC;

	/*****
	* See if we need to proceed with a fixed font, only used to determine
	* initial font family.
	*****/
	if(font_style & FONT_FIXED)
	{
		new_style |= FONT_FIXED;
		family = HTML_ATTR(font_family_fixed);
	}
	else
	{
		new_style &= ~FONT_FIXED;
		family = HTML_ATTR(font_family);
	}

	/* we must have a ``,'' or strtok will fail */
	if((strstr(face, ",")) == NULL)
	{
		all_faces = (String)malloc(strlen(face) + 2);
		strcpy(all_faces, face);
		strcat(all_faces, ",\0");
	}
	else
		all_faces = strdup(face);

	/* walk all possible spaces */
	tries = 0;
	for(chPtr = strtok(all_faces, ","); chPtr != NULL;
		chPtr = strtok(NULL, ","))
	{
		Boolean ok = False;

		tries++;

		/* skip any leading spaces */
		while(isspace(*chPtr))
			chPtr++;

		_XmHTMLDebug(8, ("format.c: _XmHTMLLoadFontWithFace, trying with "
			"face %s\n", chPtr));

		/*****
		* Disable font not found warning message, we are trying to find
		* a font of which we don't know if it exists.
		*****/
		ok = False;
		new_font = loadQueryFont(html, family, chPtr, size,
			new_style, &ok);
		if(new_font && ok)
		{
			_XmHTMLDebug(8, ("format.c: _XmHTMLLoadFontWithFace, font "
				"loaded.\n"));
			break;
		}
		if(tries == 1)
			first_face = strdup(chPtr);
	}
	free(all_faces);
	/*****
	* hmm, the first font in this face specification didn't yield a valid
	* font. To speed up things considerably, we add a font mapping for the
	* first face in the list of given faces. There's no sense in doing this
	* when there is only one face specified as this will always get us the
	* default font. We only add a mapping if the name of the returned font
	* contains at least one of the allowed faces. Not doing this check would
	* ignore face specs which do have a face we know. We also want the font
	* styles to match as well.
	* BTW: this is a tremendous speedup!!!
	*****/
	if(first_face)
	{
		/*****
		* Only add a mapping if the returned name contains one of the allowed
		* faces. No need to check for the presence of a comma: we only take
		* lists that have multiple face specifications.
		*****/
		if(tries > 1)
		{
			/*****
			* Walk all possible faces. Nukes the face array but that's not
			* bad as we are the only ones using it.
			*****/
			for(chPtr = strtok(face, ","); chPtr != NULL;
				chPtr = strtok(NULL, ","))
			{
				/* skip any leading spaces */
				while(isspace(*chPtr))
					chPtr++;
				/* caseless 'cause fontnames ignore case */
				if(my_strcasestr(new_font->font_name, chPtr) &&
					new_font->style == new_style)
				{
					_XmHTMLaddFontMapping(html, family, first_face, size,
						new_style, new_font);
					break;
				}
			}
		}
		free(first_face);
	}
	return(new_font);
}

/*****
* Name:			_XmHTMLUnloadFonts
* Return Type: 	void
* Description: 	removes a widget from the widget list of a display-bound
*				font cache. When the reference count of this cache reaches
*				zero, the cache is released;
* In:
*	html:		XmHTMLWidget id;
* Returns:
*	nothing.
*****/
void
_XmHTMLUnloadFonts(XmHTMLWidget html)
{
	ToolkitAbstraction *tka = HTML_ATTR(tka);
	fontCache *cache;
	int i;

	/* get current font cache */
	for(cache = master_cache; cache != NULL && cache->dpy != tka->dpy;
		cache = cache->next);

	if(cache == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLUnloadFonts"),
			XMHTML_MSG_38, "display");
			return;
	}
	for(i = 0; i < cache->nwidgets && cache->widgets[i] != (Widget)html; i++);

	if(i == cache->nwidgets)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLUnloadFonts"),
			XMHTML_MSG_39, "widget");
			return;
	}

	_XmHTMLDebug(8,( "\nfonts.c: _XmHTMLUnloadFonts, removing reference for "
		"widget %s.\n", XtName((Widget)html)));

	/* invalidate current cache? */
	if(cache == curr_cache)
		curr_cache = (fontCache*)NULL;

	/* remove this widget */
	cache->widgets[i] = (Widget)NULL;
	for(; i < cache->nwidgets - 1; i++)
		cache->widgets[i] = cache->widgets[i+1];

	cache->nwidgets--;

	/* if this was the last widget, free it */
	if(cache->nwidgets == 0)
	{
		fontCache *tmp;

		_XmHTMLDebug(8, ("fonts.c: _XmHTMLUnloadFonts, releasing font "
			"cache.\n"));

		/* special case, first entry is to be freed */
		if(cache == master_cache)
			master_cache = cache->next;
		else
		{
			for(tmp = master_cache; tmp->next != cache; tmp = tmp->next);

			/* connect next entry */
			tmp->next = cache->next;
		}
		/* free the entire list of cached fonts */
		freeFontEntries(tka, cache->cache);
		free(cache->widgets);
		free(cache);
	}
#ifdef DEBUG
	else
	{
		_XmHTMLDebug(8, ("fonts.c: _XmHTMLUnloadFonts, cache still "
			"referenced by the following widgets:\n"));
		for(i = 0; i < cache->nwidgets; i++)
			_XmHTMLDebug(8, ("\t%s\n", XtName(cache->widgets[i])));
	}
#endif
}

void
fillCacheInfo(fontCacheEntry *entry, XmHTMLFontCacheInfo *info)
{
	if(entry)
	{
		fillCacheInfo(entry->left, info);

		info->fonts[info->nentries] = entry->name;
		if(entry->is_map)
			info->mapping[info->nentries] = entry->map_to->font_name;
		else
			info->mapping[info->nentries] = NULL;
		info->nentries++;

		fillCacheInfo(entry->right, info);
	}
}

XmHTMLFontCacheInfo*
XmHTMLGetFontCacheInfo(Widget w)
{
	Display *dpy;
	fontCache *cache;
	static XmHTMLFontCacheInfo *info;

	dpy = XtDisplay(w);

	/* sanity check */
	if(dpy == NULL)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "XmHTMLGetFontCacheInfo"),
			XMHTML_MSG_39, "(null)");
		return(NULL);
	}

	/* pick up correct master cache */
	for(cache = master_cache; cache != NULL && cache->dpy != dpy;
		cache = cache->next);

	/* not found */
	if(cache == NULL)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "XmHTMLGetFontCacheInfo"),
			XMHTML_MSG_39, DisplayString(dpy));
		return(NULL);
	}

	info = (XmHTMLFontCacheInfo*)malloc(sizeof(XmHTMLFontCacheInfo));

	info->nentries  = cache->nentries;		/* no of cached fonts */
	info->nmaps     = cache->nmaps;			/* no of mapped fonts */
	info->nlookups  = cache->nlookups;		/* no of search actions */
	info->nrequests = cache->requests;		/* no of requests made */
	info->hits      = cache->hits;			/* no of hits */
	info->misses    = cache->misses;		/* no of font cache misses */
	info->nwidgets  = cache->nwidgets;		/* no of widgets using this cache */
	info->widgets   = cache->widgets;
	info->fonts     = (String*)calloc(info->nentries, sizeof(String));
	info->mapping   = (String*)calloc(info->nentries, sizeof(String));

	info->nentries = 0;
	fillCacheInfo(cache->cache, info);

	return(info);
}

void
XmHTMLFreeFontCacheInfo(XmHTMLFontCacheInfo *info)
{
	if(info == NULL)
		return;

	free(info->fonts);
	free(info->mapping);
	free(info);
}
