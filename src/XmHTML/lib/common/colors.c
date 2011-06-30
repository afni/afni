#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* colors.c : XmHTML color allocation routines
*
* This file Version	$Revision$
*
* Creation date:		Mon Dec 16 13:57:41 GMT+0100 1996
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
* Revision 1.15  1998/04/27 06:58:36  newt
* tka stuff and removed all color caching stuff, it was useless.
*
* Revision 1.14  1998/04/04 06:28:02  newt
* XmHTML Beta 1.1.3
*
* Revision 1.13  1997/10/23 00:24:51  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.12  1997/08/31 17:33:07  newt
* log edit
*
* Revision 1.11  1997/08/30 00:46:39  newt
* Changed _XmHTMLConfirmColor32 proto from void to Boolean. Added
* _XmHTMLRecomputeColors.
*
* Revision 1.10  1997/08/01 12:57:49  newt
* my_strdup -> strdup
*
* Revision 1.9  1997/05/28 01:45:14  newt
* Changes for the XmHTMLAllocColor and XmHTMLFreeColor functions.
*
* Revision 1.8  1997/04/29 14:24:51  newt
* Fix in _XmHTMLFreeColors
*
* Revision 1.7  1997/04/03 05:33:37  newt
* _XmHTMLGetPixelByName is much more robuster and fault tolerant
* (patch by Dick Ported, dick@cymru.net)
*
* Revision 1.6  1997/03/20 08:08:18  newt
* fixed a few bugs in BestPixel and _XmHTMLFreeColors
*
* Revision 1.5  1997/03/02 23:15:32  newt
* some obscure free() changes
*
* Revision 1.4  1997/02/11 02:06:32  newt
* Bugfixes related to color releasing.
*
* Revision 1.3  1997/01/09 06:55:20  newt
* expanded copyright marker
*
* Revision 1.2  1997/01/09 06:43:42  newt
* small fix on ConfirmColor32
*
* Revision 1.1  1996/12/19 02:17:07  newt
* Initial Revision
*
*****/ 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "toolkit.h"
#include XmHTMLPrivateHeader

#ifdef HAVE_XCCP_H
#include "XCCP.h"
#endif

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/
static int CreateColormap(XmHTMLWidget html, XCOLOR *cmap);

/*** Private Variable Declarations ***/
static Boolean confirm_warning = True;

/* HTML-3.2 color names */
static String html_32_color_names[16] = {"black", "silver", "gray", 
	"white", "maroon", "red", "purple", "fuchsia", "green", 
	"lime", "olive", "yellow", "navy", "blue", "teal", 
	"aqua"};

/* corresponding HTML-3.2 sRGB values */
static String html_32_color_values[16] = {"#000000", "#c0c0c0", "#808080", 
	"#ffffff", "#800000", "#ff0000", "#800080", "#ff00ff", "#008000", 
	"#00ff00", "#808000", "#ffff00", "#000080", "#0000ff", "#008080", 
	"#00ffff"};

/* for creating a 3/3/2 based palette */
#define RMASK		0xe0
#define RSHIFT		0
#define GMASK		0xe0
#define GSHIFT		3
#define BMASK		0xc0
#define BSHIFT		6

/*****
* Name: 		tryColor
* Return Type: 	Boolean
* Description: 	verifies the validity of the given colorname and returns
*				the corresponding RGB components for the requested color.
* In: 
*	dpy:		Display on which color should be allocated;
*	colormap:	colormap in which color should be allocated;
*	color:		name of color to allocate. Either symbolic or an RGB triplet;
*	*defs:		XCOLOR struct for allocated color. Filled upon return.
* Returns:
*	True when color name is valid, False if not.
* Note:
*	This routine tries to recover from incorrectly specified RGB triplets.
*	(not all six fields present).
*****/
static Boolean
tryColor(ToolkitAbstraction *tka, COLORMAP colormap, String color, XCOLOR *def)
{
	/*
	* backup color for stupid html writers that don't use a leading hash.
	* The 000000 will ensure we have a full colorspec if the user didn't
	* specify the full symbolic color name (2 red, 2 green and 2 blue).
	*/
	char hash[]="#000000";
	int i;

	/* first try original name */
	if(!(tka->ParseColor(tka->dpy, colormap, color, def)))
	{
		/*
		* Failed, see if we have a leading hash. This doesn't work with
		* symbolic color names. Too bad then.
		*/
		if(color[0] != '#')
		{
			/*
			* Only prepend the hash sign by setting the second char to NULL.
			* Can't initialize hash this way (above that is) since the literal
			* copy below won't work that way. Don't ask me why, it just won't
			* work.
			*/
			hash[1] = '\0';
			strncat(hash, color, 6);
		}
		/*
		* Copy up to seven chars. This will make a valid color spec
		* even if the full triplet hasn't been specified. The strlen check
		* to prevent a buffer overflow.
		*/
		else
			/* literal copy so we get a valid triplet */
			if(strlen(color) < 7)
			{
				for(i = 0; i < strlen(color); i++)
					hash[i] = color[i];
				/* complete with zeros */
				for(; i < 7; i++)
					hash[i] = (char)0;
			}
			else
				strncpy(hash, color, 7);
		/* NULL terminate */
		hash[7] = '\0';

		/* try again */
		if(!(tka->ParseColor(tka->dpy, colormap, hash, def)))
			return(False);
	}
	return(True);
}

/*****
* Name: 		_XmHTMLGetPixelByName
* Return Type: 	Pixel
* Description: 	retrieves the pixel value for a color name. Color can be
*				given as a color name as well as a color value.
* In: 
*	display:	display where color value should be retrieved from
*	color:		the color to allocate.
*	def_pixel:	default pixel to return if color allocation fails
* Returns:
*	The pixel value closest to the requested color
* Note:
*	It's useless to employ some sort of color caching here, the color
*	allocation code in XCC is fast *and* smart.
*****/
Pixel
_XmHTMLGetPixelByName(XmHTMLWidget html, String color, Pixel def_pixel)
{
	ToolkitAbstraction *tka = HTML_ATTR(tka);
	XCOLOR def;
	COLORMAP colormap;
	Pixel pixel[1];
	unsigned short r[1], g[1], b[1];
	int success = False;	/* REQUIRED */

	/* sanity check */
	if(!color || *color == '\0')
		return(def_pixel);

	/*****
	* See if we have an xcc for this widget. Since CheckXCC can call
	* CheckGC, which calls XCreateGC, we *must* make sure that we have
	* a valid window available. BadWindow errors will occur otherwise.
	*****/
	if(tka->win == None)
	{
		WINDOW win = CORE_ATTR(window);

		if(win == None)
			win = tka->defaultRoot;

		XmHTMLTkaSetDrawable(tka, win);
	}

	_XmHTMLCheckXCC(html);
	colormap = TkaGetColormap(html);

	if((!tryColor(tka, colormap, color, &def)))
	{
		Boolean again;

		/* turn of warnings */
		confirm_warning = False;

		/* see if by chance it's one of the 16 appointed colors */
		again = _XmHTMLConfirmColor32(color);

		/* turn on warnings */
		confirm_warning = True;

		/* try it */
		if(!again || !tryColor(tka, colormap, color, &def))
		{
			/* bad color spec, return */
			_XmHTMLWarning(__WFUNC__(html, "_XmHTMLGetPixelByName"),
				XMHTML_MSG_29, color);
			return(def_pixel);
		}
	}

	/* try to allocate it */
	r[0] = GETR(def);
	g[0] = GETG(def);
	b[0] = GETB(def);
	pixel[0] = None;		/* REQUIRED! */

	XCCGetPixels(HTML_ATTR(xcc), r, g, b, 1, pixel, &success);

	if(!success)
	{
		/* failed, return default pixel */
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLGetPixelByName"), 
			XMHTML_MSG_30, color);
		return(def_pixel);
	}
	/* success! */
	return(pixel[0]);
}

/*****
* Name: 		_XmHTMLConfirmColor32
* Return Type: 	void
* Description: 	converts the given named color to the corresponding sRGB value.
* In: 
*	color:		color name to check
* Returns:
*	nothing, but if a match is found, color is updated with the corresponding
*	sRGB value.
* Note:
*	This routine is here for consistency. The standard HTML 3.2 colors are
*	referred to as the ``standard 16 color Windows VGA pallete''. This
*	uttermost *dumb*, *stupid* (you name it) pallete does not only contain an
*	absolute minimum of 16 colors, but in addition, most of the color names 
*	used are unknown to almost all X servers! Can you imagine a greater m$
*	ignorance!!!! Yuck.
*****/
Boolean
_XmHTMLConfirmColor32(char *color)
{
	register int i;

	/* an sRGB spec, see if we know it */
	if(color[0] == '#')
	{
		for(i = 0 ; i < 16; i++)
		{
			if(!strcasecmp(color, html_32_color_values[i]))
				return(True);
		}
	}
	else
	{
		for(i = 0 ; i < 16; i++)
		{
			if(!strcasecmp(color, html_32_color_names[i]))
			{
				color = realloc(color, strlen(html_32_color_values[i]));
				strcpy(color, html_32_color_values[i]);
				color[strlen(html_32_color_values[i])] = '\0';
				return(True);
			}
		}
	}
	/* nope, don't know it. Use black */
	if(confirm_warning)
		_XmHTMLWarning(__WFUNC__(NULL, "_XmHTMLConfirmColor32"), 
			XMHTML_MSG_31, color);
	return(False);
}

/*****
* Name: 		XmHTMLAllocColor
* Return Type: 	Pixel
* Description: 	allocates the named color and takes the XmNmaxImageColors
*				resource into account.
* In: 
*	w:			XmHTMLWidget id;
*	color:		colorname, either symbolic or an RGB triplet.
*	def_pixel:	pixel to return when allocation of "color" fails.
* Returns:
*	allocated pixel upon success or def_pixel upon failure to allocate "color".
*****/
Pixel
XmHTMLAllocColor(Widget w, String color, Pixel def_pixel)
{
	XCOLOR def;
	COLORMAP colormap;
	ToolkitAbstraction *tka = NULL;
	int success = True;

	/* sanity check */
	if(!w || !color || *color == '\0')
	{
		_XmHTMLWarning(__WFUNC__(w, "XmHTMLAllocColor"), XMHTML_MSG_21,
			w ? "NULL color name" : "NULL parent", "XmHTMLAllocColor");
		return(def_pixel);
	}

	tka = XmHTMLTkaCreate();
	XmHTMLTkaSetDisplay(tka, w);

	/*
	* Get colormap for this widget. Will always succeed as all widgets
	* (and gadgets as well) are subclassed from core.
	*/
	colormap = TkaGetColormap(w);

	if((!tryColor(tka, colormap, color, &def)))
	{
		/* bad color spec, return */
		_XmHTMLWarning(__WFUNC__(w, "XmHTMLAllocColor"), XMHTML_MSG_29, color);
		return(def_pixel);
	}

	/* try to allocate it */
	if(!tka->AllocColor(tka->dpy, colormap, &def))
	{
		/*
		* Initial allocation failed, try to find a close match from any
		* colors already allocated in the colormap
		*/
		XCOLOR *cmap;
		VISUAL *visual = NULL;
		int cmapsize;
		int d, mdist, close, ri, gi, bi;
		register int i, rd, gd, bd;

		_XmHTMLDebug(7, ("colors.c: XmHTMLAllocColor: first stage allocation "
			"for %s failed, trying to match it.\n", color));

		/* Get a suitable visual */
		visual = XCCGetParentVisual(w);

		/* we only use up to XmHTML_MAX_IMAGE_COLORS */
		cmapsize = (visual->map_entries > XmHTML_MAX_IMAGE_COLORS ?
			XmHTML_MAX_IMAGE_COLORS : visual->map_entries);

		cmap = (XCOLOR*)malloc(cmapsize*sizeof(XCOLOR));

		/* initialise pixels */
		for(i = 0; i < cmapsize; i++)
		{
			GETP(cmap[i]) = (Pixel)i;
			GETR(cmap[i]) = GETG(cmap[i]) = GETB(cmap[i]) = 0;
		}

		/* read the colormap */
		tka->QueryColors(tka->dpy, colormap, cmap, cmapsize);

		/* speedup: downscale here instead of in the matching code */
		for(i = 0; i < cmapsize; i++)
		{
			GETR(cmap[i]) >>= 8;
			GETG(cmap[i]) >>= 8;
			GETB(cmap[i]) >>= 8;
		}

		mdist = 0x1000000;
		close = -1;

		/* downscale */
		ri = (GETR(def) >> 8);
		gi = (GETG(def) >> 8);
		bi = (GETB(def) >> 8);

		/* 
		* walk all colors in the colormap and see which one is the 
		* closest. Uses plain least squares.
		*/
		for(i = 0; i < cmapsize && mdist != 0; i++)
		{
			rd = ri - GETR(cmap[i]);
			gd = gi - GETG(cmap[i]);
			bd = bi - GETB(cmap[i]);

			if((d = (rd*rd) + (gd*gd) + (bd*bd)) < mdist)
			{
				close = i;
				mdist = d;
			}
		}
		if(close != -1)
		{
			/* we got a match, try to allocate this color */
			GETR(def) = (GETR(cmap[close]) << 8);
			GETG(def) = (GETG(cmap[close]) << 8);
			GETB(def) = (GETB(cmap[close]) << 8);
			if(!tka->AllocColor(tka->dpy, colormap, &def))
				success = False;
		}
		else
			success = False;

		/* no longer needed */
		free(cmap);
	}
	/* no longer needed */
	free(tka);

	if(success == False)
	{
		/* failed, return default pixel */
		_XmHTMLWarning(__WFUNC__(w, "_XmHTMLGetPixelByName"), XMHTML_MSG_30,
			color);
		return(def_pixel);
	}
	_XmHTMLDebug(7, ("colors.c: XmHTMLAllocColor: %s allocated!\n", color));
	return(GETP(def));
}

/*****
* Name: 		XmHTMLFreeColor
* Return Type: 	void
* Description: 	releases an allocated pixel
* In: 
*	w:			XmHTMLWidget id;
*	pixel:		pixel to be freed.
* Returns:
*	nothing.
*****/
void
XmHTMLFreeColor(Widget w, Pixel pixel)
{
	ToolkitAbstraction *tka;

	/* sanity check */
	if(!w)
	{
		_XmHTMLBadParent(w, "FreeColor");
		return;
	}

	tka = XmHTMLTkaCreate();
	XmHTMLTkaSetDisplay(tka, w);
	tka->FreeColors(tka->dpy, TkaGetColormap(w), &pixel, 1, 0L);

	XmHTMLTkaDestroy(tka);
}

Boolean
_XmHTMLAddPalette(XmHTMLWidget html)
{
	XCOLOR cmap[XmHTML_MAX_IMAGE_COLORS];
	int ncolors = 0, nlines = 0;
	int i,r,g,b;
	String chPtr;

	if(HTML_ATTR(palette) != NULL)
	{
		chPtr = HTML_ATTR(palette);
		/* skip leading whitespace */
		while(*chPtr != '\0' && isspace(*chPtr))
		{
			if(*chPtr == '\n')
				nlines++;
			chPtr++;
		}
		while(*chPtr != '\0' && ncolors < XmHTML_MAX_IMAGE_COLORS)
		{
			if((sscanf(chPtr, "%x %x %x", &r, &g, &b)) != 3)
			{
				_XmHTMLWarning(__WFUNC__(html, "_XmHTMLAddPalette"),
					XMHTML_MSG_32, nlines);
				/* skip to next entry */
				while(*chPtr != '\0' && !isspace(*chPtr))
					chPtr++;
			} 
			else
			{
				RANGE(r,0,255);
				RANGE(g,0,255);
				RANGE(b,0,255);
				GETR(cmap[ncolors]) = r;
				GETG(cmap[ncolors]) = g;
				GETB(cmap[ncolors]) = b;
				ncolors++;
				/* skip them */
				for(i = 0; i < 3; i++)
				{
					while(*chPtr != '\0' && isalnum(*chPtr))
						chPtr++;
					while(*chPtr != '\0' && isspace(*chPtr))
					{
						if(*chPtr == '\n')
							nlines++;
						chPtr++;
					}
				}
			}
			/* move to next slot */
			while(*chPtr != '\0' && isspace(*chPtr))
			{
				if(*chPtr == '\n')
					nlines++;
				chPtr++;
			}
		}

		/* check against maxImageColors */
		if(ncolors != HTML_ATTR(max_image_colors))
		{
			if(ncolors < HTML_ATTR(max_image_colors))
				HTML_ATTR(max_image_colors) = ncolors;
			else
			{
				/* check how many colors are really allowed on this display */
				if(ncolors < XCCGetNumColors(HTML_ATTR(xcc)))
					HTML_ATTR(max_image_colors) = ncolors;
				else
					ncolors = HTML_ATTR(max_image_colors);
			}
		}
	}
	else
		ncolors = CreateColormap(html, &cmap[0]);

	/* upscale to 0-2^16 */
	for(i = 0; i < ncolors; i++)
	{
		GETR(cmap[i]) <<= 8;
		GETG(cmap[i]) <<= 8;
		GETB(cmap[i]) <<= 8;
	}

	/* allocate this palette */
	ncolors = XCCAddPalette(HTML_ATTR(xcc), cmap, ncolors);

	/* check if we need to initialize dithering */
	if(HTML_ATTR(map_to_palette) == XmBEST ||
		HTML_ATTR(map_to_palette) == XmFAST)
		XCCInitDither(HTML_ATTR(xcc));

	/* silently adjust max_image_colors */
	HTML_ATTR(max_image_colors) = ncolors;

	_XmHTMLDebug(7, ("colors.c, _XmHTMLAddPalette, added a palette with %i "
		"colors\n", ncolors));

	return(True);
}

#ifdef DITHER_SIMPLE_COLORMAP
static int
CreateColormap(XmHTMLWidget html, XCOLOR *cmap)
{
	int i, idx, ncolors;
	float mul;

	ncolors = HTML_ATTR(max_image_colors);
	mul = (float)XmHTML_MAX_IMAGE_COLORS/(float)HTML_ATTR(max_image_colors);

	if(HTML_ATTR(xcc)->mode == MODE_BW || HTML_ATTR(xcc)->mode == MODE_MY_GRAY)
	{
		/* grayscale visual */
		for (i = 0;  i < ncolors;  ++i)
		{
			idx = (int)(i * mul);
			GETR(cmap[i]) = idx;
			GETG(cmap[i]) = idx;
			GETB(cmap[i]) = idx;
		}
	}
	else
	{
		for(i = 0; i < ncolors; i++)
		{
			idx = (int)(i * mul);
			GETR(cmap[i]) = (((idx << 0) & 0xe0)*255 + 0.5*0xe0)/0xe0;
			GETG(cmap[i]) = (((idx << 3) & 0xe0)*255 + 0.5*0xe0)/0xe0;
			GETB(cmap[i]) = (((idx << 6) & 0xc0)*255 + 0.5*0xc0)/0xc0;
		}
	}
	return(ncolors);
}

#else
/*****
* Name:			CreateColormap
* Return Type: 	int
* Description: 	creates a colormap (with equally spaced color components)
* In: 
*	html:		XmHTMLWidget id;
*	cmap:		colormap storage room. Filled upon return.
* Returns:
*	actual size of colormap. This is at most equal to the value of the
*	XmNmaxImageColors resource (unless the value of this resource is less than
*	8, in which case it will be set to 8).
*****/
static int
CreateColormap(XmHTMLWidget html, XCOLOR *cmap)
{
	int iroot, nc, max_colors, blksize, blkdist, nci, val, maxsample;
	int i, j, k, l, total_colors, Ncolors[3], temp;
	static int RGB[3] = {1, 0, 2};
	Boolean changed;
	Byte **colormap;

	/* number of components per color */
	if(HTML_ATTR(xcc)->mode == MODE_BW || HTML_ATTR(xcc)->mode == MODE_MY_GRAY)
		nc = 1;	/* grayscale */
	else
		nc = 3;	/* color */

	/*****
	* requested colormap size.
	* To get an even distribution of the colors, we require this value to be
	* a triple power, with a minumum of 8 colors. 
	*****/
	max_colors = HTML_ATTR(max_image_colors);
	if(max_colors < 8)
		max_colors = 8;

	iroot = 1;
	do
	{
		iroot++;
		temp = iroot;
		for(i = 1; i < nc; i++)
			temp *= iroot;
	}while(temp <= max_colors);
	iroot--;

	/* Set number of distinct values for each color component */
	total_colors = 1;
	for(i = 0; i < nc; i++)
	{
		Ncolors[i] = iroot;
		total_colors *= iroot;
	}

	/*****
	* See if we can increase the number of distinct color components
	* without exceeding the allowed number of colors.
	*****/
	do
	{
		changed = False;
		for(i = 0; i < nc; i++)
		{
			j = (nc == 1 ? 0 : RGB[i]);
			temp = total_colors/Ncolors[j];
			temp *= Ncolors[j]+1;
			if(temp > max_colors)
				break;	/* too large, done with this pass */
			Ncolors[j]++;
			total_colors = (int)temp;
			changed = True;
		}
	}while(changed);

	if(total_colors != html->html.max_image_colors)
	{
		_XmHTMLWarning(__WFUNC__(html, "makeDitherCmap"), XMHTML_MSG_33,
			HTML_ATTR(max_image_colors), total_colors, XmHTML_MAX_IMAGE_COLORS);
		HTML_ATTR(max_image_colors) = total_colors;
	}

	/* temporary storage */
	colormap = (Byte**)calloc(nc, sizeof(Byte*));
	for(i = 0; i < nc; i++)
		colormap[i] = (Byte*)calloc(total_colors, sizeof(Byte));

	/* distance between groups of identical entries for a component */
	blkdist = total_colors;

	/* maximum value of a single color component */
	maxsample = XmHTML_MAX_IMAGE_COLORS-1;

	/* now go and fill the palette */
	for(i = 0; i < nc; i++)
	{
		/* fill in entries for i'th color */
		nci = Ncolors[i];	/* no of distinct values for this color */
		blksize = blkdist/nci;
		for(j = 0; j < nci; j++)
		{
			/* get color value */
			val = (int)(((unsigned long)(j * maxsample + (nci-1)/2))/ (nci-1));
			/* fill all entries that have this value for this component. */
			for(k = j * blksize; k < total_colors; k+= blkdist)
			{
				/* fill in blksize entries starting at k */
				for(l = 0; l < blksize; l++)
					colormap[i][k+l] = (Byte)val;
			}
		}
		blkdist = blksize;	/* size of this color is offset to next color */
	}
	/* now save colormap in private storage */
	if(nc == 1) /* grayscale */
	{
		for(i = 0; i < total_colors; i++)
			GETR(cmap[i]) = GETG(cmap[i]) = GETB(cmap[i]) = colormap[0][i];
	}
	else	/* rgb map */
	{
		for(i = 0; i < total_colors; i++)
		{
			GETR(cmap[i]) = colormap[0][i];
			GETG(cmap[i]) = colormap[1][i];
			GETB(cmap[i]) = colormap[2][i];
		}
	}
	/* no longer needed */
	for(i = 0; i < nc; i++)
		free(colormap[i]);
	free(colormap);

	/* all done */
	return(total_colors);
}
#endif
