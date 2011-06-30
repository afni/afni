#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* XCC.c : XColorContext routines.
*
* This file Version	$Revision$
*
* Creation date:		Mon Mar  3 00:28:16 GMT+0100 1997
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
* Id: XCC.c,v 1.16 1995/08/10 04:08:41 cwikla
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
* ChangeLog 
* $Log$
* Revision 1.1  2011/06/30 16:10:30  rwcox
* Cadd
*
* Revision 1.30  1998/04/27 06:56:02  newt
* tka stuff
*
* Revision 1.29  1998/04/04 06:27:49  newt
* XmHTML Beta 1.1.3
*
* Revision 1.28  1997/10/23 00:24:41  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.27  1997/08/31 17:31:35  newt
* Several bugfixes, rr
*
* Revision 1.26  1997/08/30 00:30:39  newt
* Color HashTable changes & preparations for fixed palette.
*
* Revision 1.25  1997/08/01 12:54:15  newt
* Progressive image loading: XCCGetPixelsIncremental. Some dead code eliminated.
*
* Revision 1.24  1997/05/28 01:34:50  newt
* Modified XCCCreate to support the XmNmaxImageColors resource. Added a fourth
* level to XCCGetPixels to map unallocated colors to any allocated color and a
* fifth level to map any remaining colors to black as a last resort.
*
* Revision 1.23  1997/04/29 14:21:09  newt
* bugfix 04/23/97-01
*
* Revision 1.22  1997/04/03 05:28:55  newt
* Added XCCFreeColors
*
* Revision 1.21  1997/03/28 07:04:10  newt
* ?
*
* Revision 1.20  1997/03/20 08:02:45  newt
* replaced bcopy and bzero by memcpy and memset
*
* Revision 1.19  1997/03/11 19:46:25  newt
* Replaced fatal errors in XCCGetPixels by a BlackPixel substitution
*
* Revision 1.18  1997/03/04 00:55:26  newt
* Small bugfix: ColorAtomList and GrayAtomList was defined twice
*
* Revision 1.17  1997/03/02 23:44:21  newt
* Expanded copyright marker
*
*****/ 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>

#ifdef DMALLOC
#include "dmalloc.h"
#endif

#include "toolkit.h"
#include XmHTMLPrivateHeader
#include "XCCP.h"

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/
#define NUMBER(a) ((int)(sizeof(a)/sizeof(a[0])))
static int _pixelSort(const void *_arg1, const void *_arg2);
static void _queryColors(XCC _xcc);
static int _findGoodCube(XCC _xcc, Atom _atom, XStandardColormap *_matchedCube);
static int _lookForStdCmap(XCC _xcc, Atom _atom);
static void _initBW(XCC _xcc);
static void _initGray(XCC _xcc);
static void _initColor(XCC _xcc);
static void _initTrueColor(XCC _xcc);
static void _initDirectColor(XCC _xcc);
static void _initPalette(XCC _xcc);

static void table_idestroy(HashTable *table);
static HashEntry * delete_fromilist(HashTable *table, HashEntry *entry,
	unsigned long key);
static void rebuild_itable(HashTable *table);

/*** Private Variable Declarations ***/

Atom ColorAtomList[] =
{
	XA_RGB_DEFAULT_MAP,
	XA_RGB_BEST_MAP,
	XA_RGB_GRAY_MAP,
};
Atom GrayAtomList[] =
{
	XA_RGB_GRAY_MAP,
	XA_RGB_DEFAULT_MAP,
};

Visual*
XCCGetParentVisual(Widget w)
{
	Widget parent, tmp = w;
	Visual *visual = NULL;

	/* First try to get the Visual from the widget */
	XtVaGetValues(w,
		XmNvisual, &visual,
		NULL);

	if(visual != NULL)
		return(visual);

	/*
	* Walk the widget tree until we either run out of widgets or we have
	* a widget that is a subclass of Shell (which has the XmNvisual resource)
	*/
	while(True)
	{
		parent = XtParent(tmp);
		if(parent == NULL || XtIsShell(parent))
			break;
		tmp = parent;
	}
	/* if we have a parent it's a subclass of shell and thus a visual is here */
	if(parent)
		XtVaGetValues(parent, XmNvisual, &visual, NULL);

	/*
	* parent == NULL or shell didn't have a visual (very unlikely)
	* fallback to the default visual.
	*/
	if(visual == NULL)
		visual = DefaultVisual(XtDisplay(w), DefaultScreen(XtDisplay(w)));
	return(visual);
}

static int 
_pixelSort(const void *_arg1, const void *_arg2)
{
	return ( ((XColor *)_arg1)->pixel - ((XColor *)_arg2)->pixel);
}

static void 
_queryColors(XCC _xcc)
{
	int i;

	_xcc->CMAP = (XColor *)malloc(sizeof(XColor) * _xcc->numColors);
	if (_xcc->CMAP)
	{
		for(i = 0; i < _xcc->numColors; i++)
			_xcc->CMAP[i].pixel = _xcc->CLUT ? 
				_xcc->CLUT[i] : _xcc->stdCmap.base_pixel + i;

		XQueryColors(_xcc->dpy, _xcc->colormap, _xcc->CMAP, 
			_xcc->numColors);
		qsort(_xcc->CMAP, _xcc->numColors, sizeof(XColor), _pixelSort);
	}
}

/*
** Find a standard colormap from a property, and make sure the visual matches
** the one we are using!
*/
static int 
_findGoodCube(XCC _xcc, Atom _atom, XStandardColormap *_matchedCube)
{
	XStandardColormap *cubes, *match;
	int status;
	int count;
	int i;

	if (!_atom)
		return 0;

	cubes = (XStandardColormap *)NULL;
	status = XGetRGBColormaps(_xcc->dpy, 
		RootWindow(_xcc->dpy, DefaultScreen(_xcc->dpy)), &cubes, &count, _atom);

	match = NULL;
	if (status)
	{
		status = 0;
		for(i = 0; (match == NULL) && (i < count); i++)
		{
			if (cubes[i].visualid == _xcc->visualInfo->visualid)
			{
				match = cubes+i;
				status = 1;
			}
		}
	}

	if (match)
		*_matchedCube = *match;

	if (cubes)
		free((char *)cubes);

	return status;
}

/*
** Find a standard cmap if it exists.
*/
static int
_lookForStdCmap(XCC _xcc, Atom _atom)
{
	int status;
	int i;

	status = 0;

	if ((status = _findGoodCube(_xcc, _atom, &_xcc->stdCmap)) == 0)
		switch(_xcc->visualInfo->MEMBER_CLASS)
		{
			case TrueColor: /* HMMM? */
			case StaticColor:
			case PseudoColor:
			case DirectColor:

				for(i = 0; i < NUMBER(ColorAtomList); i++)
					if((status = _findGoodCube(_xcc, ColorAtomList[i], 
							&_xcc->stdCmap)) != 0)
						break;

				break;

			case StaticGray:
			case GrayScale:
		
				for(i = 0; i < NUMBER(GrayAtomList); i++)
					if((status = _findGoodCube(_xcc, GrayAtomList[i], 
						&_xcc->stdCmap)) != 0)
						break;
				break;
		}

	if (!status)
		return 0;

	/*
	* This is a hack to force standard colormaps that don't set green/blue max
	* to work correctly.  For instance RGB_DEFAULT_GRAY has these set if 
	* xstdcmap is used, but not if xscm was. Plus this also makes RGB_RED_MAP 
	* (etc) work.
	*/

	if((!_xcc->stdCmap.green_max + !_xcc->stdCmap.blue_max + 
		!_xcc->stdCmap.red_max) > 1)
	{
		_xcc->mode = MODE_MY_GRAY;
		if(_xcc->stdCmap.green_max)
		{
			_xcc->stdCmap.red_max = _xcc->stdCmap.green_max;
			_xcc->stdCmap.red_mult = _xcc->stdCmap.green_mult;
		}
		else
		if(_xcc->stdCmap.blue_max)
		{
			_xcc->stdCmap.red_max = _xcc->stdCmap.blue_max;
			_xcc->stdCmap.red_mult = _xcc->stdCmap.blue_mult;
		}
		_xcc->stdCmap.green_max = _xcc->stdCmap.blue_max = 0;
		_xcc->stdCmap.green_mult = _xcc->stdCmap.blue_mult = 0;
	}
	else
		_xcc->mode = MODE_STDCMAP;

	_xcc->numColors = (_xcc->stdCmap.red_max+1) *
					 (_xcc->stdCmap.green_max+1) *
					 (_xcc->stdCmap.blue_max+1);

	_queryColors(_xcc);

	return status;
}

/*
** If we die, we go to the world of B+W
*/
static void
_initBW(XCC _xcc)
{
	XColor color;

	_XmHTMLWarning(__WFUNC__(NULL, "_initBW"), XMHTML_MSG_5);

	_xcc->mode = MODE_BW;

	color.red = color.blue = color.green = 0;
	if(!XAllocColor(_xcc->dpy, _xcc->colormap, &color))
		_xcc->blackPixel = 0;
	else
		_xcc->blackPixel = color.pixel;

	color.red = color.blue = color.green = 0xFFFF;
	if (!XAllocColor(_xcc->dpy, _xcc->colormap, &color))
		_xcc->whitePixel = _xcc->blackPixel ? 0 : 1;
	else
		_xcc->whitePixel = color.pixel;

	_xcc->numColors = 2;
}

/*
** Make our life easier and ramp our grays. Note
** that each lookup is /= 2 of the previous.
*/
static void
_initGray(XCC _xcc)
{
	XColor *clrs, *cstart;
	register int i;
	double dinc;

	_xcc->numColors = _xcc->visual->map_entries;

	_xcc->CLUT = (unsigned long *)malloc(sizeof(unsigned long) * 
		_xcc->numColors);
	cstart = (XColor *)malloc(sizeof(XColor) * _xcc->numColors);

retryGray:

	dinc = 65535.0/(_xcc->numColors-1);

	clrs = cstart;
	for(i = 0; i < _xcc->numColors; i++)
	{
		clrs->red = clrs->blue = clrs->green = dinc * i;
		if(!XAllocColor(_xcc->dpy, _xcc->colormap, clrs))
		{
			XFreeColors(_xcc->dpy, _xcc->colormap, _xcc->CLUT, i, 0);

			_xcc->numColors /= 2;

			if (_xcc->numColors > 1)
				goto retryGray;
			else
			{
				free((char *)_xcc->CLUT);
				_xcc->CLUT = NULL;
				_initBW(_xcc);
				free((char *)cstart);
				return;
			}
		}
		_xcc->CLUT[i] = clrs++->pixel;
	}

	free((char *)cstart);

	_xcc->stdCmap.colormap = _xcc->colormap;
	_xcc->stdCmap.base_pixel = 0;
	_xcc->stdCmap.red_max = _xcc->numColors-1;
	_xcc->stdCmap.green_max = 0;
	_xcc->stdCmap.blue_max = 0;
	_xcc->stdCmap.red_mult = 1;
	_xcc->stdCmap.green_mult = _xcc->stdCmap.blue_mult = 0;

	_xcc->whitePixel = WhitePixel(_xcc->dpy, DefaultScreen(_xcc->dpy));
	_xcc->blackPixel = BlackPixel(_xcc->dpy, DefaultScreen(_xcc->dpy));

	_queryColors(_xcc);

	_xcc->mode = MODE_MY_GRAY;
}

/*****
* Name: 		_initColor
* Return Type: 	void
* Description: 	initializes colors for Static and PseudoColor visuals
* In: 
*	_xcc:		XColorContext
* Returns:
*	nothing.
* Note:
*	This is a strongly modified version of the original _initColor routine.
*	It allocated a fixed list of colors and wasn't flexible enough for XmHTML.
*	This routine now initializes the colormap and queries the server for all
*	available pixel values. Actual color allocation is now postponed until
*	it needs to be allocated by XCCGetPixel. The previous routine also returned
*	a very limited number of colors: only the successfully allocated colors 
*	were counted, no interpolation was done.
*	The CLUT is no longer used for this kind of visual.
*****/
static void
_initColor(XCC _xcc)
{
	int cubeval;

	cubeval = 1;
	while((cubeval*cubeval*cubeval) < _xcc->visual->map_entries)
		cubeval++;
	cubeval--;
	_xcc->numColors = cubeval * cubeval * cubeval;

	_xcc->stdCmap.red_max = cubeval - 1;
	_xcc->stdCmap.green_max = cubeval - 1;
	_xcc->stdCmap.blue_max = cubeval - 1;
	_xcc->stdCmap.red_mult = cubeval * cubeval;
	_xcc->stdCmap.green_mult = cubeval;
	_xcc->stdCmap.blue_mult = 1;
	_xcc->stdCmap.base_pixel = 0;

	_xcc->whitePixel = WhitePixel(_xcc->dpy, DefaultScreen(_xcc->dpy));
	_xcc->blackPixel = BlackPixel(_xcc->dpy, DefaultScreen(_xcc->dpy));
	_xcc->numColors = DisplayCells(_xcc->dpy, DefaultScreen(_xcc->dpy));

	/* a clut for storing allocated pixel indices */
	_xcc->maxColors = _xcc->numColors;
	_xcc->CLUT = (unsigned long *)malloc(sizeof(unsigned long) * 
		_xcc->maxColors);
	for(cubeval = 0; cubeval < _xcc->maxColors; cubeval++)
		_xcc->CLUT[cubeval] = (unsigned long)cubeval;

	_queryColors(_xcc);

	_xcc->mode = MODE_STDCMAP;
}

/*
** Get our shifts and masks
*/
static void
_initTrueColor(XCC _xcc)
{
	register unsigned long rmask, gmask, bmask;

	_xcc->mode = MODE_TRUE;

	rmask = _xcc->masks.red = _xcc->visualInfo->red_mask;
	_xcc->shifts.red = 0;
	_xcc->bits.red = 0;
	while (!(rmask & 1))
	{
		rmask >>= 1;
		_xcc->shifts.red++;
	}
	while((rmask & 1))
	{
		rmask >>= 1;
		_xcc->bits.red++;
	}

	gmask = _xcc->masks.green = _xcc->visualInfo->green_mask;
	_xcc->shifts.green = 0;
	_xcc->bits.green = 0;
	while (!(gmask & 1))
	{
		gmask >>= 1;
		_xcc->shifts.green++;
	}
	while(gmask & 1)
	{
		gmask >>= 1;
		_xcc->bits.green++;
	}

	bmask = _xcc->masks.blue = _xcc->visualInfo->blue_mask;
	_xcc->shifts.blue = 0;
	_xcc->bits.blue = 0;
	while (!(bmask & 1))
	{
		bmask >>= 1;
		_xcc->shifts.blue++;
	}
	while(bmask & 1)
	{
		bmask >>= 1;
		_xcc->bits.blue++;
	}

	_xcc->numColors = ((_xcc->visualInfo->red_mask) | 
						(_xcc->visualInfo->green_mask) | 
						(_xcc->visualInfo->blue_mask)) + 1;
	_xcc->whitePixel = WhitePixel(_xcc->dpy, DefaultScreen(_xcc->dpy));
	_xcc->blackPixel = BlackPixel(_xcc->dpy, DefaultScreen(_xcc->dpy));
}

/*
** Cheat here! Make the direct color visual work like
** a true color! USE the CLUT!!!
*/
static void
_initDirectColor(XCC _xcc)
{
	int n, count;
	XColor *clrs, *cstart;
	unsigned long rval, bval, gval;
	unsigned long *rtable;
	unsigned long *gtable;
	unsigned long *btable;
	double dinc;

	_initTrueColor(_xcc); /* for shift stuff */

	rval = _xcc->visualInfo->red_mask >> _xcc->shifts.red;
	gval = _xcc->visualInfo->green_mask >> _xcc->shifts.green;
	bval = _xcc->visualInfo->blue_mask >> _xcc->shifts.blue;

	rtable = (unsigned long *)malloc(sizeof(unsigned long) * (rval+1));
	gtable = (unsigned long *)malloc(sizeof(unsigned long) * (gval+1)); 
	btable = (unsigned long *)malloc(sizeof(unsigned long) * (bval+1));

	_xcc->maxEntry = (rval > gval) ? rval : gval;
	_xcc->maxEntry = (_xcc->maxEntry > bval) ? _xcc->maxEntry : bval;

	cstart = (XColor *)malloc(sizeof(XColor) * (_xcc->maxEntry+1));
	_xcc->CLUT = (unsigned long *)malloc(sizeof(unsigned long) * 
		(_xcc->maxEntry+1));

retrydirect:

	for(n = 0; n <= rval; n++)
		rtable[n] = rval ? 65535.0/(double)rval * n : 0;
	for(n = 0; n <= gval; n++)
		gtable[n] = gval ? 65535.0/(double)gval * n : 0;
	for(n = 0; n <= bval; n++)
		btable[n] = bval ? 65535.0/bval * n : 0;

	_xcc->maxEntry = (rval > gval) ? rval : gval;
	_xcc->maxEntry = (_xcc->maxEntry > bval) ? _xcc->maxEntry : bval;

	count = 0;
	clrs = cstart;
	_xcc->numColors = (bval + 1) * (gval + 1) * (rval + 1);
	for(n = 0; n <= _xcc->maxEntry; n++)
	{
		dinc = (double)n/(double)_xcc->maxEntry;
		clrs->red = rtable[(int)(dinc * rval)];
		clrs->green = gtable[(int)(dinc * gval)];
		clrs->blue = btable[(int)(dinc * bval)];
		if (XAllocColor(_xcc->dpy, _xcc->colormap, clrs))
		{
			_xcc->CLUT[count++] = clrs->pixel;
			clrs++;
		}
		else
		{
			XFreeColors(_xcc->dpy, _xcc->colormap, _xcc->CLUT, count, 0);

			bval >>= 1;
			gval >>= 1;
			rval >>= 1;

			_xcc->masks.red = (_xcc->masks.red >> 1) & 
				_xcc->visualInfo->red_mask;
			_xcc->masks.green = (_xcc->masks.green >> 1) & 
				_xcc->visualInfo->green_mask;
			_xcc->masks.blue = (_xcc->masks.green >> 1) & 
				_xcc->visualInfo->blue_mask;
			
			_xcc->shifts.red++;
			_xcc->shifts.green++;
			_xcc->shifts.blue++;

			_xcc->bits.red--;
			_xcc->bits.green--;
			_xcc->bits.blue--;

			 _xcc->numColors = (bval + 1) * (gval + 1) * (rval + 1);

			if (_xcc->numColors > 1)
				goto retrydirect;
			else
			{
				free((char *)_xcc->CLUT);
				_xcc->CLUT = NULL;
				_initBW(_xcc);
				break;
			}
		}
	}
	/*
	* Update allocated color count; original numColors is maxEntry, which
	* is not necessarly the same as the really allocated number of colors.
	*/
	_xcc->numColors = count;

	free((char*)rtable);
	free((char*)gtable);
	free((char*)btable);
	free((char*)cstart);
}

XCC
XCCMonoCreate(Display *_dpy, Visual *_visual, Colormap _colormap)
{
	XCC xcc;
	XVisualInfo visInfo;
	int n;

	xcc = (XCC)malloc(sizeof(struct _XColorContext));
   
	if (xcc == NULL)
		return NULL;

	xcc->dpy = _dpy;
	xcc->visual = _visual;
	xcc->colormap = _colormap;
	xcc->CLUT = NULL;
	xcc->CMAP = NULL;
	xcc->mode = MODE_UNDEFINED;
	xcc->needToFreeColormap = 0;

	visInfo.visualid = XVisualIDFromVisual(_visual);
	xcc->visualInfo = XGetVisualInfo(_dpy, VisualIDMask, &visInfo, &n);

	_initBW(xcc);

	return xcc;
}

XCC 
XCCCreate(Widget w, Visual *_visual, Colormap _colormap)
{
	XCC xcc;
	int n;
	XVisualInfo visInfo;
	int retryCount;
	Boolean usePrivateColormap = False;
	Display *_dpy = XtDisplay(w);

	xcc = (XCC)malloc(sizeof(struct _XColorContext));

	if (xcc == NULL)
		return NULL;

	xcc->dpy = _dpy;
	xcc->visual = _visual;
	xcc->colormap = _colormap;
	xcc->CLUT = NULL;
	xcc->CMAP = NULL;
	xcc->mode = MODE_UNDEFINED;
	xcc->needToFreeColormap = 0;

	xcc->color_hash = (HashTable*)NULL;
	xcc->palette = (XColor*)NULL;
	xcc->num_palette = 0;
	xcc->fast_dither = (XCCDither*)NULL;

	visInfo.visualid = XVisualIDFromVisual(_visual);
	xcc->visualInfo = XGetVisualInfo(_dpy, VisualIDMask, &visInfo, &n);

	retryCount = 0;
	while(retryCount < 2)
	{
		/* only create a private colormap if the visual found isn't equal
		* to the default visual and we don't have a private colormap,
		* -or- if we are instructed to create a private colormap (which
		* never is the case for XmHTML).
		*/
		if(usePrivateColormap || 
			((xcc->visual != DefaultVisual(_dpy, DefaultScreen(_dpy))) &&
			_colormap == DefaultColormap(_dpy, DefaultScreen(_dpy))))
		{
			_XmHTMLWarning(__WFUNC__(w, "XCCCreate"), XMHTML_MSG_6);
			xcc->colormap = XCreateColormap(_dpy, 
					RootWindow(_dpy, DefaultScreen(_dpy)), xcc->visual, 
					AllocNone);
			xcc->needToFreeColormap = 
				(xcc->colormap != DefaultColormap(_dpy, 
					DefaultScreen(_dpy)));
			
		}
		switch(_visual->MEMBER_CLASS)
		{
			case StaticGray:
			case GrayScale:
				_XmHTMLDebug(9, ("XCC.c: XCCCreate, visual class is %s\n",
					(_visual->MEMBER_CLASS == GrayScale ? "GrayScale" : 
					"StaticGray")));
				if (xcc->visual->map_entries == 2)
					_initBW(xcc);
				else
					_initGray(xcc);
				break;

			case TrueColor: /* shifts */
				_XmHTMLDebug(9, ("XCC.c: XCCCreate, visual class is "
					"TrueColor\n")); 
				_initTrueColor(xcc);
				break;
			
			case DirectColor: /* shifts & fake CLUT */
				_XmHTMLDebug(9, ("XCC.c: XCCCreate, visual class is "
					"DirectColor\n")); 
				_initDirectColor(xcc);
				break;

			case StaticColor:
			case PseudoColor:
				_XmHTMLDebug(9, ("XCC.c: XCCCreate, visual class is %s\n",
					(_visual->MEMBER_CLASS == StaticColor ? "StaticColor" : 
					"PseudoColor")));
				_initColor(xcc);
				break;
		}

		if((xcc->mode == MODE_BW) && (xcc->visualInfo->depth > 1))
		{
			usePrivateColormap = True;
			retryCount++;
		}
		else
			break;
	}
	/* no colors allocated yet */
	xcc->numAllocated = 0;

	_XmHTMLDebug(9, ("XCC.c: XCCCreate, screen depth : %i, no of colors: %i\n",
		xcc->visualInfo->depth, xcc->numColors));

	/* check if we need to initialize a hashtable */
	if(xcc->mode == MODE_STDCMAP || xcc->mode == MODE_UNDEFINED)
	{
		xcc->color_hash = (HashTable*)malloc(sizeof(HashTable));
		_XCCHashInit(xcc->color_hash);
	}
	return(xcc);
}

static void
_initPalette(XCC _xcc)
{
	/* restore correct mode for this XCC */
	switch(_xcc->visual->MEMBER_CLASS)
	{
		case StaticGray:
		case GrayScale:
			if(_xcc->visual->map_entries == 2)
				_xcc->mode = MODE_BW;
			else
				_xcc->mode = MODE_MY_GRAY;
			break;

		case TrueColor:
		case DirectColor:
			_xcc->mode = MODE_TRUE;
			break;
			
		case StaticColor:
		case PseudoColor:
			_xcc->mode = MODE_STDCMAP;
			break;
		default:
			_xcc->mode = MODE_UNDEFINED;
			break;
	}

	/* previous palette */
	if(_xcc->num_palette)
		free(_xcc->palette);
	if(_xcc->fast_dither)
		free(_xcc->fast_dither);

	/* clear hashtable if present */
	if(_xcc->color_hash)
		_XCCHashDestroy(_xcc->color_hash);

	_xcc->palette = (XColor*)NULL;
	_xcc->num_palette = 0;
	_xcc->fast_dither = (XCCDither*)NULL;

}

#define hashpixel(r,g,b) ((unsigned short) ((r) >> 8) * 33023 +    \
    (unsigned short) ((g) >> 8) * 30013 +    \
    (unsigned short) ((b) >> 8) * 27011)

/*****
* Name: 		XCCInitDither
* Return Type: 	void
* Description: 	initialize precomputed error matrices.
* In: 
*	_xcc:		XColorContext for which we have to add a dither matrix.
* Returns:
*	nothing.
*****/
void
XCCInitDither(XCC _xcc)
{
	int rr, gg, bb, err, erg, erb;
	Boolean success = False;

	if(_xcc == NULL)
		return;

	/* now we can initialize the fast dither matrix */
	if(_xcc->fast_dither == NULL)
		_xcc->fast_dither = (XCCDither*)malloc(sizeof(XCCDither));

	/*
	* Fill it. We ignore unsuccessfull allocations, they are just mapped
	* to black instead.
	*/
	for(rr = 0; rr < 32; rr++)
	{
		for(gg = 0; gg < 32; gg++)
		{
			for(bb = 0; bb < 32; bb++)
			{
				err = (rr<<3)|(rr>>2);
				erg = (gg<<3)|(gg>>2);
				erb = (bb<<3)|(bb>>2);
				_xcc->fast_dither->fast_rgb[rr][gg][bb] =
					XCCGetIndexFromPalette(_xcc, &err, &erg, &erb, &success);
				_xcc->fast_dither->fast_err[rr][gg][bb] = err;
				_xcc->fast_dither->fast_erg[rr][gg][bb] = erg;
				_xcc->fast_dither->fast_erb[rr][gg][bb] = erb;
			}
		}
	}
}

/*****
* Name: 		XCCFreeDither
* Return Type: 	void
* Description: 	free dither matrices.
* In: 
*	_xcc:		XColorContext id;
* Returns:
*	nothing.
*****/
void
XCCFreeDither(XCC _xcc)
{
	if(_xcc == NULL)
		return;
	if(_xcc->fast_dither)
		free(_xcc->fast_dither);
	_xcc->fast_dither = (XCCDither*)NULL;

}

/*****
* Name: 		XCCAddPalette
* Return Type: 	void
* Description: 	adds or erases a palette for the given XCC.
* In: 
*	_xcc:		current XCC;
*	palette:	palette to add. Unused if num_palette is 0;
*	num_pa..:	no of colors in palette. If 0 any current palette is
*				erased and no new one is added and the mode is reset to
*				whatever it was before a palette was added.
* Returns:
*	-1 on error, 0 if palette was cleared, no of allocated colors otherwise.
* Note:
*	Palette arithmatic is always done in the range 0-255: it's only use
*	is for Pseudo/Static color visuals, which, by definition, are always
*	8bit.
*****/
int
XCCAddPalette(XCC _xcc, XColor *palette, int num_palette)
{
	int i, j, erg;
	unsigned short r, g, b;
	Pixel pixel[1];

	if(_xcc == NULL)
		return(-1);

	/* initialize this palette (will also erase previous palette as well) */
	_initPalette(_xcc);

	/* restore previous mode if we aren't adding a new palette */
	if(num_palette == 0)
	{
		/* MODE_STDCMAP uses a hashtable, so we'd better initialize one */
		if(_xcc->mode == MODE_STDCMAP || _xcc->mode == MODE_UNDEFINED)
			_XCCHashInit(_xcc->color_hash);
		return(0);
	}
	if(_xcc->color_hash == NULL)
		_xcc->color_hash = (HashTable*)malloc(sizeof(HashTable));

	/*****
	* Initialize a hashtable for this palette (we need one for allocating
	* the pixels in the palette using the current settings).
	*****/
	_XCCHashInit(_xcc->color_hash);

	/* copy incoming palette */
	_xcc->palette = (XColor*)calloc(num_palette, sizeof(XColor));

	j = 0;

	for(i = 0; i < num_palette; i++)
	{
		erg = 0;
		pixel[0] = None;
		erg = 0;

		/* try to allocate this color */
		r = palette[i].red;
		g = palette[i].green;
		b = palette[i].blue;

		XCCGetPixels(_xcc, &r, &g, &b, 1, pixel, &erg);

		/* only store if we succeed */
		if(erg)
		{
			/* store in palette and downscale to 0-255 range */
			_xcc->palette[j].red   = r >> 8;
			_xcc->palette[j].green = g >> 8;
			_xcc->palette[j].blue  = b >> 8;
			_xcc->palette[j].pixel = pixel[0];

			/* move to next slot */
			j++;
		}
	}
	/* resize to fit */
	if(j != num_palette)
		_xcc->palette = (XColor*)realloc(_xcc->palette, j*sizeof(XColor));

	/* clear the hashtable, we don't use it when dithering */
	if(_xcc->color_hash)
	{
		_XCCHashDestroy(_xcc->color_hash);
		free(_xcc->color_hash);
		_xcc->color_hash = (HashTable*)NULL;
	}

	/* store real palette size */
	_xcc->num_palette = j;

	/* switch to palette mode */
	_xcc->mode = MODE_PALETTE;

	/* sort palette */
	qsort(_xcc->palette, _xcc->num_palette, sizeof(XColor), _pixelSort);

	_xcc->fast_dither = (XCCDither*)NULL;

	return(j);
}

/*
** This doesn't currently free black/white. Hmm...
*/
void
XCCFree(XCC _xcc)
{
	if (_xcc == NULL)
		return;

	_XmHTMLDebug(9, ("XCC.c: XCCFree start\n"));

	/* these classes use a CLUT for storing allocated pixel indices */
	if(_xcc->visualInfo->MEMBER_CLASS == StaticColor ||
		_xcc->visualInfo->MEMBER_CLASS == PseudoColor)
	{
		_XmHTMLDebug(9, ("XCC.c: XCCFree, freeing %i allocated colors "
			"(%s visual)\n", _xcc->numAllocated,
			_xcc->visualInfo->MEMBER_CLASS == StaticColor ? "StaticColor" :
			"PseudoColor"));
		if(_xcc->numAllocated)
			XFreeColors(_xcc->dpy, _xcc->colormap, _xcc->CLUT,
				_xcc->numAllocated, 0);
		free((char *)_xcc->CLUT);
	}
	else if (_xcc->CLUT != NULL)
	{
		if(_xcc->numColors)
			XFreeColors(_xcc->dpy, _xcc->colormap, _xcc->CLUT,
				_xcc->numColors, 0);
		free((char *)_xcc->CLUT);
	}

	if (_xcc->CMAP != NULL)
		free((char *)_xcc->CMAP);

	if (_xcc->needToFreeColormap)
		XFreeColormap(_xcc->dpy, _xcc->colormap);

	/* free any palette that has been associated with this XCC */
	_initPalette(_xcc);

	if(_xcc->color_hash)		/* fix 09/01/97-03, kdh */
		free(_xcc->color_hash);

	/* XVisualInfo must be freed with XFree and not with XtFree */
	XFree(_xcc->visualInfo);
	free((char *)_xcc);

	_XmHTMLDebug(9, ("XCC.c: XCCFree End\n"));
}

/*****
* Name:			XCCGetPixelFromPalette
* Return Type: 	unsigned long;
* Description: 	searches the palette for a color that is closest to a
*				requested color.
* In: 
*	_xcc:		XColorContext to use;
*	_red,..:	color component values making up the requested color, inside
*				the range 0-255;
*	failed:		error indicator. Set to True when requested color could not
*				be matched, False otherwise.
* Returns:
*	Pixel value for the requested color.
* Note:
*	No hashing for palettes, it takes a *huge* amount of time & memory to
*	fill the hashtable for the dither matrix (which is 4*32*32*32 bytes...)
*****/
unsigned long
XCCGetPixelFromPalette(XCC _xcc, unsigned short *_red,
	unsigned short *_green, unsigned short *_blue, Boolean *failed)
{
	unsigned long pixel = None;
	int dif, dr, dg, db, j = -1;
	int mindif=0x7fffffff;
	int err = 0, erg = 0, erb = 0;
	register int i;

	*failed = False;

	for(i = 0; i < _xcc->num_palette; i++)
	{
		dr = *_red   - _xcc->palette[i].red;
		dg = *_green - _xcc->palette[i].green;
		db = *_blue  - _xcc->palette[i].blue;
		if((dif = dr*dr + dg*dg + db*db) < mindif)
		{
			mindif = dif;
			j = i;
			pixel = _xcc->palette[i].pixel;
			err = dr;
			erg = dg;
			erb = db;
						
			if(mindif == 0)
				break;
		}
	}
	/* we failed to map onto a color */
	if(j == -1)
		*failed = True;
	else
	{
		*_red   = (unsigned short)(err < 0 ? -err : err);
		*_green = (unsigned short)(erg < 0 ? -erg : erg);
		*_blue  = (unsigned short)(erb < 0 ? -erb : erb);
	}
	return(pixel);
}

/*****
* Name:			XCCGetIndexFromPalette
* Return Type: 	Byte
* Description: 	same as XCCGetPixelFromPalette, only this one returns an
*				index into the palette instead of an actual pixel value.
*				(which actually happens to be the actual pixel value...)
*				This routine is intended to be used by dithering routines.
* In: 
*	_xcc:		XColorContext to use;
*	_red,..:	color component values inside the range 0-255;
*	failed:		error indicator. Set to True when requested color could not
*				be matched, False otherwise.
* Returns:
*	palette index for the requested color;
*****/
Byte
XCCGetIndexFromPalette(XCC _xcc, int *_red, int *_green, int *_blue,
	Boolean *failed)
{
	int dif, dr, dg, db, j = -1;
	int mindif=0x7fffffff;
	int err = 0, erg = 0, erb = 0;
	register int i;

	*failed = False;

	for(i = 0; i < _xcc->num_palette; i++)
	{
		dr = *_red   - _xcc->palette[i].red;
		dg = *_green - _xcc->palette[i].green;
		db = *_blue  - _xcc->palette[i].blue;
		if((dif = dr*dr + dg*dg + db*db) < mindif)
		{
			mindif = dif;
			j = i;
			err = dr;	/* save error fractions */
			erg = dg;
			erb = db;
						
			if(mindif == 0)
				break;
		}
	}
	/* we failed to map onto a color */
	if(j == -1)
	{
		*failed = True;
		j = 0;
	}
	else
	{
		/* return error fractions */
		*_red   = err;
		*_green = erg;
		*_blue  = erb;
	}
	return((Byte)j);
}

/*****
* Name:			XCCGetPixel
* Return Type: 	unsigned long
* Description: 	allocates a color, returning it's pixel value.
* In: 
*	_xcc:		XColorContext id;
*	_red,..:	color component values in the range 0-2^16
*	*failed:	error indicator, filled upon return.
* Returns:
*	a pixel id for the requested color.
*****/
unsigned long
XCCGetPixel(XCC _xcc, unsigned short _red, unsigned short _green, 
	unsigned short _blue, Boolean *failed)
{
	*failed = False;

	_XmHTMLDebug(9, ("XCC.c: XCCGetPixel, red = %hu, green = %hu, "
		"blue = %hu\n", _red, _green, _blue));

	switch(_xcc->mode)
	{
		case MODE_BW:
		{
			double value;

			value = (double)_red/65535.0 * 0.3 + 
					(double)_green/65535.0 * 0.59 + 
					(double)_blue/65535.0 * 0.11;
			if (value > 0.5)
				return _xcc->whitePixel;
			return _xcc->blackPixel;
		}
		case MODE_MY_GRAY:
		{
			unsigned long ired, igreen, iblue;

			_red = _red * 0.3 + _green * 0.59 + _blue * 0.1;
			_green = 0;
			_blue = 0;

			if((ired = _red * (_xcc->stdCmap.red_max + 1) / 0xFFFF)
				> _xcc->stdCmap.red_max)
				ired = _xcc->stdCmap.red_max;

			ired *= _xcc->stdCmap.red_mult;

			if((igreen = _green * (_xcc->stdCmap.green_max + 1) / 0xFFFF)
				> _xcc->stdCmap.green_max)
				igreen = _xcc->stdCmap.green_max;

			igreen *= _xcc->stdCmap.green_mult;

			if((iblue = _blue * (_xcc->stdCmap.blue_max + 1) / 0xFFFF)
				> _xcc->stdCmap.blue_max)
				iblue = _xcc->stdCmap.blue_max;

			iblue *= _xcc->stdCmap.blue_mult;

			if (_xcc->CLUT != NULL)
				return(_xcc->CLUT[_xcc->stdCmap.base_pixel +
					ired + igreen + iblue]);
			return(_xcc->stdCmap.base_pixel + ired + igreen + iblue);

		}
		case MODE_TRUE:
		{
			unsigned long ired, igreen, iblue;

			if (_xcc->CLUT == NULL)
			{
				_red >>= 16 - _xcc->bits.red;
				_green >>= 16 - _xcc->bits.green;
				_blue >>= 16 - _xcc->bits.blue;

				ired   = (_red << _xcc->shifts.red)     & _xcc->masks.red;
				igreen = (_green << _xcc->shifts.green) & _xcc->masks.green;
				iblue  = (_blue << _xcc->shifts.blue)   & _xcc->masks.blue;
				return(ired | igreen | iblue);
			}
			ired = _xcc->CLUT[(int)((_red * _xcc->maxEntry)/65535)] &
				_xcc->masks.red;
			igreen = _xcc->CLUT[(int)((_green * _xcc->maxEntry)/65535)] & 
				_xcc->masks.green;
			iblue = _xcc->CLUT[(int)((_blue * _xcc->maxEntry)/65535)] & 
				_xcc->masks.blue;
			return(ired | igreen | iblue);
		}
		case MODE_PALETTE:
		{
			unsigned short r, g, b;
			/* palette stuff is always done in 0-255 range */
			r = _red >> 8;
			g = _green >> 8;
			b = _blue >> 8;
			return(XCCGetPixelFromPalette(_xcc, (unsigned short*)(&r),
				(unsigned short*)(&g), (unsigned short*)(&b), failed));
		}

		case MODE_STDCMAP:
		default:
		{
			unsigned long key, pixel = 0;
			HashEntry *entry;
			Boolean gotit = False;

			/* try hashtable */
			key = hashpixel(_red,_green,_blue);

			_XmHTMLFullDebug(9, ("XCC.c: XCCGetPixel, hashkey = %lu\n", key));

#ifdef DEBUG
			_xcc->color_hash->requests++;
#endif
			entry = _xcc->color_hash->table[key % _xcc->color_hash->size];
			while(entry != NULL)
			{
				if(entry->key == key)
				{
					pixel = entry->data;
					gotit = True;
#ifdef DEBUG
					_xcc->color_hash->hits++;
#endif
					break;
				}
				entry = entry->next;
			}
			if(!gotit)
			{
				XColor color;
#ifdef DEBUG
				_xcc->color_hash->misses++;
#endif
				_XmHTMLFullDebug(9, ("XCC.c: XCCGetPixel, not found in "
					"hashtable, trying to allocate it.\n"));
				color.red   = _red;
				color.green = _green;
				color.blue  = _blue;
				color.pixel = 0;
				color.flags = DoRed|DoGreen|DoBlue;
				if(!XAllocColor(_xcc->dpy, _xcc->colormap, &color))
					*failed = True;
				else
				{
					_XmHTMLFullDebug(9, ("XCC.c: XCCGetPixel, got it, storing "
						"hashtable.\n"));
					/*
					* I can't figure this out entirely, but it *is* possible
					* that XAllocColor succeeds, even if the number of
					* allocations we've made exceeds the number of available
					* colors in the current colormap. And therefore it
					* might be necessary for us to resize the CLUT.
					*/
					if(_xcc->numAllocated == _xcc->maxColors)
					{
						_xcc->maxColors *=2;
						_XmHTMLDebug(9, ("XCC.c: XCCGetPixel, resizing CLUT "
							"to %i entries\n", _xcc->maxColors));
						_xcc->CLUT = (unsigned long*)realloc(_xcc->CLUT,
							_xcc->maxColors * sizeof(unsigned long));
					}

					_XCCHashPut(_xcc->color_hash, key, color.pixel);
					_xcc->CLUT[_xcc->numAllocated] = color.pixel;
					_xcc->numAllocated++;
					return(color.pixel);
				}
			}
#ifdef DEBUG
			else
				_XmHTMLFullDebug(9, ("XCC.c: XCCGetPixel, found in hashtable "
					"as being pixel %lu.\n", pixel));
#endif
			return(pixel);
		}
	}
}

/*****
* Name: 
* Return Type: 
* Description: 
* In: 
*	_xcc:		XColorContext
*	reds:		array of red values
*	greens:		array of green values
*	blues:		array of blue values
*	ncolors:	no of colors to allocate
*	colors:		array of allocated colors, filled upon return
*	nallocated:	no of really allocated colors, filled upon return
* Returns:
*	Nothing, but colors will contain the pixel values for every requested
*	color (allocated, matched or substituted).
* Note: all color values are within the range 0-255 (inclusive)!
*****/
void
XCCGetPixels(XCC _xcc, unsigned short *reds, unsigned short *greens,
	unsigned short *blues, int ncolors, unsigned long *colors, int *nallocated)
{
	register int i, k, idx;
	int cmapsize, ncols = 0, nopen = 0, counter = 0;
	Boolean bad_alloc = False;
	int failed[XmHTML_MAX_IMAGE_COLORS], allocated[XmHTML_MAX_IMAGE_COLORS];
	XColor defs[XmHTML_MAX_IMAGE_COLORS], cmap[XmHTML_MAX_IMAGE_COLORS];

#ifdef DEBUG
	int exact_col = 0, subst_col = 0, close_col = 0, black_col = 0;
#endif

	memset(defs, 0, XmHTML_MAX_IMAGE_COLORS*sizeof(XColor));
	/* fix 08/29/97-01, rr */
	memset(failed, 0, XmHTML_MAX_IMAGE_COLORS*sizeof(int));
	memset(allocated, 0, XmHTML_MAX_IMAGE_COLORS*sizeof(int));

	/* will only have a value if used by the progressive image loader */
	ncols = *nallocated;

	*nallocated = 0;

	/* First allocate all pixels */
	for(i = 0 ; i < ncolors; i++)
	{
		/*****
		* colors[i] is only zero if the pixel at that location hasn't
		* been allocated yet. This is a sanity check required for proper
		* color allocation by the progressive image loader.
		*****/
		if(colors[i] == 0)
		{
			defs[i].red   = reds[i];
			defs[i].green = greens[i];
			defs[i].blue  = blues[i];

			colors[i] = XCCGetPixel(_xcc, defs[i].red, defs[i].green,
				defs[i].blue, &bad_alloc);

			/* succesfully allocated, store it */
			if(!bad_alloc)
			{
				defs[i].pixel = colors[i];
				allocated[ncols++] = (int)colors[i];
			}
			else
				failed[nopen++] = i;
		}
	}
	*nallocated = ncols;

	/* all colors available, all done */
	if(ncols == ncolors || nopen == 0)
	{
		_XmHTMLDebug(9, ("XCC.c: XCCGetPixels, got all %i colors\n", ncolors));
		_XmHTMLDebug(9, ("       (%i colors allocated so far)\n",
			_xcc->numAllocated));
		return;
	}

	/*****
	* The fun part. We now try to allocate the colors we couldn't allocate
	* directly. The first step will map a color onto it's nearest color
	* that has been allocated (either by us or someone else). If any colors
	* remain unallocated, we map these onto the colors that we have allocated
	* ourselves.
	*****/

	/* read up to XmHTML_MAX_IMAGE_COLORS colors of the current colormap */
	cmapsize = (_xcc->numColors < XmHTML_MAX_IMAGE_COLORS ?
		_xcc->numColors : XmHTML_MAX_IMAGE_COLORS);

	/* see if the colormap has any colors to read */
	if(cmapsize < 1)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "XCCGetPixels"), XMHTML_MSG_7);
		return;
	}
#ifdef DEBUG
	exact_col = ncols;
#endif

	/* initialise pixels */
	memset(cmap, 0, XmHTML_MAX_IMAGE_COLORS*sizeof(XColor));
	for(i = 0; i < cmapsize; i++)
		cmap[i].pixel = (Pixel)i;

	/* read the colormap */
	XQueryColors(_xcc->dpy, _xcc->colormap, cmap, cmapsize);

	/* speedup: downscale here instead of in the matching code */
	for(i = 0; i < cmapsize; i++)
	{
		cmap[i].red   >>= 8;
		cmap[i].green >>= 8;
		cmap[i].blue  >>= 8;
	}

	/* get a close match for any unallocated colors */
	counter = nopen;
	nopen = 0;
	idx = 0;
	do
	{
		int d, j, mdist, close, ri, gi, bi;
		register int rd, gd, bd;
		unsigned short red, green, blue;

		i = failed[idx];

		mdist = 0x1ffffff;
		close = -1;

		/*****
		* Store these vals. Small performance increase as this skips three
		* indexing & shift operations in the loop code.
		*****/
		ri = reds[i] >> 8;
		gi = greens[i] >> 8;
		bi = blues[i] >> 8;

		/***** 
		* walk all colors in the colormap and see which one is the 
		* closest. Uses plain least squares.
		*****/
		for(j = 0; j < cmapsize ; j++)
		{
			rd = ri - cmap[j].red;
			gd = gi - cmap[j].green;
			bd = bi - cmap[j].blue;

			if((d = (rd*rd) + (gd*gd) + (bd*bd)) < mdist)
			{
				close = j;
				mdist = d;
				if(mdist == 0)
					break;
			}
		}
		if(close != -1)
		{
			/* upscale & allocate */
			red = cmap[close].red << 8;
			green = cmap[close].green << 8;
			blue = cmap[close].blue << 8;
			colors[i] = XCCGetPixel(_xcc, red, green, blue, &bad_alloc);

			/* store */
			if(!bad_alloc)
			{
				/* store *upscaled* values */
				defs[i].red   = red;
				defs[i].green = green;
				defs[i].blue  = blue;
				defs[i].flags = cmap[i].flags;
				defs[i].pixel = colors[i];
				allocated[ncols++] = (int)colors[i];
#ifdef DEBUG
				close_col++;
#endif
			}
			else
				failed[nopen++] = i;
		}
		else
			failed[nopen++] = i;
		/* deal with in next stage if allocation failed */
	}
	while(++idx < counter);

	*nallocated = ncols;

	/*****
	* This is the maximum no of allocated colors. Any colors that remain
	* unallocated will be mapped to either one of these colors or black. 
	*****/
	if(ncols == ncolors || nopen == 0)
	{
		_XmHTMLDebug(9, ("XCC.c: XCCGetPixels, got %i colors, %i exact and "
			"%i close\n", ncolors, exact_col, close_col));
		_XmHTMLDebug(9, ("       (%i colors allocated so far)\n",
			_xcc->numAllocated));
		return;
	}

	/* now map any remaining unallocated pixels into the colors we did get */
	idx = 0;
	do
	{
		int d, mdist, close, ri, gi, bi;
		register int j, rd, gd, bd;

		i = failed[idx];

		mdist = 0x1ffffff;
		close = -1;

		/* downscale & store */
		ri = reds[i] >> 8;
		gi = greens[i] >> 8;
		bi = blues[i] >> 8;

		/* search allocated colors */
		for(j = 0; j < ncols ; j++)
		{
			k = allocated[j];

			/* defs RGB values are 16bit so downscale for proper compare */
			rd = ri - (defs[k].red >> 8);
			gd = gi - (defs[k].green >> 8);
			bd = bi - (defs[k].blue >> 8);
			if((d = (rd*rd) + (gd*gd) + (bd*bd)) < mdist)
			{
				close = k;
				mdist = d;
				if(mdist == 0)
					break;
			}
		}
		if(close < 0)
		{
			/* too bad, map to black */
			defs[i].pixel = _xcc->blackPixel;
			defs[i].red = defs[i].green = defs[i].blue = 0;
#ifdef DEBUG
			black_col++;
#endif
		}
		else
		{
			(void)memcpy((char*)&defs[i], (char*)&defs[close], sizeof(XColor));
#ifdef DEBUG
			subst_col++;
#endif
		}
		colors[i] = defs[i].pixel;
	}
	while(++idx < nopen);

	_XmHTMLDebug(9, ("XCC.c: XCCGetPixels, got %i colors, %i exact, %i close, "
		"%i substituted,\n        and %i to black ", ncolors,
		exact_col, close_col, subst_col, black_col));
	_XmHTMLDebug(9, ("(%i colors allocated so far).\n",
		_xcc->numAllocated));
}

/*****
* Name: 		XCCGetPixelsIncremental
* Return Type: 	void
* Description:  XCCGetPixels using an array of previously allocated pixels
*				Also see the comments in XCCGetPixels.
* In: 
*	*used:		array of previously allocated pixels
* Returns:
*	nothing.
*****/
void
XCCGetPixelsIncremental(XCC _xcc, unsigned short *reds, unsigned short *greens,
	unsigned short *blues, int ncolors, Boolean *used,
	unsigned long *colors, int *nallocated)
{
	register int i, k, idx;
	int cmapsize, ncols = 0, nopen = 0, counter = 0;
	Boolean bad_alloc = False;
	int failed[XmHTML_MAX_IMAGE_COLORS], allocated[XmHTML_MAX_IMAGE_COLORS];
	XColor defs[XmHTML_MAX_IMAGE_COLORS], cmap[XmHTML_MAX_IMAGE_COLORS];

#ifdef DEBUG
	int exact_col = 0, subst_col = 0, close_col = 0, black_col = 0;
#endif

	memset(defs, 0, XmHTML_MAX_IMAGE_COLORS*sizeof(XColor));
	memset(failed, 0, XmHTML_MAX_IMAGE_COLORS*sizeof(int));
	memset(allocated, 0, XmHTML_MAX_IMAGE_COLORS*sizeof(int));

	/* will only have a value if used by the progressive image loader */
	ncols = *nallocated;

	*nallocated = 0;

	/* First allocate all pixels */
	for(i = 0 ; i < ncolors; i++)
	{
		/*****
		* used[i] is only -1 if the pixel at that location hasn't
		* been allocated yet. This is a sanity check required for proper
		* color allocation by the progressive image loader.
		* When colors[i] == 0 it indicates the slot is available for
		* allocation.
		*****/
		if(used[i] != False)
		{
			if(colors[i] == 0)
			{
				defs[i].red   = reds[i];
				defs[i].green = greens[i];
				defs[i].blue  = blues[i];

				colors[i] = XCCGetPixel(_xcc, reds[i], greens[i], blues[i],
					&bad_alloc);

				/* succesfully allocated, store it */
				if(!bad_alloc)
				{
					defs[i].pixel = colors[i];
					allocated[ncols++] = (int)colors[i];
				}
				else
					failed[nopen++] = i;
			}
#ifdef DEBUG
			else
				_XmHTMLDebug(9, ("XCC.c: XCCGetPixelsIncremental, "
					"Pixel at slot %i already allocated, skipping\n", i));
#endif
		}
	}
	*nallocated = ncols;

	if(ncols == ncolors || nopen == 0)
	{
		_XmHTMLDebug(9, ("XCC.c: XCCGetPixels, got all %i colors\n", ncolors));
		_XmHTMLDebug(9, ("       (%i colors allocated so far)\n",
			_xcc->numAllocated));
		return;
	}
	cmapsize = (_xcc->numColors < XmHTML_MAX_IMAGE_COLORS ?
		_xcc->numColors : XmHTML_MAX_IMAGE_COLORS);

	if(cmapsize < 0)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "XCCGetPixelsIncremental"),
			XMHTML_MSG_7);
		return;
	}
#ifdef DEBUG
	exact_col = ncols;
#endif
	/* initialise pixels */
	for(i = 0; i < cmapsize; i++)
	{
		cmap[i].pixel = (Pixel)i;
		cmap[i].red = cmap[i].green = cmap[i].blue = 0;
	}
	/* read & downscale */
	XQueryColors(_xcc->dpy, _xcc->colormap, cmap, cmapsize);
	for(i = 0; i < cmapsize; i++)
	{
		cmap[i].red   >>= 8;
		cmap[i].green >>= 8;
		cmap[i].blue  >>= 8;
	}

	/* now match any unallocated colors */
	counter = nopen;
	nopen = 0;
	idx = 0;
	do
	{
		int d, j, mdist, close, ri, gi, bi;
		register int rd, gd, bd;

		i = failed[idx];

		mdist = 0x1ffffff;
		close = -1;

		/* store */
		ri = reds[i] >> 8;
		gi = greens[i] >> 8;
		bi = blues[i] >> 8;

		for(j = 0; j < cmapsize && mdist != 0; j++)
		{
			rd = ri - cmap[j].red;
			gd = gi - cmap[j].green;
			bd = bi - cmap[j].blue;

			if((d = (rd*rd) + (gd*gd) + (bd*bd)) < mdist)
			{
				close = j;
				mdist = d;
			}
		}
		if(close != -1)
		{
			rd = cmap[close].red << 8;
			gd = cmap[close].green << 8;
			bd = cmap[close].blue << 8;

			/* allocate */
			colors[i] = XCCGetPixel(_xcc, (unsigned short)rd,
				(unsigned short)gd, (unsigned short)bd, &bad_alloc);

			/* store */
			if(!bad_alloc)
			{
				(void)memcpy((char*)&defs[i], (char*)&cmap[close],
					sizeof(XColor));
				defs[i].pixel      = colors[i];
				allocated[ncols++] = (int)colors[i];
#ifdef DEBUG
				close_col++;
#endif
			}
			else
				failed[nopen++] = i;
		}
		else
			failed[nopen++] = i;
		/* deal with in next stage if allocation failed */
	}
	while(++idx < counter);

	*nallocated = ncols;

	if(ncols == ncolors || nopen == 0)
	{
		_XmHTMLDebug(9, ("XCC.c: XCCGetPixels, got %i colors, %i exact and "
			"%i close\n", ncolors, exact_col, close_col));
		_XmHTMLDebug(9, ("       (%i colors allocated so far)\n",
			_xcc->numAllocated));
		return;
	}

	/* map remaining unallocated pixels into colors we did get */
	idx = 0;
	do
	{
		int d, mdist, close, ri, gi, bi;
		register int j, rd, gd, bd;

		i = failed[idx];

		mdist = 0x1ffffff;
		close = -1;

		ri = reds[i] >> 8;
		gi = greens[i] >> 8;
		bi = blues[i] >> 8;

		/* search allocated colors */
		for(j = 0; j < ncols && mdist != 0; j++)
		{
			k = allocated[j];

			/* downscale */
			rd = ri - defs[k].red;
			gd = gi - defs[k].green;
			bd = bi - defs[k].blue;
			if((d = (rd*rd) + (gd*gd) + (bd*bd)) < mdist)
			{
				close = k;
				mdist = d;
			}
		}
		if(close < 0)
		{
			/* too bad, map to black */
			defs[i].pixel = _xcc->blackPixel;
			defs[i].red = defs[i].green = defs[i].blue = 0;
#ifdef DEBUG
			black_col++;
#endif
		}
		else
		{
			(void)memcpy((char*)&defs[i], (char*)&defs[close], sizeof(XColor));
#ifdef DEBUG
			subst_col++;
#endif
		}
		colors[i] = defs[i].pixel;
	}
	while(++idx < nopen);

	_XmHTMLDebug(9, ("XCC.c: XCCGetPixels, got %i colors, %i exact, %i close, "
		"%i substituted,\n        and %i to black ", ncolors,
		exact_col, close_col, subst_col, black_col));
	_XmHTMLDebug(9, ("(%i colors allocated so far).\n",
		_xcc->numAllocated));
}

int
XCCGetNumColors(XCC _xcc)
{
	return _xcc->numColors;
}

Colormap
XCCGetColormap(XCC _xcc)
{
	if (_xcc)
		return _xcc->colormap;
	else
		return (Colormap)0;
}

Visual*
XCCGetVisual(XCC _xcc)
{
	if (_xcc)
		return _xcc->visual;
	else
		return (Visual *)NULL;
}

XVisualInfo*
XCCGetVisualInfo(XCC _xcc)
{
	if (_xcc)
		return _xcc->visualInfo;
	else
		return (XVisualInfo *)NULL;
}

int
XCCGetDepth(XCC _xcc)
{
	if(_xcc)
		return _xcc->visualInfo->depth;
	else
		return 0;
}

int
XCCGetClass(XCC _xcc)
{
	if(_xcc)
		return _xcc->visualInfo->MEMBER_CLASS;
	else
		return 0;
}

int
XCCQueryColors(XCC _xcc, XColor *_colors, int _numColors)
{
	int i;
	XColor *tc;

	switch(_xcc->mode)
	{
		case MODE_BW:
			for(i = 0, tc = _colors; i < _numColors; i++, tc++)
			{
				if (tc->pixel == _xcc->whitePixel)
					tc->red = tc->green = tc->blue = 65535;
				else
					tc->red = tc->green = tc->blue = 0;
			}
			break;

		case MODE_TRUE:
			if (_xcc->CLUT == NULL)
			{
				for(i = 0, tc = _colors; i < _numColors; i++, tc++)
				{
					tc->red = ((tc->pixel & _xcc->masks.red) * 65535)/
								_xcc->masks.red;
					tc->green = ((tc->pixel & _xcc->masks.green) * 65535)/
								_xcc->masks.green;
					tc->blue = ((tc->pixel & _xcc->masks.blue) * 65535)/
								_xcc->masks.blue;
				}
			}
			else
			{
				XQueryColors(_xcc->dpy, _xcc->colormap, _colors, _numColors);
				return(1);	/* fix 04/23/97-01, ro */
			}
			break;

		case MODE_STDCMAP:
		default:
			if(_xcc->CMAP == NULL)
			{
				XQueryColors(_xcc->dpy, _xcc->colormap, _colors, _numColors);
				return(1);	/* fix 04/23/97-01, ro */
			}
			else
			{
				register int first, last, half;
				unsigned long halfPixel;

				for(i = 0, tc = _colors; i < _numColors; i++)
				{
					first = 0;
					last = _xcc->numColors-1;

					while(first <= last)
					{
						half = (first+last)/2;
						halfPixel = _xcc->CMAP[half].pixel;
						if(tc->pixel == halfPixel)
						{
							tc->red = _xcc->CMAP[half].red;
							tc->green = _xcc->CMAP[half].green;
							tc->blue = _xcc->CMAP[half].blue;
							first = last+1; /* fake break */
						}
						else
						{
							if(tc->pixel > halfPixel)
								first = half+1;
							else
								last = half-1;
						}
					}
				}
				return 1;
			}
			break;
	}
	return 1;
}

int
XCCQueryColor(XCC _xcc, XColor *_color)
{
	return XCCQueryColors(_xcc, _color, 1);
}

int
XCCGetColor(XCC _xcc, unsigned long pixel_val,
	unsigned short *red, unsigned short *green, unsigned short *blue)
{
	XColor color;

	color.pixel = pixel_val;

	/* resolve color */
	if(!XCCQueryColor(_xcc, &color))
		return(0);

	/* update return values */
	*red   = color.red;
	*green = color.green;
	*blue  = color.blue;

	return(1);
}


Display*
XCCGetDisplay(XCC _xcc)
{
	if (_xcc)
		return _xcc->dpy;
	else
		return (Display *)NULL;
}

/*****
* hashing routines
*****/

/*****
* Name: 		table_idestroy
* Return Type: 	void
* Description: 	frees the table of a given hashtable. Only used when table
*				is being rebuild.
* In: 
*	table:		table to be destroyed;
* Returns:
*	nothing.
*****/
static void
table_idestroy(HashTable *table)
{
	HashEntry *entry, *next;
	int i;

	for (i=0; i<table->size; i++)
	{
		entry=table->table[i];
		while (entry)
		{
			next = entry->next;
			entry = next;
		}
	}
	free(table->table);
}

/*****
* Name: 		delete_fromilist
* Return Type: 	HashEntry
* Description: 	deletes a given entry from the given hashtable.
* In: 
*	table:		table from which an entry should be deleted.
*	entry:		entry to be deleted;
*	key:		entry identifier
* Returns:
*	entry following the deleted entry. This can be non-null if a hashvalue
*	contains multiple keys.
*****/
static HashEntry *
delete_fromilist(HashTable *table, HashEntry *entry, unsigned long key)
{
	HashEntry *next;

	if(entry==NULL)
		return NULL;
	if(entry->key==key)
	{
		if(table->last == entry)
			table->last = entry->pptr;
		if(entry->nptr)
			entry->nptr->pptr = entry->pptr;
		if(entry->pptr)
			entry->pptr->nptr = entry->nptr;
		next = entry->next;
		free(entry);
		return next;
	}
	entry->next = delete_fromilist(table, entry->next, key);
	return entry;
}

/*****
* Name: 		rebuild_itable
* Return Type: 	void
* Description: 	enlarges & rebuilds the given hashtable. Used when the
*				size of the current hashtable is becoming to small to store
*				new info efficiently.
* In: 
*	table:		table to rebuild
* Returns:
*	nothing.
*****/
static void
rebuild_itable(HashTable *table)
{
	HashTable newtable;
	HashEntry *entry;
	int i;

	newtable.last = NULL;
	newtable.elements = 0;
	newtable.size = table->size*2;
	newtable.table = (HashEntry**)malloc(newtable.size * sizeof(HashEntry*));
	memset(newtable.table, 0, newtable.size * sizeof(HashEntry*));
	for (i=0; i<table->size; i++)
	{
		entry = table->table[i];
		while (entry)
		{
			_XCCHashPut(&newtable, entry->key, entry->data);
			entry=entry->next;
		}
	}
	table_idestroy(table);
	table->elements = newtable.elements;
	table->size = newtable.size;
	table->table = newtable.table;
}

/*****
* (Public) hash routines.
*****/

/*************
****** Hashing 
*************/

/*****
* Name: 		_XCCHashInit
* Return Type: 	HashTable
* Description: 	Initializes a hashtable with a initial
*				size = XmHTML_COLORHASH_SIZE 
*				The table must already be allocated.
* In: 
*	table:		hashtable to be initialized;
* Returns:
*	initialized table.
*****/
HashTable *
_XCCHashInit(HashTable *table)
{
	table->elements = 0;
	table->size = XmHTML_COLORHASH_SIZE;
	table->table =(HashEntry**)malloc(XmHTML_COLORHASH_SIZE*sizeof(HashEntry*));
	table->last = NULL;
	memset(table->table, 0, XmHTML_COLORHASH_SIZE * sizeof(HashEntry*));

#ifdef DEBUG
	table->requests = table->hits = table->misses = 0;
	table->puts = table->collisions = 0;
#endif

	return table;
}

/*****
* Name: 		_XCCHashPut
* Return Type: 	void
* Description: 	puts a new entry in the hash table
* In: 
*	key:		handle to data to be stored;
*	data:		data to be stored;
* Returns:
*	nothing.
*****/
void
_XCCHashPut(HashTable *table, unsigned long key, unsigned long data)
{
	unsigned long hkey;
	HashEntry *nentry;
    
#ifdef DEBUG
	table->puts++;
#endif
	nentry = (HashEntry*)malloc(sizeof(HashEntry));
#ifdef DEBUG
	nentry->ncoll = 0;
#endif

	nentry->key = key;
	nentry->data = data;
	hkey = key % table->size;
	/* Aaie, collided */
	if (table->table[hkey]!=NULL)
	{
#ifdef DEBUG
		_XmHTMLDebug(9, ("XCC.c: _XCCHashPut, collision on key %lu, slot "
			"%lu, (pixel = %lu)\n", key, hkey, data));
		nentry->ncoll++;
#endif
		nentry->next = table->table[hkey];
		table->table[hkey] = nentry;
#ifdef DEBUG
		table->collisions++;
#endif
    }
	else
	{
		nentry->next = NULL;
		table->table[hkey] = nentry;
    }
	table->elements++;
    
	nentry->nptr = NULL;
	nentry->pptr = table->last;
	if(table->last)
		table->last->nptr = nentry;
	table->last = nentry;
    
	if(table->elements>(table->size*3)/2)
	{
		_XmHTMLDebug(9, ("XCC.c: _XCCHashPut, rebuilding table, "
			"%i elements stored using %i available slots.\n", table->elements,
			table->size));
		rebuild_itable(table);
    }
}

/*****
* Name: 		_XCCHashGet
* Return Type: 	Boolean
* Description: 	retrieves a hash entry.
* In: 
*	key:		id of entry to retrieve;
*	*data:		object in which to store data reference;
* Returns:
*	True when entry was found, False if not.
*****/
Boolean
_XCCHashGet(HashTable *table, unsigned long key, unsigned long *data)
{
	unsigned long hkey;
	HashEntry *entry;
#ifdef DEBUG
	table->requests++;
#endif
	hkey = key % table->size;
	entry = table->table[hkey];
	while (entry!=NULL)
	{
		if(entry->key==key)
		{
			*data=entry->data;
#ifdef DEBUG
			table->hits++;
#endif
			return(True);
		}
		entry = entry->next;
	}
#ifdef DEBUG
	table->misses++;
#endif
	return(False);
}

/*****
* Name: 		_XCCHashDelete
* Return Type: 	void
* Description: 	deletes the hash entry for the given key.
* In: 
*	table:		hashtable from which to delete an entry;
*	key:		id of entry to be deleted.
* Returns:
*	nothing.
*****/
void
_XCCHashDelete(HashTable *table, unsigned long key)
{
    unsigned long hkey;
    
    hkey = key % table->size;
    table->table[hkey] = delete_fromilist(table, table->table[hkey], key);
    table->elements--;
}

/*****
* Name: 		_XCCHashDestroy
* Return Type: 	void
* Description: 	completely destroys the given hashtable contents. Table
*				and contents are not destroyed.
* In: 
*	table:		table to be destroyed;
* Returns:
*	nothing.
*****/
void
_XCCHashDestroy(HashTable *table)
{
	int i;

	_XmHTMLDebug(9, ("Hash statistics:\n"
		"%i elements hashed, %i collided\n"
		"%i requests, %i hits and %i misses.\n",
		table->puts, table->collisions, table->requests, table->hits,
		table->misses));

	for (i=0; i<table->size; i++)
	{
		/* delete all entries */
		if(table->table[i]!=NULL)
			while((table->table[i] = delete_fromilist(table, table->table[i],
				table->table[i]->key)) != NULL);
	}
	/* delete table */
	free(table->table);

	/* sanity */
	table->table = NULL;

#ifdef DEBUG
	table->requests = table->hits = table->misses = 0;
	table->puts = table->collisions = 0;
#endif
}

