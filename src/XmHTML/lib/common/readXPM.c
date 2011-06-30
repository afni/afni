#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* readXPM.c : XmHTML XPM image loading routines
*
* This file Version	$Revision$
*
* Creation date:		Wed Feb 19 03:19:23 GMT+0100 1997
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
*
* Copyright (C) 1994-1997 by Ripley Software Development 
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
* Revision 1.1  2011/06/30 16:10:38  rwcox
* Cadd
*
* Revision 1.11  1998/04/27 07:03:20  newt
* tka stuff
*
* Revision 1.10  1998/04/04 06:28:36  newt
* XmHTML Beta 1.1.3
*
* Revision 1.9  1997/10/23 00:25:26  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.8  1997/08/30 01:34:07  newt
* Extended the check for transparent pixel names to include mask and background.
*
* Revision 1.7  1997/08/01 13:12:43  newt
* Much more readable: eliminated duplicate code.
*
* Revision 1.6  1997/05/28 01:56:09  newt
* Image depth support.
*
* Revision 1.5  1997/04/29 14:31:15  newt
* Header files modifications.
*
* Revision 1.4  1997/03/20 08:16:03  newt
* Transparency color bugfix: pixel is now index instead of background pixel
*
* Revision 1.3  1997/03/11 19:59:03  newt
* ImageBuffer changes
*
* Revision 1.2  1997/03/04 18:49:55  newt
* Removed dependency on work_area field of XmHTMLWidget
*
* Revision 1.1  1997/03/02 23:03:00  newt
* Initial Revision
*
*****/ 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>

/* prevent Byte re-declaration */
#if defined(HAVE_LIBPNG) || defined(HAVE_LIBZ)
#include <zlib.h>
#endif

#include "toolkit.h"
#include XmHTMLPrivateHeader
#include "plc.h"

/* xpm checks whether Pixel has already been defined. IntrinsicP doesn't */
#include <X11/xpm.h>

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/
static XmHTMLRawImageData *doXpm(Widget html, ImageBuffer *ib,
	XpmImage *xpm_image);

/*** Private Variable Declarations ***/

/*****
* Name: 		doXpm
* Return Type: 	XmHTMLRawImageData*
* Description: 	converts the given xpm data to our own format
* In: 
*	html:		widget id;
*	ib:			current image buffer;
*	xpm_image:	xpm image data;
* Returns:
*
*****/
static XmHTMLRawImageData*
doXpm(Widget html, ImageBuffer *ib, XpmImage *xpm_image)
{
	int i, is_gray = 0;
	XColor tmpcolr;
	String col_name;
	Colormap cmap;
	static XmHTMLRawImageData *img_data;
	ToolkitAbstraction *tka;
	register Byte *bptr;
	register unsigned int *ptr;

	if(XmIsHTML(html))
		tka = HTML_ATTR(tka);
	else if(_xmimage_cfg == NULL || (tka = _xmimage_cfg->tka) == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "doXpm"),
			"XmImage(XPM): Unable to find a valid ToolkitAbstraction.");
		return(NULL);
	}

	/*
	* get colormap. We need one to obtain the RGB components of the
	* selected colors, pixmaps define their colors using a symbolic name
	* instead of defining the wanted RGB components.
	*/
	cmap = TkaGetColormap(html);

	/* allocate raw image */
	AllocRawImageWithCmap(img_data, (int)xpm_image->width,
		(int)xpm_image->height, (int)xpm_image->ncolors);

	/* little trick to compute image depth */
	if(ib != NULL)
	{
		ib->depth = 2;
		while((ib->depth << 2) < img_data->cmapsize && ib->depth < 12)
			ib->depth++;
	}

	/* fill colormap for this image */
	for(i = 0; i < img_data->cmapsize; i++)
	{
		/* pick up the name of the current color */
		col_name = xpm_image->colorTable[i].c_color;

		/* transparancy, these can *all* name a transparent color. */
		if(!(strcasecmp(col_name, "none")) ||
			!(strcasecmp(col_name, "mask")) ||
			!(strcasecmp(col_name, "background")))
		{
			Pixel bg_pixel;

			/*
			* Get current background index: use the given background pixel
			* index if we have one. Else get the current background color of
			* the given widget.
			*/
			if(XmIsHTML(html))
				bg_pixel = HTML_ATTR(body_bg);
			else
			{
				if(_xmimage_cfg->flags & ImageBackground)
					bg_pixel = _xmimage_cfg->bg_color;
				else
					bg_pixel = 0;
			}

			/* get RGB components for this color. */
			GETP(tmpcolr) = bg_pixel;
			tka->QueryColor(tka->dpy, cmap, &tmpcolr);

			/* store background pixel index */
			img_data->bg = i;
		}
		else /* get RGB components for this color */
			(void)tka->ParseColor(tka->dpy, cmap, col_name, &tmpcolr);

		/* 16bit RGB values */
		GETR(img_data->cmap[i]) = GETR(tmpcolr);
		GETG(img_data->cmap[i]) = GETG(tmpcolr);
		GETB(img_data->cmap[i]) = GETB(tmpcolr);
		is_gray &= (GETR(tmpcolr) == GETG(tmpcolr)) &&
					(GETG(tmpcolr) == GETB(tmpcolr));
	}
	/* no need to fill in remainder of colormap, gets done by AllocRawImage */
	img_data->color_class = (is_gray != 0 ? XmIMAGE_COLORSPACE_INDEXED :
		XmIMAGE_COLORSPACE_GRAYSCALE);

	/*****
	* convert xpm image data to our own internal format: array of indices
	* in the colormap for this image. First pixel at upper-left corner.
	* The XpmImage data is actually already in this format but as the
	* XpmImage data is unsigned int we need to check if the indices don't
	* exceed 255 (or we would get an out-of-bounds indexing leading to a
	* segmentation fault eventually).
	*****/
	ptr  = xpm_image->data;
	bptr = img_data->data;
	for(i = 0; i < (img_data->width * img_data->height); i++)
	{
		int pix;
		pix = (int)*ptr;
		if (pix > (XmHTML_MAX_IMAGE_COLORS - 1))
			pix = 0;
		*bptr++ = (Byte)pix;
		ptr++;
	}
	XpmFreeXpmImage(xpm_image);
	return(img_data);
}

/*****
* Name: 		_XmHTMLReadXPM
* Return Type: 	XmHTMLRawImageData*
* Description: 	reads an xpm image of any type from xpm data read from a file.
* In: 
*	html:		widget id;
*	ib:			image data;
* Returns:
*	allocated image upon success. NULL on failure.
*****/
XmHTMLRawImageData*
_XmHTMLReadXPM(Widget html, ImageBuffer *ib)
{
	XpmImage xpm_image;
	XpmInfo foo;
	int i;

	(void)memset(&xpm_image, 0, sizeof(xpm_image));
	(void)memset(&foo, 0, sizeof(foo));

	if((i = XpmCreateXpmImageFromBuffer((String)ib->buffer, &xpm_image, 
		&foo)) != XpmSuccess)
	{
		/* spit out appropriate error message */
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLReadXPM"),
			XMHTML_MSG_121, "Xpm", ib->file, XpmGetErrorString(i));

		/* release everything */
		XpmFreeXpmInfo(&foo);
		XpmFreeXpmImage(&xpm_image);
		return(NULL);
	}
	/* we don't use the returned info so free it */
	XpmFreeXpmInfo(&foo);

	/* convert xpm data to raw image data */
	return(doXpm(html, ib, &xpm_image));
}

/*****
* Name: 		_XmHTMLCreateXpmFromData
* Return Type: 	XmHTMLRawImageData*
* Description: 	reads an xpm image of any type from raw xpm data
* In: 
*	html:		widget id;
*	data:		xpm data
* Returns:
*	allocated image upon success. NULL on failure.
*****/
XmHTMLRawImageData*
_XmHTMLCreateXpmFromData(Widget html, char **data, String src)
{
	XpmImage xpm_image;
	XpmInfo foo;
	int i;

	(void)memset(&xpm_image, 0, sizeof(xpm_image));
	(void)memset(&foo, 0, sizeof(foo));

	if((i = XpmCreateXpmImageFromData(data, &xpm_image, &foo)) != XpmSuccess)
	{
		/* spit out appropriate error message */
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLCreateXpmFromData"),
			XMHTML_MSG_121, "Xpm", "(builtin)", XpmGetErrorString(i));

		/* release everything */
		XpmFreeXpmInfo(&foo);
		XpmFreeXpmImage(&xpm_image);
		return(NULL);
	}
	/* we don't use the returned info so free it */
	XpmFreeXpmInfo(&foo);

	/* convert xpm data to raw image data */
	return(doXpm(html, NULL, &xpm_image));
}

/*****
* Progressive Pixmap loading routines
*****/

void
_PLC_XPM_Init(PLC *plc)
{
	plc->plc_status = PLC_ABORT;
}

void
_PLC_XPM_ScanlineProc(PLC *plc)
{
	plc->plc_status = PLC_ABORT;
}

void
_PLC_XPM_Destructor(PLC *plc)
{
	plc->plc_status = PLC_ABORT;
}

