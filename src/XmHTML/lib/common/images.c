#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* images.c : XmHTML image loading/manipulation routines.
*
* This file Version	$Revision$
*
* Creation date:		Tue Dec 24 04:08:22 GMT+0100 1996
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
*
* Portions Copyright (C) 1994 by John Bradley. Used by permission.
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
* Revision 1.17  1998/04/27 06:59:53  newt
* bugfix in scaleImage
*
* Revision 1.16  1998/04/04 06:28:11  newt
* XmHTML Beta 1.1.3
*
* Revision 1.15  1997/10/23 00:25:02  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.14  1997/08/31 17:36:59  newt
* scaleImage fix: now uses real image dimensions instead of those read from
* file.
*
* Revision 1.13  1997/08/30 01:10:37  newt
* *lots* of changes: the XImage code is now shared between normal and
* progressive image load, proper alpha channel support (even for background
* images), proto changes for XCCGetPixels, XmImageInfo struct changes.
* Furthermore a large number of bugfixes in default image processing,
* _XmHTMLImageReplaceOrUpdate, _XmHTMLNewImage, processBodyImage and image
* releasing.
*
* Revision 1.12  1997/08/01 13:01:53  newt
* Progressive image loading changes.
*
* Revision 1.11  1997/05/28 01:50:50  newt
* Added support for the XmNmaxImageColors resource and color quantization.
* Added support for the XmImageConfig structure. Added support for the GZF
* image type. Moved all XmImage stuff to XmImage.c
*
* Revision 1.10  1997/04/29 14:27:38  newt
* Added all XmImage routines + changes to make XmImage work.
*
* Revision 1.9  1997/04/03 05:37:55  newt
* updateImageWord macro. Changed each XFreeColors call to a XCCFreeColors call.
* Added _XmHTMLLoadBodyImage. Changes on image scaling: correct image
* dimensions are now used.
*
* Revision 1.8  1997/03/28 07:15:32  newt
* XtDisplay bugfix: now always use XmHTML as argument. Previous implementation
* caused a segv when the widget's Destroy method was invoked.
* Character escapes sequences in alt text are now expanded.
*
* Revision 1.7  1997/03/20 08:12:10  newt
* PNG support. Replaced _XmHTMLReplaceImage and _XmHTMLUpdateImage by
* _XmHTMLReplaceOrUpdateImage.
* Bug fixes: color release, delayed image replacement
*
* Revision 1.6  1997/03/11 19:53:42  newt
* Animated Gif support. Added _XmHTMLGetImageType and _XmHTMLReleaseImage.
* All image readers now use a memory buffer instead of reading from file
*
* Revision 1.5  1997/03/04 18:47:30  newt
* animation stuff added
*
* Revision 1.4  1997/03/04 01:00:10  newt
* Delayed Image Loading: _XmHTMLReplaceImage, _XmHTMLUpdateImage,
* updateImageCopies
*
* Revision 1.3  1997/03/02 23:18:48  newt
* completely changed: image support has now been added
*
* Revision 1.2  1997/01/09 06:55:42  newt
* expanded copyright marker
*
* Revision 1.1  1997/01/09 06:41:32  newt
* Initial Revision
*
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/param.h>

#ifndef WINNT
#include <unistd.h>		/* required for SEEK_END on most Unixii */
#endif

/* prevent Byte re-declaration */
#if defined(HAVE_LIBPNG) || defined(HAVE_LIBZ)
#include <zlib.h>
#endif

#include "toolkit.h"
#include XmHTMLPrivateHeader

#ifdef HAVE_XCCP_H
#include "XCCP.h"
#endif

#include "plc.h"

#include "pixmaps/boomerang.xpm"
#include "pixmaps/noboomerang.xpm"
#include "icons.h"				/* W3D Working Draft WD-wwwicn-960729 */
#if 0
#include "xpmtags.h"			/* icon version of all HTML tags	*/
#endif

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/
#define DEFAULT_IMG_SUSPENDED	1	/* images suspended icon */
#define DEFAULT_IMG_UNSUPPORTED	2	/* unsupported image icon */

/*** Private Function Prototype Declarations ****/
static XmImageInfo* readGifAnimation(Widget w, ImageBuffer *ib);

static XmHTMLRawImageData *readImage(Widget html, ImageBuffer *ib);

static void initAlphaChannels(XmHTMLWidget html, Boolean for_body_image);

static void doAlphaChannel(XmHTMLWidget html, XmHTMLImage *image);

static unsigned long *makeColormap(XmHTMLWidget html, XmHTMLImage *image,
	XmImageInfo *info);

static void getImageAttributes(XmHTMLImage *image, String attributes);

static void addImageToList(XmHTMLWidget html, XmHTMLImage *image);

static XmHTMLImage *copyImage(XmHTMLImage *src, String attributes);

static XmHTMLImage *lookForImage(XmHTMLWidget html, String url,
	String attributes, Dimension *width, Dimension *height);

static XmImageInfo *imageDefaultProc(Widget w, XmHTMLRawImageData *img_data,
	String url);

static XmImageInfo *animDefaultProc(Widget w, XmHTMLRawImageData *img_data,
	XmHTMLRawImageData *master, int *global_used, int *global_used_size,
	Boolean return_global_used, String url);

static XmImageInfo *imageDelayedProc(Widget w, XmHTMLRawImageData *img_data,
	ImageBuffer *ib);

static XmImageInfo *defaultImage(XmHTMLWidget html, String src,
	int default_image_type, Boolean call_for_free);

static int getMaxColors(Widget w, int max_colors);

static void processBodyImage(XmHTMLWidget html, XmHTMLImage *body_image,
	Dimension width, Dimension height);

/*****
* scale an image to the given dimensions. Returns scaled data, leaves the
* original data untouched.
*****/
static Byte *scaleImage(XmImageInfo *image, Dimension new_w, Dimension new_h);

/* clip an image in the given bounding rectangle. */
static void clipImage(XmImageInfo *image, Dimension new_w, Dimension new_h);

/*****
* This macro updates the dimensions of the word represented by an image.
* The text layout routines in paint.c (SetText) considers text and images to
* be the same object: each is represented by a XmHTMLWord in which the
* dimensions of an object are given by a bounding rectangle. So when an image
* is updated, the bounding rectangle of this word also requires updating.
*
* This is a macro rather than a function 'cause it has the potential of being
* called multiple times.
*
* sanity checks to be satisfied: this image *must* have an owner, this owner
* *must* have a word and this word *must* be an image and this image *must* be
* equal to the current image.
*****/
#define updateImageWord(IMG) { \
	if((IMG)->owner != NULL && (IMG)->owner->words != NULL && \
		(IMG)->owner->words[0].image == (IMG)) \
	{ \
		(IMG)->owner->words[0].width = (IMG)->width; \
		(IMG)->owner->words[0].height = (IMG)->height; \
	} \
}

/*** Private Variable Declarations ***/

/*****
* Name: 		readImage
* Return Type: 	Byte*
* Description: 	image loading driver routine.
* In:
*	html:		widget id
*	ib:			image memory buffer.
*	width:		width of loaded image. Filled upon return
*	height:		height of loaded image. Filled upon return
*	colors:		colors used by the image. Filled upon return
*	bg:			background pixel for this image.
* Returns:
*
* Note:
*	X11 bitmaps, pixmaps and gif are supported by default. If the
*	system we are running on has the jpeglib, loading of jpeg files is
*	also supported. Same holds for png.
*****/
static XmHTMLRawImageData*
readImage(Widget html, ImageBuffer *ib)
{
	XmHTMLRawImageData *img_data = NULL;

	RewindImageBuffer(ib);

	switch(ib->type)
	{
		case IMAGE_GIF:
		case IMAGE_GZF:		/* our compatible gif format */
			img_data = _XmHTMLReadGIF(html, ib);
			_XmHTMLDebug(6, ("readImage: loaded gif image %s\n", ib->file));
			break;
		case IMAGE_XBM:
			img_data = _XmHTMLReadBitmap(html, ib);
			_XmHTMLDebug(6, ("readImage: loaded X11 bitmap image %s\n",
				ib->file));
			break;
		case IMAGE_XPM:
			img_data = _XmHTMLReadXPM(html, ib);
			_XmHTMLDebug(6, ("readImage: loaded Xpm3 image %s\n", ib->file));
			break;
		case IMAGE_JPEG:
			img_data = _XmHTMLReadJPEG(html, ib);
			_XmHTMLDebug(6, ("readImage: loaded jpeg image %s\n", ib->file));
			break;
		case IMAGE_PNG:
			img_data = _XmHTMLReadPNG(html, ib);
			_XmHTMLDebug(6, ("readImage: loaded png image %s\n", ib->file));
			break;
		case IMAGE_FLG:		/* treated wholy differently */
			break;
		case IMAGE_UNKNOWN:
			_XmHTMLDebug(6, ("Can't load image %s: unsupported image format?",
				ib->file));
		default:
			break;
	}
	/* store image type */
	if(img_data != NULL)
		img_data->type = ib->type;
	return(img_data);
}

/*****
* Name: 		clipImage
* Return Type: 	void
* Description: 	clips the given image to the given dimensions
* In:
*	image:		image to be clipped
*	new_w:		new width
*	new_h:		new height
* Returns:
*	nothing
* Note:
*	modified from xgif-1.2
*****/
static void
clipImage(XmImageInfo *image, Dimension new_w, Dimension new_h)
{
	Byte *data, *dataPtr, *imgPtr;
	int x, y;

	_XmHTMLDebug(6, ("images.c, clipImage, clipping %s. current "
		"dimensions: %ix%i, new: %ix%i\n", image->url, image->width,
		image->height, new_w, new_h));

	/* allocate memory for clipped image data */
	dataPtr = data = (Byte *)malloc(new_w*new_h);

	/* pick up current image data */
	imgPtr = image->data;

	/* clipping is done from top to bottom, left to right */
	for(y = 0; y < new_h; y++)
	{
		for(x = 0; x < new_w; x++)
			*(dataPtr++) = *(imgPtr++);
		/* skip anything outside image width */
		while(x < image->width)
			imgPtr++;
	}

	/* free previous (unclipped) image data */
	free(image->data);

	/* new clipped image data */
	image->data = data;

	/* new image dimensions */
	image->width = new_w;
	image->height= new_h;

	_XmHTMLDebug(6, ("images.c, clipImage end\n"));
}

/*****
* Name: 		scaleImage
* Return Type: 	Byte*
* Description: 	scales the given image to the given dimensions
* In:
*	image:		image to be scaled
*	new_w:		new width
*	new_h:		new height
* Returns:
*	scaled image data. The clipmask *bitmap* is recreated instead of
*	modifying the clipmask data.
* Note:
*	modified from xgif-1.2
*****/
static Byte*
scaleImage(XmImageInfo *image, Dimension new_w, Dimension new_h)
{
	Byte *data, *img_data, *ilptr, *ipptr, *elptr, *epptr;
	int ix, iy, ex, ey, src_w, src_h;

	_XmHTMLDebug(6, ("images.c, scaleImage, scaling %s. current "
		"dimensions: %ix%i, new: %ix%i\n", image->url, image->width,
		image->height, new_w, new_h));

	/* allocate memory for scaled image data */
	data = (Byte *)calloc(new_w*new_h, sizeof(Byte));

	/* pick up current image data */
	img_data = image->data;

	/* use dimensions as read from the image, not current dimensions!!! */
	src_w = image->swidth;
	src_h = image->sheight;

	/* initialize scaling */
	elptr = epptr = data;

	/* scaling is done from top to bottom, left to right */
	for(ey = 0 ; ey < new_h; ey++, elptr += new_w)
	{
		/* vertical pixel skip */
		iy = (src_h * ey) / new_h;
		epptr = elptr;
		ilptr = img_data + (iy * src_w);
		for(ex = 0; ex < new_w; ex++, epptr++)
		{
			/* horizontal pixel skip */
			ix = (src_w * ex) / new_w;
			ipptr = ilptr + ix;
			*epptr = *ipptr;
		}
	}
	/* leave external (unscaled) image data UNTOUCHED!! */

	/*****
	* Create new clipmask data if we have one instead of resizing the
	* clipmask itself.
	*****/
	if(ImageInfoClipmask(image))
	{
		int bcnt, clipsize;
		int i, j, bg_pixel;

		/*****
		* First get original transparent pixel, it's the first
		* pixel that has a zero bit in the bitmap bits of the current
		* clipmask.
		*****/
		bg_pixel = -1;

		/* compute original clipmask size */
		clipsize = src_w;

		/* make it byte-aligned */
		while((clipsize % 8))
		clipsize++;

		/* this many bytes on a row */
		clipsize /= 8;

		/* size of clipmask */
		clipsize *= src_h;

		ilptr = image->clip;
		elptr = image->data;

		for(i = 0; i < clipsize && bg_pixel == -1; i++, ilptr++)
		{
			for(j = 0; j < 8; j++)
			{
				if(!(*ilptr & bitmap_bits[j]))
				{
					bg_pixel = (int)*elptr;
					break;
				}
				elptr++;
			}
		}
		/* bad clipmask!! */
		if(bg_pixel == -1)
		{
#ifdef DEBUG
			_XmHTMLWarning(__WFUNC__(NULL, "scaleImage"),
				"WARNING: failed to detect transparent pixel while scaling "
				"clipmask!");
#endif
			/* remove clipmask */
			free(image->clip);
			image->options &= ~XmIMAGE_CLIPMASK;

			/* store new image width and height */
			image->width = new_w;
			image->height= new_h;

			_XmHTMLDebug(6, ("images.c, scaleImage End\n"));

			/* return the scaled image data */
			return(data);
		}

		/* compute new clipmask size */
		clipsize = new_w;

		while((clipsize % 8))
			clipsize++;

		clipsize /= 8;

		clipsize *= new_h;

		/* resize clipmask data */
		image->clip = (Byte*)realloc(image->clip, clipsize);

		/* zero it */
		memset(image->clip, 0, clipsize);
		ilptr = image->clip;
		elptr = data;

		/* recreate bitmap */
		for(iy = 0; iy < new_h; iy++)
		{
			for(ix = 0, bcnt = 0; ix < new_w; ix++)
			{
				if(*elptr != (Byte)bg_pixel)
					*ilptr += bitmap_bits[(bcnt % 8)];
				if((bcnt % 8) == 7 || ix == (new_w - 1))
					ilptr++;
				bcnt++;
				elptr++;
			}
		}
	}

	/* new image width and height */
	image->width = new_w;
	image->height= new_h;

	_XmHTMLDebug(6, ("images.c, scaleImage End\n"));

	/* return the scaled image data */
	return(data);
}

/*****
* Name: 		getMaxColors
* Return Type: 	int
* Description: 	check maximum number of colors allowed for current display.
* In:
*	w:			widget id;
*	max_colors:	current setting for maximum image colors.
* Returns:
*	maximum number of colors for current display.
*****/
static int
getMaxColors(Widget w, int max_colors)
{
	int ncolors;
	VISUAL *visual = NULL;

	/* get visual for this widget and take maximum colors from there. */
	visual = XCCGetParentVisual(w);

	/* maximum colors supported for this type of visual but no more than 256 */
	ncolors = TkaVisualGetMapEntries(visual) > XmHTML_MAX_IMAGE_COLORS ?
			XmHTML_MAX_IMAGE_COLORS : TkaVisualGetMapEntries(visual);

	if(max_colors > ncolors)
	{
		_XmHTMLWarning(__WFUNC__(w, "getMaxColors"), XMHTML_MSG_61,
			max_colors, ncolors, ncolors);
		return(ncolors);
	}
	else if(!max_colors)
		return(ncolors);
	return(max_colors);
}

/*****
* A function used when an XImage can not be created for this type of
* display (depth and/or bits_per_pixel).
*****/
static XIMAGE*
XImageBizarre(XmHTMLWidget html, int depth, XIMAGE *ximage)
{
	_XmHTMLWarning(__WFUNC__(html, "_XmHTMLCreateXImage"),
		XMHTML_MSG_62, depth, TkaImageBitsPerPixel(ximage));

	html->html.tka->DestroyImage(ximage);

	return((XIMAGE*)NULL);
}

/*****
* Name: 		_XmHTMLCreateXImage
* Return Type: 	XImage
* Description: 	Image XImage creation routine.
* In:
*	html:		XmHTMLWidget id;
*	xcc:		XColorContext info for this image;
*	width, height:
*				dimensions of XImage to be created;
*	url:		current image identifier
* Returns:
*	a newly created XImage with an allocated data member.
*****/
XIMAGE*
_XmHTMLCreateXImage(XmHTMLWidget html, XCC xcc, Dimension width,
	Dimension height, String url)
{
	int depth      = XCCGetDepth(xcc);
	VISUAL *vis   = xcc->visual;
	static XIMAGE *ximage = NULL;
	ToolkitAbstraction *tka = NULL;

	if(XmIsHTML((Widget)html))
		tka = html->html.tka;
	else if(_xmimage_cfg != NULL)
		tka = _xmimage_cfg->tka;
	else
		return(NULL);

	_XmHTMLDebug(6, ("images.c: _XmHTMLCreateXImage, creating XImage\n"));

	/* branch to correct display depth */
	switch(depth)
	{
		case 1:
			{
				Byte *data;

				ximage = tka->CreateImage(tka->dpy, vis, depth, XYPixmap,
					0, NULL, width, height, 32, 0);

				data = (Byte*)malloc(TkaImageBytesPerLine(ximage) * height);
				TkaImageData(ximage) = (char *)data;

				/*****
				* FIXME
				* Add code to dither this image down to 1bit depth.
				*****/
			}
			break;
		case 2:
			{
				Byte *data;
				int bpp;

				ximage = tka->CreateImage(tka->dpy, vis, depth, ZPixmap,
					0, NULL, width, height, 8, 0);

				bpp = TkaImageBitsPerPixel(ximage);

				if(bpp != 2 && bpp != 4 && bpp != 8)
					return(XImageBizarre(html, depth, ximage));

				data = (Byte*)malloc(TkaImageBytesPerLine(ximage) * height);
				TkaImageData(ximage) = (char*)data;
			}
			break;
		case 4:
			{
				Byte *data;
				int bpp;

				ximage = tka->CreateImage(tka->dpy, vis, depth, ZPixmap,
					0, NULL, width,  height, 8, 0);

				bpp = TkaImageBitsPerPixel(ximage);

				if(bpp != 4 && bpp != 8)
					return(XImageBizarre(html, depth, ximage));

				data = (Byte*)malloc(TkaImageBytesPerLine(ximage) * height);
				TkaImageData(ximage) = (char*)data;
			}
			break;
		case 5:
		case 6:
			{
				Byte *data;

				ximage = tka->CreateImage(tka->dpy, vis, depth, ZPixmap,
					0, NULL, width, height, 8, 0);

				if(TkaImageBitsPerPixel(ximage) != 8)
					return(XImageBizarre(html, depth, ximage));

				data = (Byte*)malloc(TkaImageBytesPerLine(ximage) * height);
				TkaImageData(ximage) = (char*)data;
			}
			break;
		case 8:
			{
				Byte *data;
				int imWIDE, nullCount;

				/* no of padding bytes per line */
				nullCount = (4 - (width % 4)) & 0x03;

				imWIDE = width + nullCount;

				data = (Byte*)malloc(imWIDE * height);

				ximage = tka->CreateImage(tka->dpy, vis, depth, ZPixmap, 0,
					(char*)data, width,  height, 32, imWIDE);
			}
			break;
		case 12:
		case 15:
		case 16:
			{
				unsigned short *data;

				ximage = tka->CreateImage(tka->dpy, vis, depth, ZPixmap,
					0, NULL, width, height, 16, 0);

				if(depth == 12 && TkaImageBitsPerPixel(ximage) != 16)
					return(XImageBizarre(html, depth, ximage));

				data = (unsigned short*)malloc(2 * width * height);
				TkaImageData(ximage) = (char*)data;
			}
			break;
		case 24:
		case 32:
			{
				Byte *data;

				ximage = tka->CreateImage(tka->dpy, vis, depth, ZPixmap,
					0, NULL, width, height, 32, 0);

				data = (Byte*)malloc(4 * width * height);
				TkaImageData(ximage) = (char*)data;
			}
			break;
		default:
			{
				/* too bad, we refuse to run on this display */
				_XmHTMLWarning(__WFUNC__(html, "_XmHTMLCreateXImage"),
					XMHTML_MSG_63, depth);
				return(NULL);
			}
			break;
	}
	if(ximage == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLCreateXImage"),
			XMHTML_MSG_64, url ? url : "(animation frame)");
		return(NULL);
	}
	return(ximage);
}

/*****
* Name: 		_XmHTMLFillXImage
* Return Type: 	void
* Description: 	Image data->ximage transfer function
* In:
*	html:		XmHTMLWidget id;
*	ximage:		ximage to be filled;
*	xcc:		XColorContext for this image;
*	data:		raw image data;
*	xcolors:	array of allocated pixel values;
*	*start:		starting index in data, or NULL;
*	*end:		ending index in data, or NULL. If non-NULL updated upon
*				return to align on scanline boundary;
* Returns:
*	nothing.
*****/
void
_XmHTMLFillXImage(XmHTMLWidget html, XIMAGE *ximage, XCC xcc, Byte *data,
	unsigned long *xcolors, int *start, int *end)
{
	int hi, lo;
	unsigned int wide, high;
	unsigned long xcol;
	register int i;

	wide    = ximage->width;
	high    = ximage->height;

	/*
	* hi is max. no of bytes available. lo is min. no of bytes available.
	* Any data lower than lo has already been processed, so we do not do
	* that again.
	*/
	if(start != NULL)
	{
		lo = *start;
		hi = *end;
	}
	else
	{
		hi = wide*high;
		lo = 0;
	}

	_XmHTMLDebug(6, ("images.c: _XmHTMLFillXImage, doing scanline %i to %i "
		"(bytes %i to %i)\n", lo/wide, hi/wide, lo, hi));

	switch(XCCGetDepth(xcc))
	{
		case 8:
		{
			Byte *imagedata;
			int imWIDE, nullCount;
			register int j;
			register Byte *ip, *pp;

			/*****
			* XXX: this nullCount calculation should work in Gdk because
			* GdkImage always uses 32-bit padding of scanlines.
			* I don't know why this code does not simply use
			* bytes_per_line and such.
			* I don't either -- kdh
			*****/

			/* # of padding bytes per line */
			nullCount = (4 - (wide % 4)) & 0x03;

			imWIDE = wide + nullCount;

			lo /= wide;	/* starting scanline index (image data) */
			hi /= wide;	/* ending scanline index (image data) */

			/*
			* lo*imWIDE contains the no of XImage data bytes already
			* processed.
			*/
			imagedata = (Byte*)TkaImageData(ximage) + lo*imWIDE;
			pp = data;

			for(i = lo, ip = imagedata; i < hi; i++)
			{
				for(j = 0; j < wide; j++, ip++, pp++)
					 *ip = (Byte) xcolors[*pp];
				for(j = 0; j < nullCount; j++, ip++)
					*ip = 0;
			}
		}
		break;

		case 4:
		{
			Byte *imagedata, *lip, *ip;
			int bpl;
			register int j, half;
			register Byte *pp;

			bpl = TkaImageBytesPerLine(ximage);

			pp = data;

			if(TkaImageCheck4bpp(ximage))
			{
				lo /= wide;	/* starting scanline index (image data) */
				hi /= wide;	/* ending scanline index (image data) */

				/* compute offset into XImage data */
				imagedata = (Byte*)TkaImageData(ximage) + lo*bpl;

				for(i = lo, lip = imagedata; i < hi; i++, lip += bpl)
				{
					for(j = 0, ip = lip, half = 0; j < wide; j++, pp++, half++)
					{
						xcol = xcolors[*pp] & 0x0f;
						if(TkaImageBitmapBitOrder(ximage) == LSBFIRST)
						{
							if(half&1)
							{
								*ip = *ip + (xcol<<4);
								ip++;
							}
							else
								*ip = xcol;
						}
						else
						{
							if(half&1)
							{
								*ip = *ip + xcol;
								ip++;
							}
							else
								*ip = xcol << 4;
						}
					}
				}
			}
			else /* ximage->bits_per_pixel == 8 */
			{
				/* compute offset into XImage data */
				imagedata = (Byte*)TkaImageData(ximage) + lo;

				for(i = hi, ip = imagedata; i > lo; i--, pp++, ip++)
					*ip = (Byte) xcolors[*pp];
			}
		}
		break;

		case 2:
		{
			Byte *imagedata, *lip, *ip;
			int bpl;
			register int j, half;
			register Byte *pp;

			bpl = TkaImageBytesPerLine(ximage);

			pp = data;

			if(TkaImageCheck2bpp(ximage))
			{

				lo /= wide;	/* starting scanline index (image data) */
				hi /= wide;	/* ending scanline index (image data) */

				imagedata = (Byte*)TkaImageData(ximage) + lo*bpl;

				for(i = lo, lip = imagedata; i < hi; i++, lip += bpl)
				{
					for(j = 0, ip = lip, half=0; j < wide; j++, pp++, half++)
					{
						xcol = xcolors[*pp] & 0x03;

						if(TkaImageBitmapBitOrder(ximage) == LSBFIRST)
						{
							if(half%4==0)
								*ip  = xcol;
							else if (half%4==1)
								*ip |= (xcol<<2);
						    else if (half%4==2)
								*ip |= (xcol<<4);
							else
							{
								*ip |= (xcol<<6);
								ip++;
							}
						}
						else
						{
							/* MSBFIRST.  NeXT, among others */
							if(half%4==0)
								*ip  = (xcol<<6);
							else if (half%4==1)
								*ip |= (xcol<<4);
							else if (half%4==2)
								*ip |= (xcol<<2);
							else
							{
								*ip |= xcol;
								ip++;
							}
						}
					}
				}
			}
			else
			{
				if(TkaImageCheck4bpp(ximage))
				{
					lo /= wide;	/* starting scanline index (image data) */
					hi /= wide;	/* ending scanline index (image data) */

					imagedata = (Byte*)TkaImageData(ximage) + lo*bpl;

					for(i = lo, lip = imagedata; i < hi; i++, lip += bpl)
					{
						for(j = 0, ip = lip, half = 0; j < wide;
							j++, pp++, half++)
						{
							xcol = xcolors[*pp] & 0x0f;

							if(TkaImageBitmapBitOrder(ximage) == LSBFIRST)
							{
								if(half&1)
								{
									*ip |= (xcol<<4);
									ip++;
								}
								else
									*ip = xcol;
							}
							else
							{
								/* MSBFIRST */
								if(half&1)
								{
									*ip |= xcol;
									ip++;
								}
								else
									*ip = xcol << 4;
							}
						}
					}
				}
				else /* ximage->bits_per_pixel == 8 */
				{
					imagedata = (Byte*)TkaImageData(ximage) + lo;

					for(i = hi, ip = imagedata; i > lo; i--, pp++, ip++)
						*ip = (Byte)xcolors[*pp];
				}
			}
		}
		break;

		case 5:
		case 6:
		{
			int bpl;
			Byte *imagedata;
			register Byte *ip, *pp;

			bpl = TkaImageBytesPerLine(ximage);
			imagedata = (Byte*)TkaImageData(ximage) + (lo/wide)*bpl;

			pp = data;

			for(i = hi, ip = imagedata; i > lo; i--, pp++, ip++)
				*ip = (Byte)xcolors[*pp];
		}
		break;

		case 12:
		case 15:
		case 16:
		{
			unsigned short *imagedata;
			int bpl;
			register unsigned short *ip;
			register Byte *pp;

			/* 2 bytes per pixel */
			bpl = TkaImageBytesPerLine(ximage);
			imagedata = (unsigned short*)TkaImageData(ximage) + (lo/wide)*bpl;

			pp = data;

			if(TkaImageByteOrder(ximage) == MSBFIRST)
			{
				for(i = hi, ip = imagedata; i > lo; i--, pp++)
					*ip++ = (unsigned short)(xcolors[*pp] & 0xffff);
			}
			else
			{
				/* LSBFIRST */
				for(i = hi, ip = imagedata; i > lo; i--, pp++)
					*ip++ = (unsigned short)(xcolors[*pp]);
			}
		}
		break;

		case 24:
		case 32:
		{
			Byte *imagedata, *ip;
			int do32, bpl;
			register int j;
			register Byte *pp, *tip;

			bpl = TkaImageBytesPerLine(ximage);

			lo /= wide;	/* starting scanline index (image data) */
			hi /= wide;	/* ending scanline index (image data) */

			/* 4 bytes per pixel */
			imagedata = (Byte*)TkaImageData(ximage) + lo*bpl;

			do32 = TkaImageCheck32bpp(ximage);

			pp = data;

			if(TkaImageByteOrder(ximage) == MSBFIRST)
			{
				for(i = lo, ip = imagedata; i < hi; i++)
				{
					for(j = 0, tip = ip; j < wide; j++, pp++)
					{
						xcol = xcolors[*pp];

						if (do32)
							*tip++ = 0;
						*tip++ = (xcol>>16) & 0xff;
						*tip++ = (xcol>>8) & 0xff;
						*tip++ =  xcol & 0xff;
					}
					ip += bpl;
				}
			}
			else
			{
				/* LSBFIRST */
				for(i = lo, ip = imagedata; i < hi; i++)
				{
					for(j = 0, tip = ip; j < wide; j++, pp++)
					{
						xcol = xcolors[*pp];

						*tip++ =  xcol & 0xff;
						*tip++ = (xcol>>8) & 0xff;
						*tip++ = (xcol>>16) & 0xff;
						if(do32)
							*tip++ = 0;
					}
					ip += bpl;
				}
			}
		}
		break;

		default:
			break;
	}
}

/*****
* Name: 		makeColormap
* Return Type: 	int*
* Description: 	allocates the colors for the given image and creates an
*				array of indexed pixel values (which is a sort of private
*				colormap).
* In:
*	image:		internal image data
*	info:		raw image data. Does not have to be the same as
*				image->html_image
* Returns:
*	the pixel array.
* Note:
*	There's a call to XCCFreeColors in here: when an image is being updated
*	or replaced, the XmHTMLImage structure has already got colors allocated
*	(by a default image or a previous image) and these must be freed before
*	new colors are allocated.
*****/
static unsigned long*
makeColormap(XmHTMLWidget html, XmHTMLImage *image, XmImageInfo *info)
{
	static unsigned long *color_map;

	image->npixels = 0;

	/* allocate color_map pixel entries */
	color_map = (unsigned long*)calloc(info->ncolors, sizeof(unsigned long));

	XCCGetPixels(image->xcc, info->reds, info->greens, info->blues,
		info->ncolors, color_map, &image->npixels);

	return(color_map);
}

/*****
* Name: 		freePixmaps
* Return Type: 	void
* Description: 	frees all pixmaps and allocated colors for the given image
* In:
*	html:		XmHTMLWidget id
*	image:		image for which to release pixmaps and colors
* Returns:
*	nothing.
*****/
static void
freePixmaps(XmHTMLWidget html, XmHTMLImage *image)
{
	ToolkitAbstraction *tka = html->html.tka;

	/* first free all previous pixmaps */
	if(image->frames)
	{
		int i;
		for(i = 0; i < image->nframes; i++)
		{
			FreePixmap(tka->dpy, image->frames[i].pixmap);
			FreePixmap(tka->dpy, image->frames[i].clip);
			FreePixmap(tka->dpy, image->frames[i].prev_state);
		}
		if(ImageHasState(image))
		{
			_XmHTMLDebug(6, ("images.c: freePixmaps, freeing animation state "
				"maintainer\n"));
			FreePixmap(tka->dpy, image->pixmap);
		}
		free(image->frames);
		image->frames = NULL;
	}
	else
	{
		FreePixmap(tka->dpy, image->pixmap);
		FreePixmap(tka->dpy, image->clip);
	}
	image->pixmap = image->clip = None;
	image->npixels = 0;
}

/*****
* Name: 		getImageAttributes
* Return Type: 	void
* Description: 	retrieves all possible attribute specifications for the
*				IMG element.
* In:
*	image:		image data in which to store the parsed attributes
*	attributes:	raw attribute specifications to the IMG element
* Returns:
*	nothing, but the image structure is updated.
*****/
static void
getImageAttributes(XmHTMLImage *image, String attributes)
{
	if((image->alt = _XmHTMLTagGetValue(attributes, "alt")) != NULL)
	{
		/* handle escape sequences in the alt text */
		_XmHTMLExpandEscapes(image->alt, False);
	}
	else
	{
		/* if we have a real URL or some path to this image, strip it off */
		if(strstr(image->url, "/"))
		{
			int i;
			for(i = strlen(image->url) - 1;
				i > 0 && image->url[i] != '/'; i--);
			image->alt = strdup(&image->url[i+1]);
		}
		else	/* fix 09/01/97-01, kdh */
			image->alt = strdup(image->url);
	}

	/*****
	* Don't check border yet, will be done in format.c 'cause default
	* bordering is context-sensitive: no border if the image is not an
	* anchor, one when it is an anchor.
	*****/
	image->hspace = _XmHTMLTagGetNumber(attributes, "hspace", 0);
	image->vspace = _XmHTMLTagGetNumber(attributes, "vspace", 0);
	image->align  = _XmHTMLGetImageAlignment(attributes);

	/*
	* Imagemap stuff. First check if we have a usemap spec. If so, it's
	* automatically a client-side imagemap. If no usemap is given but the
	* ISMAP attribute is set this is a server-side imagemap.
	*/
	if((image->map_url = _XmHTMLTagGetValue(attributes, "usemap")) != NULL)
		image->map_type = XmMAP_CLIENT;
	else if(_XmHTMLTagCheck(attributes, "ismap"))
		image->map_type = XmMAP_SERVER;
	else
		image->map_type = XmMAP_NONE;
}

/*****
* Name: 		copyImage
* Return Type: 	XmHTMLImage
* Description: 	links the most wastefull members of src to a new image
* In:
*	src:		source image
*	attributes:	attributes for this image
* Returns:
*	a copy of src
* Note:
*	dest has it's is_copy member set to true which tells XmHTML not to free
*	the html_image, xcc and pixmap members of the copy.
*****/
static XmHTMLImage*
copyImage(XmHTMLImage *src, String attributes)
{
	static XmHTMLImage *dest;

	_XmHTMLDebug(6, ("images.c: copyImage, making copy of %s\n", src->url));

	/* we haven't got it yet, create a new one */
	dest = (XmHTMLImage*)malloc(sizeof(XmHTMLImage));

	/* initialise all fields to zero */
	(void)memset(dest, 0, sizeof(XmHTMLImage));
	dest->magic = XmHTML_IMAGE_MAGIC;

	/* This isn't exactly a copy, but almost a full referential */
	dest->url           = src->url;
	dest->height        = src->height;
	dest->width         = src->width;
	dest->sheight       = src->sheight;
	dest->swidth        = src->swidth;
	dest->pixmap        = src->pixmap;
	dest->clip          = src->clip;
	dest->frames        = src->frames;
	dest->nframes       = src->nframes;
	dest->current_frame = src->current_frame;
	dest->current_loop  = src->current_loop;
	dest->loop_count    = src->loop_count;
	dest->options       = src->options;
	dest->html_image    = src->html_image;

	/* unique for each image */
	getImageAttributes(dest, attributes);

	/* This tells XmHTML which members to free and which not */
	dest->options |= IMG_ISCOPY;

	return(dest);
}

/*****
* Name: 		lookForImage
* Return Type: 	XmHTMLImage
* Description: 	see if the image at the given url is in the current list of
*				images.
* In:
*	html:		XmHTMLWidget id
*	url:		image location
*	attributes:	extra image attributes
*	width:		possible width specification
*	height:		possible height specification
* Returns:
*	XmHTMLImage structure for this image if found, NULL if not found.
* Note:
*	If a match is found, the dimensions of that image are checked against
*	possible specifications. When no specifications are given, we can just
*	return the image found so a copy can be made. If however specifications
*	are given, we need an exact match since I don't know a way to resize
*	pixmaps.
*****/
static XmHTMLImage*
lookForImage(XmHTMLWidget html, String url, String attributes,
	Dimension *width, Dimension *height)
{
	XmHTMLImage *image;

	_XmHTMLDebug(6, ("images.c: lookForImage, checking private cache for %s\n",
		url));

	for(image = html->html.images; image != NULL; image = image->next)
	{
		/* images that are already a copy can't be copied */
		if(image->url && !ImageIsCopy(image) && !(strcmp(image->url, url)))
		{
			/*
			* all possible combinations if dimension specification.
			* We always need to compare against the specified dimensions:
			* even though the image may be the same, different dimensions
			* require it to be scaled. We want to have the original data
			* or we would have a tremendous loss of information.
			*/
			if((!*height && !*width) ||
					(*height == image->sheight && *width == image->swidth) ||
					(!*height && *width == image->swidth) ||
					(!*width && *height == image->sheight))
			{
				_XmHTMLDebug(6, ("images.c: lookForImage, %s found!\n", url));
				if(!*height)
					*height = image->sheight;
				if(!*width)
					*width = image->swidth;
				return(image);
			}
			_XmHTMLDebug(6, ("images.c: lookForImage, %s found but "
				"dimensions differ: %i,%i versus %i,%i\n", url,
				image->swidth, image->sheight, *width, *height));
		}
	}

	_XmHTMLDebug(6, ("images.c: lookForImage, %s not yet loaded.\n", url));
	return(NULL);
}

/*****
* Name: 		addImageToList
* Return Type: 	void
* Description: 	adds the given image to the list of images of a HTML widget
* In:
*	html:		XmHTMLWidget owning this image
*	image:		image to store
* Returns:
*	nothing.
*****/
static void
addImageToList(XmHTMLWidget html, XmHTMLImage *image)
{
	XmHTMLImage *tmp;

	/* head of the list */
	if(html->html.images == NULL)
	{
		html->html.images = image;
		return;
	}

	/* walk to the one but last image in the list and insert the image */
	for(tmp = html->html.images; tmp != NULL && tmp->next != NULL;
		tmp = tmp->next);
	tmp->next = image;
}

/*****
* Name: 		defaultImage
* Return Type: 	XmImageInfo
* Description: 	creates an internal image
* In:
*	html:		XmHTMLWidget
*	src:		name of image
*	default_image_type: type of internal image to create
* Returns:
*	XmImageInfo structure for the requested default image
*****/
static XmImageInfo*
defaultImage(XmHTMLWidget html, String src, int default_image_type,
	Boolean call_for_free)
{
	static XmImageInfo *unsupported, *suspended;
	char **xpm_data;
	XmHTMLRawImageData *data;

	_XmHTMLDebug(6, ("images.c: defaultImage, called for %s image\n",
		default_image_type == DEFAULT_IMG_SUSPENDED ?
			"suspended" : "unsupported"));

	if(default_image_type == DEFAULT_IMG_SUSPENDED)
	{
		if(call_for_free || suspended != NULL)
			return(suspended);
		else
			xpm_data = boomerang_xpm;
	}
	else if(default_image_type == DEFAULT_IMG_UNSUPPORTED)
	{
		if(call_for_free || unsupported != NULL)
			return(unsupported);
		else
			xpm_data = noboomerang_xpm;
	}
	else
		_XmHTMLError(__WFUNC__(html, "defaultImage"), "Internal "
			"Error: default image requested but don't know the type!");

	data = _XmHTMLCreateXpmFromData((Widget)html, xpm_data, src);

	/*
	* The default images *must* use a clipmask: not doing this would
	* use the current background for the transparent pixel in every
	* document. And since each document doesn't have the same background
	* this would render the wrong transparent color when the document
	* background color changes.
	* Also, these images may *never* bee freed.
	*/
	if(default_image_type == DEFAULT_IMG_SUSPENDED)
	{
		suspended = imageDefaultProc((Widget)html, data, src);
		suspended->type = IMAGE_XPM;
		suspended->options &= ~XmIMAGE_DEFERRED_FREE;
		suspended->options |= XmIMAGE_SHARED_DATA;
		suspended->depth = 4;	/* 15 colors */
		return(suspended);
	}
	else
	{
		unsupported = imageDefaultProc((Widget)html, data, src);
		unsupported->type = IMAGE_XPM;
		unsupported->options &= ~XmIMAGE_DEFERRED_FREE;
		unsupported->options |= XmIMAGE_SHARED_DATA;
		unsupported->depth = 4;	/* 15 colors */
		return(unsupported);
	}
}

/*****
* Name: 		imageDefaultProc
* Return Type: 	XmImageInfo*
* Description: 	XmHTML default image loading procedure
* In:
*	w:			Widget ID;
*	img_data:	raw image data;
*	url:		full name and location of image to load
* Returns:
*	An XmImageInfo structure on success or NULL on failure
*****/
static XmImageInfo*
imageDefaultProc(Widget w, XmHTMLRawImageData *img_data, String url)
{
	XmImageInfo *image = NULL;
	int	i, j, cnt, bcnt, size, used[XmHTML_MAX_IMAGE_COLORS];
	int	bg_red = 0, bg_green = 0, bg_blue = 0, clipsize = 0;
	Byte *ptr, *cptr, *clip = NULL;
	int do_clip = 0, max_colors = 0;
	Boolean clip_valid = False, dither = False;
	ToolkitAbstraction *tka = NULL;

	/* default XmImageInfo flags */
	int options = XmIMAGE_DEFERRED_FREE|XmIMAGE_RGB_SINGLE|XmIMAGE_ALLOW_SCALE;

	if(XmIsHTML(w))
	{
		max_colors = ((XmHTMLWidget)w)->html.max_image_colors;
		/*****
		* If we are mapping to palette, the JPEG decoder has already
		* used it, hence there is no reason to dither JPEG images.
		*****/
		dither = ((XmHTMLWidget)w)->html.map_to_palette != XmDISABLED &&
					img_data->type != IMAGE_JPEG;
		tka = ((XmHTMLWidget)w)->html.tka;
	}
	else
	{
		if(_xmimage_cfg == NULL)
			return(NULL);

		/* check quantization stuff */
		if(_xmimage_cfg->flags && ImageQuantize)
			max_colors = _xmimage_cfg->ncolors;
		/* and verify no of colors */
		max_colors = getMaxColors(w, max_colors);
		tka = _xmimage_cfg->tka;
	}

	/* raw data size */
	size = img_data->width * img_data->height;

	/*
	* If we have a background pixel, it means we have a transparent image.
	* To make it really transparent, pick up the background pixel and
	* corresponding RGB values so it can be substituted in the image.
	*/
	if(img_data->bg >= 0)
	{
		_XmHTMLDebug(6, ("images.c: imageDefaultProc, image is transparent\n"));

		/*
		* We only substitute the background pixel if it has been
		* requested explicitly by the ImageBackground flag on the
		* global XmImageConfig structure.
		*/
		if(_xmimage_cfg != NULL)
		{
			/* Clipmask has priority above bg pixel substitution */
			if(_xmimage_cfg->flags & ImageClipmask)
			{
				options |= XmIMAGE_CLIPMASK;
				do_clip = 1;
			}
			else if(_xmimage_cfg->flags & ImageBackground)
			{
				XCOLOR bg_color;
				COLORMAP cmap;

				_XmHTMLDebug(6, ("images.c: imageDefaultProc, background pixel "
					"value for this image: %i\n", img_data->bg));

				/* get background color index */
				GETP(bg_color) = _xmimage_cfg->bg_color;

				_XmHTMLDebug(6, ("images.c: imageDefaultProc, provided "
					"background pixel is: %li\n", GETP(bg_color)));

				/* get colormap */
				cmap = TkaGetColormap(w);

				/* get RGB value for the background pixel */
				tka->QueryColor(tka->dpy, cmap, &bg_color);

				bg_red   = GETR(img_data->cmap[img_data->bg]) = GETR(bg_color);
				bg_green = GETG(img_data->cmap[img_data->bg]) = GETG(bg_color);
				bg_blue  = GETB(img_data->cmap[img_data->bg]) = GETB(bg_color);
				/*****
				* We need to allocate a clipmask anyway so we can properly
				* restore transparency when we need to quantize the image.
				*****/
				do_clip = 2;
			}
		}
		else
		{
			/* XmHTML call or default settings, use a clipmask */
			options |= XmIMAGE_CLIPMASK;
			do_clip = 1;
		}
	}
	/* we've got ourselves a clipmask */
	if(do_clip)
	{
		i = img_data->width;

		/* make it byte-aligned */
		while((i % 8))
			i++;

		/* this many bytes on a row */
		i /= 8;

		/* size of clipmask */
		clipsize = i * img_data->height;

		clip = (Byte*)calloc(clipsize, sizeof(Byte));
	}

	/* allocate an image */
	image = (XmImageInfo*)malloc(sizeof(XmImageInfo));

	/* initialize to zero */
	(void)memset(image, 0, sizeof(XmImageInfo));

	/* initialize used colors counter */
	memset(&used, 0, XmHTML_MAX_IMAGE_COLORS*sizeof(int));

	/* Fill in appropriate fields */
	image->url     = strdup(url);	/* image location */
	image->data    = img_data->data;	/* image data bits */
	image->clip    = clip;				/* clipbitmap bits */
	image->type    = img_data->type;	/* image type */
	image->width   = img_data->width;	/* image width */
	image->height  = img_data->height;	/* image height */
	image->swidth  = img_data->width;	/* original image width */
	image->sheight = img_data->height;	/* original image height */
	image->bg      = img_data->bg;		/* save background pixel index */
	image->options = options;			/* set XmImageInfo options */
	image->colorspace = (Byte)img_data->color_class;
	image->transparency = img_data->bg != -1 ?
		XmIMAGE_TRANSPARENCY_BG : XmHTML_NONE;

	cnt = 1;
	cptr = image->clip;
	ptr = image->data;

	/*
	* Fill array of used pixel indexes. If we have a background
	* pixel to substitute, save the pixel that should be substituted
	* and create the XYBitmap data to be used as a clip mask.
	*/
	clip_valid = False;
	for(i = 0; i < image->height; i++)
	{
		for(j = 0, bcnt = 0; j < image->width; j++)
		{
			if(used[(int)*ptr] == 0)
			{
				used[(int)*ptr] = cnt;
				cnt++;
				/*
				* check for validity of clip data. If we don't have a match
				* it means the image has a useless transparency so we
				* can safely obliterate the clipmask data.
				*/
				if(*ptr == image->bg)
					clip_valid = True;
			}
			if(do_clip)
			{
				if(*ptr != image->bg)
					*cptr += bitmap_bits[(bcnt % 8)];
				if((bcnt % 8) == 7 || j == (image->width-1))
					cptr++;
				bcnt++;
			}
			ptr++;
		}
	}
	cnt--;

	/* sanity */
	if(cnt > img_data->cmapsize)
	{
		_XmHTMLWarning(__WFUNC__(w, "imageDefaultProc"),
			XMHTML_MSG_65, cnt, img_data->cmapsize);
		cnt = img_data->cmapsize;
	}

	/* store number of colors */
	image->ncolors = image->scolors = cnt;
	_XmHTMLDebug(6, ("images.c: ImageDefaultProc, counted %i colors in "
		"image.\n", cnt));

	/*
	* erase clipmask if we didn't find a matching pixel while there is
	* supposed to be one.
	*/
	if(do_clip && !clip_valid)
	{
		image->bg = -1;
		do_clip = 0;
		free(image->clip);
		image->clip = 0;
		image->options &= ~XmIMAGE_CLIPMASK;
		image->transparency = XmIMAGE_NONE;
		_XmHTMLDebug(6, ("images.c: ImageDefaultProc, useless transparency: "
			"no matching pixel found, ignoring clip data\n"));
	}

	/*****
	* We must perform dithering (or quantization) *after* we've created a
	* (possible) clipmask. Both operations will nuke the transparent pixel...
	*****/
	if(dither || (max_colors && cnt > max_colors))
	{
		/*****
		* Upon return, image has been dithered/quantized and cmap contains a
		* new colormap
		*****/
		if(dither)
			_XmHTMLDitherImage((XmHTMLWidget)w, img_data);
		else
			_XmHTMLQuantizeImage(img_data, max_colors);

		image->data = img_data->data;

		/* need to get a new used array */
		memset(&used, 0, XmHTML_MAX_IMAGE_COLORS*sizeof(int));

		cnt = 1;
		ptr = image->data;

		for(i = 0; i < image->height; i++)
		{
			for(j = 0; j < image->width; j++)
			{
				if(used[(int)*ptr] == 0)
				{
					used[(int)*ptr] = cnt;
					cnt++;
				}
				ptr++;
			}
		}
		cnt--;
		/* store number of colors */
		image->ncolors = cnt;

		_XmHTMLDebug(6, ("images.c: ImageDefaultProc, image %s "
			"from %i to %i colors\n", dither ? "dithered" : "quantized",
			image->scolors, image->ncolors));

		/*****
		* If we have a bg color substitution, quantization messed it up.
		* Restore by comparing image and clipmask and pick up the index of
		* the new bg pixel. It's given by the first bit in a clipmask which
		* is zero.
		*****/
		if(do_clip == 2)
		{
			int k;
			cptr = image->clip;
			ptr = image->data;

			image->bg = -1; /* original bg no longer valid */

			for(i = 0; i < clipsize && image->bg == -1; i++, cptr++)
			{
				for(k = 0; k < 8; k++)
				{
					if(!(*cptr & bitmap_bits[k]))
					{
						image->bg = (int)*ptr;
						break;
					}
					ptr++;
				}
			}
			/* clipmask no longer needed, free it */
			free(image->clip);
			image->clip = 0;
			do_clip = 0;

			_XmHTMLDebug(6, ("images.c: ImageDefaultProc, new background "
				"pixel index: %i\n", image->bg));

			/* restore background color in colormap */
			if(image->bg)
			{
				GETR(img_data->cmap[img_data->bg]) = bg_red;
				GETG(img_data->cmap[img_data->bg]) = bg_green;
				GETB(img_data->cmap[img_data->bg]) = bg_blue;
			}
		}
	}
	/* remove saved clipbitmap if it hasn't been used */
	if(do_clip == 2)
	{
		free(image->clip);
		image->clip = 0;
		image->options &= ~XmIMAGE_CLIPMASK;
		image->transparency = XmIMAGE_NONE;
	}

	/* allocate image RGB values */
	image->reds   = (Dimension*)calloc(3*cnt,sizeof(Dimension));
	image->greens = image->reds + cnt;
	image->blues  = image->greens + cnt;

	/* now go and fill the RGB arrays */
	for(i = 0; i < XmHTML_MAX_IMAGE_COLORS; i++)
	{
		int indx;

		if(used[i] != 0)
		{
			indx = used[i] - 1;
			image->reds[indx]   = GETR(img_data->cmap[i]);
			image->greens[indx] = GETG(img_data->cmap[i]);
			image->blues[indx]  = GETB(img_data->cmap[i]);
			/*
			* Replace transparent color with background RGB values if
			* it has been requested.
			*/
			if(do_clip == 2 && image->bg >= 0 && i == image->bg)
			{
				image->reds[indx]   = bg_red;
				image->greens[indx] = bg_green;
				image->blues[indx]  = bg_blue;
			}
		}
	}

	/*
	* Final transition step to ZPixmap format
	*/
	ptr = image->data;
	for(i = 0; i < size ; i++)
	{
		*ptr = (Byte)(used[(int)*ptr] - 1);
		ptr++;
	}

	/* release raw image colormap */
	free(img_data->cmap);

	/* return loaded image data */
	return(image);
}

/*****
* Name: 		animDefaultProc
* Return Type: 	XmImageInfo*
* Description: 	XmHTML default animation loading procedure
* In:
*	w:			Widget ID;
*	img_data:	raw image data;
*	master:		master animation data with global colormap;
*	global..:	global colormap pixel index array;
*	return..:	when True, fill the global_used array;
*	url:		full name and location of image to load. Only valid for the
*				first frame in an animation.
* Returns:
*	An XmImageInfo structure on success or NULL on failure
*****/
static XmImageInfo*
animDefaultProc(Widget w, XmHTMLRawImageData *img_data,
	XmHTMLRawImageData *master, int *global_used, int *global_used_size,
	Boolean return_global_used, String url)
{
	XmImageInfo *image = NULL;
	int	i, j, cnt, bcnt, size, used[XmHTML_MAX_IMAGE_COLORS], clipsize = 0;
	int do_clip = 0, max_colors = 0, gcolors;
	Byte *ptr, *cptr, *clip = NULL;
	Boolean use_local_cmap = False, dither = False;
	ToolkitAbstraction *tka = NULL;

	/* default XmImageInfo flags */
	int options = XmIMAGE_DEFERRED_FREE|XmIMAGE_RGB_SINGLE|XmIMAGE_ALLOW_SCALE;

	if(XmIsHTML(w))
	{
		max_colors = ((XmHTMLWidget)w)->html.max_image_colors;
		dither = ((XmHTMLWidget)w)->html.map_to_palette != XmDISABLED;
		tka = ((XmHTMLWidget)w)->html.tka;
	}
	else
	{
		if(_xmimage_cfg == NULL)
			return(NULL);

		/* check quantization stuff */
		if(_xmimage_cfg->flags && ImageQuantize)
			max_colors = _xmimage_cfg->ncolors;
		/* and verify no of colors */
		max_colors = getMaxColors(w, max_colors);
		tka = _xmimage_cfg->tka;
	}

	/* are we to use a local colormap ? */
	use_local_cmap = (img_data->cmapsize != 0);

	_XmHTMLDebug(6, ("images.c, animDefaultProc, use local colormap : %s\n",
		use_local_cmap ? "Yes" : "No"));

	/* raw data size */
	size = img_data->width * img_data->height;

	/* pick up the background pixel and corresponding RGB values */
	if(img_data->bg >= 0)
	{
		options |= XmIMAGE_CLIPMASK;
		do_clip = 1;

		/* allocate clipmask data */
		i = img_data->width;
		while((i % 8))
			i++;
		i /= 8;
		clipsize = i * img_data->height;
		clip = (Byte*)calloc(clipsize, sizeof(Byte));
	}

	/* allocate image */
	image = (XmImageInfo*)malloc(sizeof(XmImageInfo));
	(void)memset(image, 0, sizeof(XmImageInfo));

	/* Fill in appropriate fields */
	if(url)	/* only first frame has this */
		image->url = strdup(url);
	image->data    = img_data->data;	/* image data bits */
	image->clip    = clip;				/* clipbitmap bits */
	image->type    = img_data->type;	/* image type */
	image->width   = img_data->width;	/* image width */
	image->height  = img_data->height;	/* image height */
	image->swidth  = img_data->width;	/* original image width */
	image->sheight = img_data->height;	/* original image height */
	image->bg      = img_data->bg;		/* save background pixel index */
	image->options = options;			/* set XmImageInfo options */
	image->colorspace = (Byte)img_data->color_class;
	image->transparency = img_data->bg != -1 ? XmIMAGE_TRANSPARENCY_BG:XmNONE;

	/*****
	* Fill array of used pixel indices.
	* gcolors is a count of the colors really used by this frame, whether
	* or not we should use a global colormap. We must make the distinction
	* if we want dithering to be done correctly for each frame.
	*****/
	if(use_local_cmap || return_global_used)
	{
		memset(&used, 0, XmHTML_MAX_IMAGE_COLORS*sizeof(int));
		cnt = 1;
		ptr = image->data;
		gcolors = 1;
		for(i = 0; i < size; i++, ptr++)
		{
			if(used[(int)*ptr] == 0)
			{
				used[(int)*ptr] = cnt++;
				gcolors++;
			}
		}
		cnt--;
		gcolors--;
	}
	else
	{
		/* gused is the array of colors really being used */
		int gused[XmHTML_MAX_IMAGE_COLORS];
		memcpy(&used, global_used, XmHTML_MAX_IMAGE_COLORS*sizeof(int));
		memset(&gused, 0, XmHTML_MAX_IMAGE_COLORS*sizeof(int));
		cnt = *global_used_size;
		ptr = image->data;
		gcolors = 1;
		for(i = 0; i < size; i++, ptr++)
		{
			if(used[(int)*ptr] == 0)
				used[(int)*ptr] = cnt++;		/* update running counter */
			if(gused[(int)*ptr] == 0)
				gused[(int)*ptr] = gcolors++;
		}
		cnt--;
		gcolors--;
	}

	_XmHTMLDebug(6, ("images.c, animDefaultProc, counted %i colors.\n",
		gcolors));

	/* store number of colors if we are using a local colormap */
	if(use_local_cmap)
	{
		if(cnt > img_data->cmapsize)
			cnt = img_data->cmapsize;
		image->ncolors = image->scolors = cnt;
	}
	/* only update global array when we aren't dithering */
	else if(return_global_used || *global_used_size < cnt)
	{
		/* only update if we don't have to quantize this image */
		if(return_global_used || !(max_colors && gcolors > max_colors))
		{
			_XmHTMLDebug(6, ("images.c, animDefaultProc, updating "
				"global_used array (old size: %i, new size: %i)\n",
				*global_used_size, cnt));

			for(i = 0; i < XmHTML_MAX_IMAGE_COLORS; i++)
				if(global_used[i] == 0)
					global_used[i] = used[i];
		}
		if(return_global_used)
			image->ncolors = image->scolors = gcolors;
	}

	/* check clipmask stuff */
	if(do_clip)
	{
		cptr = image->clip;
		ptr = image->data;
		for(i = 0; i < image->height; i++)
		{
			for(j = 0, bcnt = 0; j < image->width; j++)
			{
				if(*ptr != image->bg)
					*cptr += bitmap_bits[(bcnt % 8)];
				if((bcnt % 8) == 7 || j == (image->width-1))
					cptr++;
				bcnt++;
				ptr++;
			}
		}
	}

	/* dither/quantize if necessary */
	if(dither || (max_colors && gcolors > max_colors))
	{
		_XmHTMLDebug(6, ("images.c, animDefaultProc, %s from %i to "
			"%i colors max\n", dither ? "dithering" : "quantizing", gcolors,
			max_colors));

		/* if we don't have a local colormap, copy the global one */
		if(!use_local_cmap)
		{
			AllocRawImageCmap(img_data, master->cmapsize);
			/* copy it */
			memcpy(img_data->cmap, master->cmap,
				master->cmapsize * sizeof(XCOLOR));
			/* forces use of the local colormap */
			use_local_cmap = True;
		}

		/* do it */
		if(dither)
			_XmHTMLDitherImage((XmHTMLWidget)w, img_data);
		else
			_XmHTMLQuantizeImage(img_data, max_colors);

		image->data = img_data->data;

		/* need to get a new used array */
		memset(&used, 0, XmHTML_MAX_IMAGE_COLORS*sizeof(int));

		cnt = 1;
		ptr = image->data;

		/* compose it */
		for(i = 0; i < image->height; i++)
		{
			for(j = 0; j < image->width; j++)
			{
				if(used[(int)*ptr] == 0)
				{
					used[(int)*ptr] = cnt;
					cnt++;
				}
				ptr++;
			}
		}
		cnt--;
		/* store number of colors */
		image->ncolors = cnt;
	}

	/*****
	* Allocate image RGB values if we have a local colormap or when we
	* have to return it (for the first frame in this animation)
	*****/
	if(use_local_cmap)
	{
		image->reds   = (Dimension*)calloc(3*cnt,sizeof(Dimension));
		image->greens = image->reds + cnt;
		image->blues  = image->greens + cnt;

		/* now go and fill the RGB arrays */
		for(i = 0; i < XmHTML_MAX_IMAGE_COLORS; i++)
		{
			int indx;
			if(used[i] != 0)
			{
				indx = used[i] - 1;
				image->reds[indx]   = GETR(img_data->cmap[i]);
				image->greens[indx] = GETG(img_data->cmap[i]);
				image->blues[indx]  = GETB(img_data->cmap[i]);
			}
		}
		/* can happen when we have quantized the first frame in an animation */
		if(return_global_used)
		{
			memcpy(global_used, &used, XmHTML_MAX_IMAGE_COLORS*sizeof(int));
			*global_used_size = cnt;
			image->ncolors = cnt;
			/*****
			* Quantization made a copy of the master colormap. As this is
			* the first frame in this animation, we must free the copy here
			* or it won't get freed at all.
			*****/
			if(img_data->cmapsize)
				free(img_data->cmap);
			img_data->cmapsize = 0;
		}
	}
	else if(return_global_used || *global_used_size < cnt)
	{
		_XmHTMLDebug(6, ("images.c, animDefaultProc, updating global "
			"colormap\n"));

		image->reds   = (Dimension*)calloc(3*cnt,sizeof(Dimension));
		image->greens = image->reds + cnt;
		image->blues  = image->greens + cnt;

		/* now go and fill the RGB arrays */
		for(i = 0; i < XmHTML_MAX_IMAGE_COLORS; i++)
		{
			int indx;

			if(global_used[i] != 0)
			{
				indx = global_used[i] - 1;
				image->reds[indx]   = GETR(master->cmap[i]);
				image->greens[indx] = GETG(master->cmap[i]);
				image->blues[indx]  = GETB(master->cmap[i]);
			}
		}
		*global_used_size = cnt;
	}

	/* Final transition step to ZPixmap format */
	ptr = image->data;
	if(use_local_cmap)
	{
		for(i = 0; i < size ; i++)
		{
			*ptr = (Byte)(used[(int)*ptr] - 1);
			ptr++;
		}
	}
	else
	{
		for(i = 0; i < size ; i++)
		{
			*ptr = (Byte)(global_used[(int)*ptr] - 1);
			ptr++;
		}
	}

	/* release local colormap */
	if(img_data->cmapsize && !return_global_used)
	{
		free(img_data->cmap);
		img_data->cmapsize = 0;
	}

	/* return loaded image data */
	return(image);
}

/*****
* Name: 		imageDelayedProc
* Return Type: 	XmImageInfo*
* Description: 	creates an empty XmImageInfo structure required for delayed
*				image creation (e.i., images with an alpha channel).
* In:
*	w:			Widget ID;
*	img_data:	raw image data;
*	ib:			current ImageBuffer, contains unprocessed image data.
* Returns:
*	An XmImageInfo structure on success or NULL on failure
*****/
static XmImageInfo*
imageDelayedProc(Widget w, XmHTMLRawImageData *img_data, ImageBuffer *ib)
{
	static XmImageInfo *image = NULL;

	/* allocate an image */
	image = (XmImageInfo*)malloc(sizeof(XmImageInfo));

	/* initialize to zero */
	(void)memset(image, 0, sizeof(XmImageInfo));

	/* store image location and type of this image */
	image->url  = strdup(ib->file);
	image->type = ib->type;
	image->depth   = ib->depth;
	image->width   = img_data->width;		/* image width,  REQUIRED!!! */
	image->height  = img_data->height;		/* image height, REQUIRED!!! */
	image->swidth  = img_data->width;
	image->sheight = img_data->height;
	image->ncolors = image->scolors = img_data->cmapsize;

	image->bg = -1;
	image->transparency = XmIMAGE_TRANSPARENCY_ALPHA;
	image->colorspace   = img_data->color_class;
	image->options      = XmIMAGE_DELAYED_CREATION | XmIMAGE_ALLOW_SCALE;
	image->fg_gamma     = img_data->fg_gamma;
	image->alpha        = img_data->data;

	/* return loaded image data */
	return(image);
}

/*****
* Name: 		_XmHTMLInfoToPixmap
* Return Type: 	Boolean
* Description: 	creates a pixmap from the given image data
* In:
*	html:		XmHTMLWidget
*	image:		raw image data
*	width:		requested image width
*	height:		requested image height
*	clip:		clipmask, updated upon return.
* Returns:
*	A pixmap upon success, None otherwise.
*****/
PIXMAP
_XmHTMLInfoToPixmap(XmHTMLWidget html, XmHTMLImage *image,
	XmImageInfo *info, Dimension width, Dimension height,
	unsigned long *global_cmap, PIXMAP *clip)
{
	COLORMAP cmap;
	WINDOW win;
	XIMAGE *ximage = NULL;
	static PIXMAP pixmap;
	XCC xcc;
	ToolkitAbstraction *tka = NULL;
	unsigned long *color_map;
	/* store original image data in case we're scaling */
	Byte *orig_image_data = info->data;
	Boolean scaled = False;

	*clip = None;

	if(XmIsHTML((Widget)html))
		tka = html->html.tka;
	else if(_xmimage_cfg != NULL)
		tka = _xmimage_cfg->tka;
	else
		return(None);

	/*****
	* external images are only scaled if the dimensions specified in the
	* <IMG> attributes differ from the real image dimensions.
	* The XmIMAGE_ALLOW_SCALE bit must also be set (true by default).
	* When we need to scale, we use a *copy* of the original image data.
	* Using the original data itself will reduce the quality of the image
	* when the same image is reused without scaling.
	*****/
	if(!(ImageIsInternal(image)) && ImageInfoScale(info) &&
		(height != info->sheight || width != info->swidth))
	{
		info->data = scaleImage(info, width, height);
		image->height = height;
		image->width = width;
		scaled = True;
	}
	else
	{
		/*****
		* Promote real image dimensions to current image dimensions.
		* They might be different if this image was scaled on a
		* previous run.
		*****/
		info->height = info->sheight;
		info->width  = info->swidth;
	}

	/*
	* get a window
	* if we are realized we have a window
	*/
	if(tka->IsRealized((Widget)html) && tka->win != None)
		win = tka->win;
	else
		win = tka->defaultRoot;

	/* get colormap: every widget is derived from core so this is easy */
	cmap = TkaGetColormap(html);

	/*
	* Set XCC for this image.
	* When we are creating images that have nothing to do with XmHTML,
	* the image already has a privatly owned XCC.
	*/
	if((xcc = image->xcc) == NULL)
	{
		if(XmIsHTML((Widget)html))
		{
			if(!html->html.xcc)
				_XmHTMLCheckXCC(html);
			image->xcc = html->html.xcc;
		}
	}

	/* allocate colors used by the image and create it */
	if(global_cmap)
	{
		color_map = global_cmap;
		_XmHTMLDebug(6, ("image.c: _XmHTMLInfoToPixmap, using global "
			"colormap.\n"));
	}
	else
	{
		_XmHTMLDebug(6, ("image.c: _XmHTMLInfoToPixmap, allocating local "
			"colormap with %i entries\n", info->ncolors));
		color_map = makeColormap(html, image, info);
	}

	if((ximage = _XmHTMLCreateXImage(html, xcc, info->width,
		info->height, image->url)) != NULL)
		_XmHTMLFillXImage(html, ximage, xcc, info->data, color_map, NULL, NULL);

	/* restore original image data if we've scaled it */
	if(scaled)
	{
		free(info->data);
		info->data = orig_image_data;
	}

	if(color_map != global_cmap)
		free(color_map);

	if(ximage != NULL)
	{
		XGC gc;

		/* create the pixmap */
		if((pixmap = tka->CreatePixmap(tka->dpy, win, info->width,
			info->height, image->xcc->visualInfo->depth)) == None)
		{
			_XmHTMLWarning(__WFUNC__(html, "_XmHTMLInfoToPixmap"),
				XMHTML_MSG_66, image->url ? image->url : "(animation frame)");
			tka->DestroyImage(ximage);
			return(None);
		}
		/* copy the image into the pixmap */
		gc = tka->CreateGC(tka->dpy, pixmap, 0, 0);
		tka->SetFunction(tka->dpy, gc, GXcopy);
		tka->PutImage(tka->dpy, pixmap, gc, ximage, 0, 0, 0, 0, info->width,
			info->height);
		tka->FreeGC(tka->dpy, gc);
		tka->DestroyImage(ximage);

		/* Create a clip mask if are to use one */
		if(ImageInfoClipmask(info))
		{
			_XmHTMLDebug(6, ("images.c, _XmHTMLInfoToPixmap, "
				"creating clip mask for %s\n", image->url));

			*clip = tka->CreatePixmapFromBitmapData(tka->dpy, win,
				(char*)info->clip, info->width, info->height, 1, 0, 1);
#ifdef DEBUG
			/* write out this bitmap */
			if(XmIsHTML((Widget)html) && html->html.debug_save_clipmasks)
			{
				char xbm[1024];
				static int num;
				int i;

				if(strstr(image->url, "/"))
				{
					for(i = strlen(image->url) - 1;
						i > 0 && image->url[i] != '/'; i--);
					sprintf(xbm, "%s.%i.xbm", &image->url[i+1], num);
				}
				else
					sprintf(xbm, "%s.%i.xbm", image->url, num);
				XWriteBitmapFile(tka->dpy, xbm, *clip, info->width,
					info->height, 0, 0);
				fprintf(stderr, "Wrote clipping bitmap to file %s\n", xbm);
				num++;
			}
#endif
		}
		/* return the pixmap */
		return(pixmap);
	}
	else
	{
		/* failed, can't create this image, it might be too large? */
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLInfoToPixmap"),
			XMHTML_MSG_67, image->url ? image->url : "(animation frame)");
		return(None);
	}
}

/*****
* Name: 		_XmHTMLMakeAnimation
* Return Type: 	void
* Description: 	creates a series of image that form an animation
* In:
*	html:		XmHTMLWidget id
*	image:		animation data, updated upon return
*	width:		logical screen width
*	height:		logical screen height
* Returns:
*	nothing, but image contains a series of pixmaps forming the animation.
*****/
void
_XmHTMLMakeAnimation(XmHTMLWidget html, XmHTMLImage *image, Dimension width,
	Dimension height)
{
	PIXMAP pixmap, clip;
	int i = 0, nframes = image->html_image->nframes;
	Dimension w = width, h = height;
	XmImageInfo *frame = image->html_image;
	float width_p = 0., height_p = 0.;
	unsigned long *global_cmap;

	_XmHTMLDebug(6, ("images.c: _XmHTMLMakeAnimation Start, creating "
		"animation %s\n", image->url));

	image->options |= IMG_ISANIM;
	image->nframes  = nframes;
	image->frames   = (XmImageFrame*)calloc(nframes, sizeof(XmImageFrame));
	image->html     = html;
	image->context  = XtWidgetToApplicationContext((Widget)html);
	image->current_frame = 0;

	/*****
	* Animations can also be scaled. To do this, we scale each frame
	* with the scale factor that needs to be applied to the logical screen
	* area. The logical screen area is given by the width and height of the
	* first frame.
	*****/
	height_p = (float)(height/(float)frame->height);
	width_p  = (float)(width/(float)frame->width);

	_XmHTMLDebug(6, ("images.c: _XmHTMLMakeAnimation, global scaling factor:\n"
		"\thorizontal: %f\n\tvertical: %f\n", height_p, width_p));

	/* global colormap */
	global_cmap = makeColormap(html, image, frame);

	while(frame && i < nframes)
	{
		_XmHTMLDebug(6, ("images.c: _XmHTMLMakeAnimation, creating frame "
			"%i\n", i));

		/* use current frame dimensions instead of global image dimensions */
		if(!(frame->options & XmIMAGE_FRAME_IGNORE))
		{
			w = (int)(width_p*frame->width);
			h = (int)(height_p*frame->height);

			/*****
			* If the dimensions of this frame differ from the logical screen
			* dimensions or a frame has a disposal method other than none, we
			* run the animation on an internal pixmap and blit this pixmap to
			* screen when all required processing for the current frame has
			* been done. This is an enormous performance enhancment since we
			* only need to do one screen update.
			* Animations of which each frame has the same size and a disposal
			* method of none are blit to screen directly as they require no
			* processing whatsoever.
			*****/
			if((w != width || h != height ||
				frame->dispose != XmIMAGE_DISPOSE_NONE) &&
				!ImageHasState(image))
				image->options |= IMG_HASSTATE;

			/*****
			* Only use local colormap if we've got one.
			*****/
			if((pixmap = _XmHTMLInfoToPixmap(html, image, frame, w, h,
					i && frame->ncolors ? NULL : global_cmap, &clip)) == None)
			{
				image->html_image->nframes = i;
				return;
			}
			image->frames[i].pixmap = pixmap;
			image->frames[i].clip = clip;
		}
		else
			image->frames[i].pixmap = None;

		image->frames[i].x = (int)(width_p*(float)frame->x);
		image->frames[i].y = (int)(height_p*(float)frame->y);
		image->frames[i].w = w;
		image->frames[i].h = h;
		image->frames[i].dispose = frame->dispose;
		image->npixels = 0;

		/* adjust animation timeout if it's too small */
		image->frames[i].timeout = (frame->timeout ? frame->timeout : 50);

		/* move to next animation frame */
		frame = frame->frame;
		i++;
	}
	/* no longer needed */
	free(global_cmap);

	/*
	* Fallback images when loop_count is used or animations are frozen
	* The XmIsHTML test is required since these routines can also be used
	* for creating images that are not part of XmHTML. These images don't
	* have an animation state maintainer.
	*/
	if(XmIsHTML((Widget)html) && ImageHasState(image))
	{
		ToolkitAbstraction *tka = html->html.tka;
		WINDOW win = (HTML_ATTR(gc) == NULL ? tka->defaultRoot : tka->win);

		/* empty pixmap for current animation state */
		image->pixmap = tka->CreatePixmap(tka->dpy, win, width, height,
			TkaVisualGetDepth(html));

		/* no clipmask for the current animation state */

		/* first frame is first state of this animation */
		if(html->html.gc != NULL)
			tka->CopyArea(tka->dpy, image->frames[0].pixmap, image->pixmap,
				html->html.gc, 0, 0, width, height, 0, 0);

		_XmHTMLDebug(6, ("images.c: _XmHTMLMakeAnimation, allocated a "
			"state maintainer for this animation (%ix%i)\n", width, height));
	}
	else
	{
		/* fallback images when loop_count is used or animations are frozen */
		image->pixmap = image->frames[0].pixmap;
		image->clip = image->frames[0].clip;
	}
#ifdef DEBUG
	if(XmIsHTML((Widget)html) && html->html.debug_no_loopcount)
		image->loop_count = 0;
	else
#endif
		image->loop_count = image->html_image->loop_count;
	image->current_loop = 0;
	image->current_frame = 0;
	/* this will initialize looping in DrawImage, paint.c */
	image->options |= IMG_FRAMEREFRESH;
	_XmHTMLDebug(6, ("images.c: _XmHTMLMakeAnimation End\n"));
}

static XmImageInfo*
readGifAnimation(Widget w, ImageBuffer *ib)
{
	int	width, height, x, y, screen_width, screen_height, dispose;
	static XmImageInfo *all_frames;
	XmImageInfo *frame;
	int nframes = 0, loop_count = 0, timeout = 50, bg;
	/* fallbacks if no dispose or timeout method is specified */
	int fallback_dispose = XmIMAGE_DISPOSE_NONE, fallback_timeout = 50;
	static XmHTMLRawImageData img_data, master;
	int global_used[XmHTML_MAX_IMAGE_COLORS], global_size = 0;

	_XmHTMLDebug(6, ("images.c, readGifAnimation, %s is an animated "
		"gif\n", ib->file));

	all_frames = frame = NULL;

	memset(&global_used, 0, XmHTML_MAX_IMAGE_COLORS*sizeof(int));

	/* initialize gif animation reading */
	if((loop_count = _XmHTMLGifAnimInit(w, ib, &master)) == -1)
		return(NULL);

	screen_height = master.height;
	screen_width  = master.width;
	bg = master.bg;

	/* ignore loop count if selected */
#ifdef DEBUG
	if(XmIsHTML(w))
	{
		XmHTMLWidget html = (XmHTMLWidget)w;
		if(HTML_ATTR(debug_no_loopcount))
			loop_count = 0;
	}
#endif

	/* keep reading frames until we run out of them */
	while(_XmHTMLGifAnimNextFrame(ib, &img_data, &x, &y,
		&timeout, &dispose))
	{
		/* Go and create each frame. */
		if(nframes)
		{
			int cnt = global_size;
			frame->frame = animDefaultProc(w, &img_data, &master,
							&global_used[0], &cnt, False, NULL);
			frame = frame->frame;

			/*****
			* Check if the global colormap has been modified.
			* We may *never* do this for images that have been quantized!
			*****/
			if(global_size < cnt && frame->ncolors == frame->scolors)
			{
				/* release previous colormap */
				free(all_frames->reds);

				/* store new cmap */
				global_size = cnt;
				all_frames->reds = frame->reds;
				all_frames->greens = frame->greens;
				all_frames->blues  = frame->blues;
				all_frames->ncolors = all_frames->scolors = global_size;

				/* wipe colormap for this frame, it uses the global one */
				frame->reds = frame->greens = frame->blues = (Dimension*)NULL;
				frame->ncolors = frame->scolors = 0;
			}
		}
		else
		{
			/* this is the first frame with the master colormap */
			all_frames = animDefaultProc(w, &img_data, &master,
							&global_used[0], &global_size, True, ib->file);
			frame = all_frames;
		}

		if(dispose)
			fallback_dispose = dispose;
		if(timeout)
			fallback_timeout = timeout;
		/* save info for this frame */
		frame->depth = ib->depth;
		frame->x = x;
		frame->y = y;
		frame->width  = width  = img_data.width;
		frame->height = height = img_data.height;
		frame->dispose = fallback_dispose;
		frame->timeout = fallback_timeout;
		/*
		* reset dispose to none if the dimensions of this frame equal
		* the screen dimensions *and* if this image isn't transparent.
		* This will give us a small performance enhancement since the
		* frame refreshing routines don't have to restore the background
		* or blit the previous screen state to the screen.
		*/
		if(width == screen_width && height == screen_height && x == 0 &&
			 y == 0 && bg == -1)
			frame->dispose = XmIMAGE_DISPOSE_NONE;
		/*
		* final check before moving to the next frame: if this frame doesn't
		* fit on the logical screen, clip the parts that lie outside the
		* logical screen area. Images that fall completely outside the
		* logical screen are just left the way they are.
		*/
		if(x + width > screen_width || y + height > screen_height)
		{
			int new_w = x + width > screen_width ? screen_width - x : width;
			int new_h = y + height > screen_height ? screen_height - y : height;
			if(x < screen_width && y < screen_height)
				clipImage(frame, new_w, new_h);
			else
				frame->options |= XmIMAGE_FRAME_IGNORE;
		}
		nframes++;
	}
	/* free global colormap */
	free(master.cmap);

	/* terminate gif animation reading */
	_XmHTMLGifAnimTerminate(ib);

	if(all_frames)
	{
		_XmHTMLDebug(6, ("images.c, readGifAnimation, loaded animation %s, "
			"no of frames: %i, loop_count: %i\n", ib->file, nframes,
			loop_count));
		all_frames->loop_count = loop_count;
		/* nframes is total no of frames in this animation */
		all_frames->nframes = nframes;
	}
	return(all_frames);
}

/*****
* Name: 		_XmHTMLImageFileToBuffer
* Return Type: 	ImageBuffer*
* Description: 	loads a file into a memory buffer
* In:
*	file:		file to load
* Returns:
*	filled ImageBuffer.
*****/
ImageBuffer*
_XmHTMLImageFileToBuffer(String file)
{
	FILE *fp;
	static ImageBuffer *ib;
	int size;

	ib = NULL;

	if((fp = fopen(file, "r")) == NULL)
	{
		perror(file);
		return(NULL);
	}

	fseek(fp, 0L, SEEK_END);
	size = ftell(fp);

	/* sanity check */
	if(size == 0)
		return(NULL);

	rewind(fp);

	ib = (ImageBuffer*)malloc(sizeof(ImageBuffer));

	ib->buffer = (Byte*)malloc((size+1)*sizeof(Byte));
	ib->size = size;

	if((fread(ib->buffer, ib->size, 1, fp)) != 1)
	{
		perror(file);
		fclose(fp);
		free(ib->buffer);
		free(ib);
		return(NULL);
	}
	fclose(fp);

	/* sanity, we deliver properly null terminated buffers */
	ib->buffer[ib->size] = '\0';

	ib->file = strdup(file);
	ib->curr_pos = ib->buffer;
	ib->next = 0;
	ib->may_free = True;

	return(ib);
}

static XmHTMLImage*
copyHTMLImage(XmHTMLWidget html, XmHTMLImage *image, String attributes)
{
	static XmHTMLImage *dest;
	XmHTMLImage *tmp;

	/*****
	* If creation for this image should be delayed until it's needed, set
	* the global creation flag.
	*****/
	if(ImageDelayedCreation(image))
		html->html.delayed_creation = True;

	/*****
	* If this is an orphaned image, it is currently not being used
	* and can thus be used without copying it or inserting it in the
	* list (images are orphaned when a resource is changed that does not
	* require a reload of images itself).
	*****/
	if(ImageIsOrphaned(image))
	{
		_XmHTMLDebug(6, ("images.c: copyHTMLImage, found an orphaned "
			"image: %s!\n", image->url));
		image->options &= ~IMG_ORPHANED;
		image->context = XtWidgetToApplicationContext((Widget)html);
		image->html    = html;
		return(image);
	}
	dest = copyImage(image, attributes);

	/* These are unique to an image. */
	dest->context = XtWidgetToApplicationContext((Widget)html);
	dest->html    = html;

	_XmHTMLDebug(6, ("images.c: copyHTMLImage, image %s copied, "
		"dimensions: %ix%i\n", dest->html_image->url, dest->swidth,
		dest->sheight));

	/* store in the image list */
	addImageToList(html, dest);

	/* add it to the child list of the parent image */
	if(image->child == NULL)
		image->child = dest;
	else
	{
		for(tmp = image->child; tmp != NULL && tmp->child != NULL;
			tmp = tmp->child);
		tmp->child = dest;
	}
	return(dest);
}

/*****
* Name:			initAlphaChannels
* Return Type: 	void
* Description: 	initializes alpha channel processing: obtains background
*				color/image information which will be merged with
*				alpha-channeled images.
* In:
*	html:		XmHTMLWidget id;
*	for_body..:	True when we should initialize for body image processing.
* Returns:
*	nothing.
*****/
static void
initAlphaChannels(XmHTMLWidget html, Boolean for_body_image)
{
	AlphaPtr alpha;
	ToolkitAbstraction *tka = html->html.tka;

	/*****
	* Always (re-)initialize the AlphaChannel buffer. If the body image
	* is an alpha-channeled image we must use the current background *color*
	* for it, and for all subsequent images we need to use the colormap of
	* the background *image*.
	*****/
	if(!html->html.alpha_buffer)
		html->html.alpha_buffer = (AlphaPtr)malloc(sizeof(AlphaChannelInfo));
	else if(html->html.alpha_buffer->ncolors)
		free(html->html.alpha_buffer->bg_cmap);

	alpha = html->html.alpha_buffer;
	alpha->bg_cmap = (XCOLOR*)NULL;
	alpha->ncolors = 0;
	alpha->fb_maxsample = (1 << TkaVisualGetDepth(html)) - 1;

	/* no body image or this *is* the body image, use background color */
	if(html->html.body_image == NULL || for_body_image)
	{
		XCOLOR bg_color;

		/* current background color */
		GETP(bg_color) = html->html.body_bg;

		/* get rgb components */
		tka->QueryColor(tka->dpy, TkaGetColormap(html), &bg_color);

		/* downscale to range 0-255, required for alpha channel processing */
		alpha->background[0] = GETR(bg_color) >> 8;
		alpha->background[1] = GETG(bg_color) >> 8;
		alpha->background[2] = GETB(bg_color) >> 8;
	}
	else
	{
		int i;
		unsigned long *color_map;
		XmImageInfo *bg_image = html->html.body_image->html_image;

		/* allocate color_map pixel entries */
		color_map = (unsigned long*)calloc(bg_image->ncolors,
			sizeof(unsigned long));

		/*****
		* Note:
		* Due to the nature/smartness of the XCC, no color allocation will be
		* performed when we make the call below: the body image is already
		* loaded and the colors it uses have been allocated.
		* XCCGetPixels either has a color lookup table or a hashtable
		* of already allocated colors (depending on the current visual, but
		* XCC takes care of all that), and since the body colors have
		* been allocated by the time we make this call, we will simply
		* get the pixels corresponding to already allocated colors.
		*
		* I *love* this, there are some *true* gems in this code of mine!
		*****/
		alpha->ncolors = 0;
		XCCGetPixels(html->html.xcc, bg_image->reds, bg_image->greens,
			bg_image->blues, bg_image->ncolors, color_map,
			&alpha->ncolors);

		/* use all pixel values, not only the allocated ones */
		alpha->ncolors = bg_image->ncolors;

		/* initialize body image colormap */
		alpha->bg_cmap = (XCOLOR*)calloc(alpha->ncolors, sizeof(XCOLOR));
		for(i = 0; i < alpha->ncolors; i++)
			GETP(alpha->bg_cmap[i]) = color_map[i];

		/* no longer needed */
		free(color_map);

		/* get rgb values */
		tka->QueryColors(tka->dpy, TkaGetColormap(html), alpha->bg_cmap,
				alpha->ncolors);

		/* downscale to range 0-255, required for alpha channel processing */
		for(i = 0; i < alpha->ncolors ; i++)
		{
			GETR(alpha->bg_cmap[i]) >>= 8;
			GETG(alpha->bg_cmap[i]) >>= 8;
			GETB(alpha->bg_cmap[i]) >>= 8;
		}
	}
}

/*****
* Name: 		doAlphaChannel
* Return Type: 	void
* Description: 	recreates an image specified by image_word
* In:
*	html:		XmHTMLWidget id;
*	image_word:	image data;
* Returns:
*	nothing, but the image described by image_word contains new pixmaps.
* Note:
*	This routine is only used for PNG images with either a tRNS chunk or an
*	alpha channel. As the actual pixmap contents depend on the location of
*	the image in the document, we need to create a new pixmap every time the
*	document is resized.
*****/
static void
doAlphaChannel(XmHTMLWidget html, XmHTMLImage *image)
{
	XmImageInfo *html_image = image->html_image;
	XmImageInfo *new_info;
	XmHTMLRawImageData *img_data = NULL, raw_data;
	int x = 0, y = 0;
	PIXMAP pixmap, clip;
	unsigned int flags = 0;

	_XmHTMLDebug(6, ("doAlphaChannel, rereading image %s\n", image->url));

	/*****
	* Verify that this image has an owner, e.g., is not the body image.
	* If it has an owner, get its position. If this *is* the body image,
	* alpha channel processing is done against the current background color.
	* (either set thru <body bgcolor=".."> or the default background color)
	*****/
	if(image->owner)
	{
		x = image->owner->words[0].x;
		y = image->owner->words[0].y;
	}

	/* we *require* an XmImageInfo structure */
	if(ImageInfoFreed(image) || html_image == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "doAlphaChannel"),
			XMHTML_MSG_68, image->url, XMHTML_MSG_69);
		image->options &= ~IMG_DELAYED_CREATION;
		return;
	}

	/* sanity */
	if(html_image->type != IMAGE_PNG)
	{
		_XmHTMLWarning(__WFUNC__(html, "doAlphaChannel"),
			XMHTML_MSG_68, image->url, XMHTML_MSG_70);
		image->options &= ~IMG_DELAYED_CREATION;
		return;
	}

	/* more sanity */
	if(!(ImageInfoDelayedCreation(html_image)))
	{
		_XmHTMLWarning(__WFUNC__(html, "doAlphaChannel"),
			XMHTML_MSG_68, image->url, XMHTML_MSG_71);
		image->options &= ~IMG_DELAYED_CREATION;
		return;
	}

#if 0
	if(CHECK_CALLBACK(html, anchor_track_callback, ANCHOR_TRACK))
	{
		char msg[1024];
		XmHTMLAnchorCallbackStruct cbs;
		(void)memset(&cbs, 0, sizeof(XmHTMLAnchorCallbackStruct));

		sprintf(msg, "%s: processing alpha channel", image->url);

		/* initialize callback fields */
		cbs.reason = XmCR_HTML_ANCHORTRACK;
		cbs.event = NULL;

		cbs.url_type = ANCHOR_FILE_LOCAL;	/* doesn't really matter */
		cbs.line     = 0;
		cbs.href     = msg;
		cbs.target   = NULL;
		cbs.rel      = NULL;
		cbs.rev      = NULL;
		cbs.title    = NULL;
		cbs.doit     = False;		/* doesn't matter */
		cbs.visited  = False;		/* doesn't matter */

		XtCallCallbackList((Widget)html, html->html.anchor_track_callback,
			&cbs);

		XXSync(XtDisplay((Widget)html), False);
		XmUpdateDisplay((Widget)html);
	}
#endif

	/* fill in a rawImage structure */
	raw_data.data     = html_image->alpha;
	raw_data.alpha    = (Byte*)NULL;
	raw_data.width    = html_image->swidth;
	raw_data.height   = html_image->sheight;
	raw_data.bg       = -1;
	raw_data.cmap     = (XCOLOR*)NULL;
	raw_data.cmapsize = 0;
	raw_data.type     = html_image->type;
	raw_data.fg_gamma = html_image->fg_gamma;
	raw_data.color_class      = html_image->colorspace;
	raw_data.delayed_creation = True;

	_XmHTMLDebug(6, ("doAlphaChannel, processing alpha channel.\n"));

	img_data = _XmHTMLReReadPNG(html, &raw_data, x, y, image->owner == NULL);

	_XmHTMLDebug(6, ("doAlphaChannel, processing read image data.\n"));

	img_data->type = IMAGE_PNG;
	new_info = imageDefaultProc((Widget)html, img_data, image->url);
	free(img_data);

	/* sanity */
	if(new_info == NULL)
	{
		image->options &= ~IMG_DELAYED_CREATION;
		return;
	}

	/* update the XmImageInfo for this image */
	if(!(ImageInfoShared(html_image)))
	{
		if(html_image->data)
			free(html_image->data);
		if(ImageInfoClipmask(html_image))
			free(html_image->clip);
		if(ImageInfoRGBSingle(html_image))
		{
			if(html_image->reds)
				free(html_image->reds);
		}
		else
		{
			if(html_image->reds)
				free(html_image->reds);
			if(html_image->greens)
				free(html_image->greens);
			if(html_image->blues)
				free(html_image->blues);
		}
	}

	/*****
	* Now copy members of the new ImageInfo structure that are likely to
	* have changed. This is required if we do not want to mess up any of
	* the user's XmImageInfo administration (caching and stuff).
	* scolors is left untouched, unless it is zero (png has a palette size
	* for gray and paletted colorspaces).
	*****/
	html_image->data    = new_info->data;
	html_image->clip    = new_info->clip;
	html_image->reds    = new_info->reds;
	html_image->greens  = new_info->greens;
	html_image->blues   = new_info->blues;
	html_image->depth   = new_info->depth;
	html_image->ncolors = new_info->ncolors;
	html_image->depth   = 8;	/* always */
	if(!html_image->scolors)
		html_image->scolors = new_info->scolors;
	/*****
	* Check what flags we should set. We must check a number of flags
	* that the user might have set himself (such as keeping the image alive).
	* We ignore the delayed, clipmask and progressive flags: when we get
	* here, the image is fully available. And alpha channels don't even
	* begin to think about clipmasks, it's why I've got to deal with this
	* delayed creation mess in the first place!!
	*****/
	if(ImageInfoFreeLater(html_image))
		flags |= XmIMAGE_DEFERRED_FREE;		/* free when doc switches? */
	else if(ImageInfoFreeNow(html_image))
		flags |= XmIMAGE_IMMEDIATE_FREE;	/* free when image created? */
	if(ImageInfoScale(html_image))
		flags |= XmIMAGE_ALLOW_SCALE;		/* may we scale? */
	if(ImageInfoShared(html_image))
		flags |= XmIMAGE_SHARED_DATA;		/* must data be kept alive? */

	/* flags from the imageDefaultProc and of course the delayed creation bit */
	flags |= XmIMAGE_RGB_SINGLE|XmIMAGE_DELAYED_CREATION;

	html_image->options = flags;

	/* and free the new info, we no longer need it */
	free(new_info);

	/* this image has an alpha channel */
	html_image->transparency = XmIMAGE_TRANSPARENCY_ALPHA;

	/* save image type as well */
	html_image->type = raw_data.type;

	_XmHTMLDebug(6, ("doAlphaChannel, creating pixmap.\n"));

	/*****
	* Fourth step is to create the actual pixmap
	* First destroy any previous pixmaps.
	*****/
	freePixmaps(html, image);

	image->html_image = html_image;
	pixmap = _XmHTMLInfoToPixmap(html, image, html_image, image->width,
		image->height, NULL, &clip);

	image->pixmap = pixmap;
	image->clip   = clip;		/* which is always None */

	/*****
	* Unset delayed creation if we have a solid background color: the image
	* does not have to be recreated when this doc is resized.
	* Also unset delayed creation flag for the body image, it does not need
	* reprocessing on document resize.
	*****/
	if(ImageIsBackground(image))
		image->options &= ~IMG_DELAYED_CREATION;
	else if(html->html.body_image)
		image->options |= IMG_DELAYED_CREATION;
	else
		image->options &= ~IMG_DELAYED_CREATION;

	/* and we are done */
	_XmHTMLDebug(6, ("doAlphaChannel, done.\n"));
}

/*****
* Name:			processBodyImage
* Return Type: 	void
* Description:  final body image processing: verifies a few things and plugs
*				in the background color if the body image is transparent.
*				Stores the background image in the widget.
* In:
*	html:		XmHTMLWidget id;
*	body_im..:	actual background image;
*	width:		background image width;
*	height:		background image height;
* Returns:
*	nothing.
*****/
static void
processBodyImage(XmHTMLWidget html, XmHTMLImage *body_image,
	Dimension width, Dimension height)
{
	/* animations are not allowed as background images */
	if(ImageIsAnim(body_image))
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLLoadBodyImage"),
			XMHTML_MSG_72);
		html->html.body_image = NULL;
		return;
	}

	/* mark as the background image */
	body_image->options |= IMG_ISBACKGROUND;

	/*
	* See if the image was loaded succesfully, e.i. is an
	* external image.
	* might seem strange, but this image *will* be freed
	* when XmHTMLFreeAllImages is called (e.i. destroy() is
	* invoked or new text is set).
	*/
	if(ImageIsInternal(body_image))
		html->html.body_image = NULL;
	else
	{
		html->html.body_image = body_image;

		/*****
		* We support transparent background images as well.
		* To do so, we need to create a pixmap, fill it with the
		* current background color, set the clipmask and copy the
		* real image onto it. Then we replace the old background image
		* with the new semi-transparent background image.
		*****/
		if(!ImageDelayedCreation(body_image) && body_image->clip != None)
		{
			ToolkitAbstraction *tka = HTML_ATTR(tka);
			WINDOW win;
			PIXMAP pixmap;

			if(tka->IsRealized((Widget)html) && tka->win != None)
				win = tka->win;
			else
				win = tka->defaultRoot;

			/* create a new pixmap */
			if((pixmap = tka->CreatePixmap(tka->dpy, win, width, height,
				TkaVisualGetDepth(html))) != None)
			{
				XGC gc;
				gc = tka->CreateGC(tka->dpy, win, 0, NULL);

				/* do a fillrect in the given background color */
				tka->SetForeground(tka->dpy, gc, html->html.body_bg);
				tka->FillRectangle(tka->dpy, pixmap, gc, 0, 0, width, height);

				/* set clipmask */
				tka->SetClipMask(tka->dpy, gc, body_image->clip);
				tka->SetClipOrigin(tka->dpy, gc, 0, 0);

				/* and copy original pixmap to the new pixmap */
				tka->CopyArea(tka->dpy, body_image->pixmap, pixmap, gc, 0, 0,
						width, height, 0, 0);

				/* release original pixmap/gc */
				FreePixmap(tka->dpy, body_image->pixmap);
				FreePixmap(tka->dpy, body_image->clip);

				/* release gc */
				tka->FreeGC(tka->dpy, gc);

				/* save new pixmap */
				body_image->pixmap = pixmap;
				body_image->clip = None;

				/* all done! */
			}
		}
	}
}

/********
****** Private XmHTML Functions
********/

/*****
* Name: 		_XmHTMLGetImageType
* Return Type: 	int
* Description: 	determines the type of an image
* In:
*	ib:			image for which to determine the type
* Returns:
*	image type upon success, IMAGE_UNKNOWN on failure to determine
*	the type.
*****/
Byte
_XmHTMLGetImageType(ImageBuffer *ib)
{
	Byte ret_val = IMAGE_UNKNOWN;
	Byte magic[30];
	static Byte png_magic[8] = {137, 80, 78, 71, 13, 10, 26, 10};

	(void)memcpy(magic, ib->buffer, 30);

	/* check image types we known of. Do most (?) logical order */
	if(!(strncmp((char*)magic, "GIF87a", 6)) ||
		!(strncmp((char*)magic, "GIF89a", 6)))
		ret_val = (Byte)_XmHTMLIsGifAnimated(ib);
	/* compatible gif */
	else if(!(strncmp((char*)magic, "GZF87a", 6)) ||
		!(strncmp((char*)magic, "GZF89a", 6)))
	{
		ret_val = (Byte)_XmHTMLIsGifAnimated(ib);
		/* and adjust return value */
		if(ret_val == IMAGE_GIF)
			ret_val = IMAGE_GZF;
		else if(ret_val == IMAGE_GIFANIM)
			ret_val = IMAGE_GZFANIM;
		else ret_val = IMAGE_GZFANIMLOOP;
	}
	else if(!(strncmp((char*)magic, "eX!flg", 6)))
		ret_val = IMAGE_FLG;
	else if(magic[0] == 0xff && magic[1] == 0xd8 && magic[2] == 0xff)
		ret_val = IMAGE_JPEG;
	else if(!(memcmp(magic, png_magic, 8)))
		ret_val = IMAGE_PNG;
	else if(!(strncmp((char*)magic, "/* XPM */", 9)))
		ret_val = IMAGE_XPM;
	else if(!(strncmp((char*)magic, "#define", 7)) ||
		(!(strncmp((char*)magic, "/* XBM */", 9))))
		ret_val = IMAGE_XBM;

	return(ret_val);
}

/*****
* Name: 		_XmHTMLNewImage
* Return Type: 	XmHTMLImage
* Description: 	creates and fills an image structure
* In:
*	html:		XmHTMLWidget
*	attributes:	image source data
*	*width:		image width, updated upon return
*	*height:	image height, updated upon return
* Returns:
*	a newly created image, all fields filled.
* Note:
*	The width and height returned are stored in the owning ObjectTable
*	element. The real image dimensions are stored in the XmImageInfo and
*	XmHTMLImage structure. A resize of the image is only done if the
*	real image dimensions differ from the requested dimensions and then only
*	if the image isn't part of an animation or is an internal image.
*****/
XmHTMLImage*
_XmHTMLNewImage(XmHTMLWidget html, String attributes, Dimension *width,
	Dimension *height)
{
	static XmHTMLImage *image;
	static XmImageInfo *html_image;
	String src;
	unsigned char image_type = 0;
	PIXMAP pixmap = None, clip = None;
	int pw, ph;

	/* no XmImage stuff, so reset it to NULL */
	_xmimage_cfg = (XmImageConfig*)NULL;

	/* See if we have a source for this image. If not, just return */
	if(!attributes || (src = _XmHTMLTagGetValue(attributes, "src")) == NULL)
		return(NULL);

	/* initialize!!! */
	image = NULL;
	html_image = NULL;

	/*****
	* Get specified width and height. We support relative dimensions as
	* well.
	*****/
	if((pw = _XmHTMLTagCheckNumber(attributes, "width", 0)) < 0)
		*width = (Dimension)(-0.01*pw*HTML_ATTR(work_width));
	else
		*width = pw;
	if((ph = _XmHTMLTagCheckNumber(attributes, "height", 0)) < 0)
		*height = (Dimension)(-0.01*ph*HTML_ATTR(work_height));
	else
		*height = ph;

	_XmHTMLDebug(6, ("images.c: _XmHTMLNewImage, using dimensions %ix%i "
		"(values specified in IMG tag: %ix%i)\n", *width, *height, pw, ph));

	if(html->html.images_enabled)
	{
		/*****
		* We try to be a little bit carefull with system resources.
		* HTML documents intend to have an increasing amount of images in
		* them, so we try to reuse as much as possible.
		* lookForImage also updates width and height if width and/or height
		* is zero or one of them is zero and the other one matches the real
		* image dimension.
		* Images for which the delayed creation bit has been set cannot be
		* copied as they will be different depending on their location on
		* screen.
		*****/
		if((image = lookForImage(html, src, attributes, width, height)) != NULL)
		{
			free(src);
			return(copyHTMLImage(html, image, attributes));
		}

		/* first check for icon entities (if enabled of course) */
		if(HTML_ATTR(icon_entities_enabled))
		{
			int index = _XmHTMLTagGetNumber(attributes, "icon_index", -1);

			if(index != -1)
				html_image = _XmHTMLIconEntities[index].icon;
		}

		/* now do regular loading */
		if(html_image == NULL && html->html.image_proc)
		{
			/* only external loaders can enable delayed image loading */
			html_image = html->html.image_proc((Widget)html, src,
				*width, *height, html->html.client_data);

			/*
			* If this image is to be loaded progressively we *need* to have
			* a get_data() function installed. It's an error if it's not!
			*/
			if(html_image && ImageInfoProgressive(html_image))
			{
				PLC *plc;

				if(!XmIsHTML((Widget)html))
					_XmHTMLError(__WFUNC__(html, "_XmHTMLNewImage"),
						"Fatal: can't do progressive image loading for "
						"XmImage.");

				if(html->html.get_data == NULL)
					_XmHTMLError(__WFUNC__(html, "_XmHTMLNewImage"),
						"Fatal: progressive image loading requires a "
						"XmNprogressiveReadProc procedure!");

				/* progressive images can't also be delayed */
				if(ImageInfoDelayed(html_image))
					html_image->options &= ~XmIMAGE_DELAYED;

				/* no freeing of this ImageInfo */
				html_image->options &= ~XmIMAGE_IMMEDIATE_FREE;
				html_image->options &= ~XmIMAGE_DEFERRED_FREE;

				/* create a context for this image */
				plc = _XmHTMLPLCCreate(html, (XtPointer)html_image, src,
					XmPLC_IMAGE);

				/*
				* Update/override PLC fields.
				* This can include storing image-specific function pointers
				* in the obj_funcs array of the returned PLC.
				*
				* The user_data field must *always* be set or there will be
				* nothing to do. It is used as the user_data field in the
				* stream argument to the get_data() and end_data() calls.
				*/
				plc->user_data = html_image->user_data;
			}

			/* see if loading of this image has been suspended */
			if(html_image && ImageInfoDelayed(html_image))
			{
				XmImageInfo *delayed;

				/* get delayed image information */
				delayed = defaultImage(html, src, DEFAULT_IMG_SUSPENDED, False);

				/* substitute appropriate data fields */
				html_image->data    = delayed->data;
				html_image->clip    = delayed->clip;
				html_image->ncolors = delayed->ncolors;
				html_image->reds    = delayed->reds;
				html_image->greens  = delayed->greens;
				html_image->blues   = delayed->blues;
				html_image->bg      = delayed->bg;
				html_image->depth   = delayed->depth;
				html_image->ncolors = html_image->scolors = delayed->ncolors;
				html_image->colorspace   = delayed->colorspace;
				html_image->transparency = delayed->colorspace;

				/* always use default image dimensions or bad things happen */
				html_image->width   = delayed->width;
				html_image->height  = delayed->height;
				html_image->swidth  = delayed->swidth;
				html_image->sheight = delayed->sheight;

				/* and we don't know the image type (yet) */
				html_image->type = IMAGE_UNKNOWN;

				/* need to append options of the default image as well */
				html_image->options |= delayed->options;
				image_type = IMG_ISINTERNAL;
			}
			/*****
			* If we had an image_proc but it returned NULL, just fall back
			* to the default. This allows the external image_proc to
			* handle a new kind of image without having to handle them
			* all.
			* Added by DCW, 6 Jul 1998.
			*****/
			if(html_image == NULL)
				html_image = XmHTMLImageDefaultProc((Widget)html, src, NULL, 0);
		}
		else if(html_image == NULL)
			html_image = XmHTMLImageDefaultProc((Widget)html, src, NULL, 0);

		/*
		* This widget contains images which are to be created when they
		* are needed.
		*/
		if(html_image && ImageInfoDelayedCreation(html_image))
			html->html.delayed_creation = True;
	}
	else
	{
		/* see if we have this image already loaded */
		if((image = lookForImage(html, src, attributes, width, height)) != NULL
			&& ImageIsInternal(image))
			return(copyHTMLImage(html, image, attributes));

		/* we haven't got it, get a default image */
		html_image = defaultImage(html, src, DEFAULT_IMG_SUSPENDED, False);
		image_type = IMG_ISINTERNAL;
	}

	if(html_image == NULL)
	{
		/* imageProc failed, get default image */
		html_image = defaultImage(html, src, DEFAULT_IMG_UNSUPPORTED, False);
		image_type = IMG_ISINTERNAL;

		/* This is *definitly* an error */
		if(html_image == NULL)
		{
			_XmHTMLError(__WFUNC__(html, "_XmHTMLNewImage"),
			    "A fatal error occured in the default image loading "
				"procedure for image\n    %s\n", src);
		}
	}

	/* allocate and initialize a new image */
	image = (XmHTMLImage*)malloc(sizeof(XmHTMLImage));
	(void)memset(image, 0, sizeof(XmHTMLImage));

	image->magic = XmHTML_IMAGE_MAGIC;

	image->html_image = html_image;
	image->options = image_type;

	/* store original url in private image info */
	image->url = src;

	/* Check if this image is an animation */
	if(html_image->nframes > 1)
		image->options |= IMG_ISANIM;

	/* check if we have delayed creation */
	if(ImageInfoDelayedCreation(html_image))
		image->options |= IMG_DELAYED_CREATION;

	/* store real image dimensions */
	image->width = image->html_image->swidth;
	image->height = image->html_image->sheight;

	/* Store specified width and height (if any) */
	if(*height && *width)
	{
		/* we have got dimensions */
		image->options |= IMG_HASDIMENSIONS;

		/* store requested dimensions */
		image->sheight = *height;
		image->swidth = *width;

		/*****
		* sanity check: if this is an internal image and the specified
		* dimensions are smaller than default image dimensions, return the
		* dimensions of the default image instead or text layout will be
		* really horrible....
		*****/
		if(image_type == IMG_ISINTERNAL)
		{
			if(*height < image->height)
				*height = image->height;
			if(*width < image->width)
				*width = image->width;
		}

		/* revert to original dimensions if we may not scale this image */
		if(!ImageInfoScale(image->html_image))
		{
			*height = image->height;
			*width = image->width;
		}
	}
	else
	{
		image->swidth  = *width  = image->width;
		image->sheight = *height = image->height;
	}

	/* store widget id */
	image->html = html;

	/* set XCC for this image */
	if(!html->html.xcc)
		_XmHTMLCheckXCC(html);
	image->xcc = html->html.xcc;

	/*
	* Go and create the image.
	* We have four options here:
	*	1. the image can be an animation
	*	2. the image can be a plain image;
	*	3. the image can have the delayed creation flag set.
	*	4. we are instructed to progressively load this image.
	*/
	if(ImageInfoProgressive(html_image))
	{
		_XmHTMLDebug(6, ("images.c: _XmHTMLNewImage, image %s:\n"
			"          Progressive bit set!\n", image->url));

		if(ImageHasDimensions(image))
		{
			*width  = image->swidth;
			*height = image->sheight;
		}
		else	/* we must set some return dimensions */
			*width = *height = 64;

		/* plc has already been created */
	}
	else if(ImageIsAnim(image))
		_XmHTMLMakeAnimation(html, image, *width, *height);
	else
	{
		if(!(ImageDelayedCreation(image)))
		{
			/* _XmHTMLInfoToPixmap will scale the image if required */
			if((pixmap = _XmHTMLInfoToPixmap(html, image, html_image,
				*width, *height, NULL, &clip)) == None)
			{
				_XmHTMLFreeImage(html, image);
				return(NULL);
			}
			image->pixmap = pixmap;
			image->clip = clip;
		}
		/* no additional processing for delayed image creation */
	}

	/* pick up remaining image attributes */
	getImageAttributes(image, attributes);

	_XmHTMLDebug(6, ("images.c: _XmHTMLNewImage, image %s loaded, "
		"dimensions: %ix%i\n", image->url, image->width, image->height));

	/* store in the image list */
	addImageToList(html, image);

	/* check if we may free the XmImageInfo right now */
	if(ImageInfoFreeNow(html_image) && !ImageIsCopy(image))
	{
		_XmHTMLFreeImageInfo(html, html_image, False);
		image->html_image = NULL;
	}
	return(image);
}

/*****
* Name: 		_XmHTMLImageUpdateChilds
* Return Type: 	void
* Description: 	updates all childs for the given parent image
* In:
*	image:		parent image data
* Returns:
*	nothing
* Note:
*	This routine udjusts the XmImageInfo and pixmap fields of an image
*	child. It is called whenever _XmHTMLUpdateImage or _XmHTMLReplaceImage
*	is called. It is also called from within plc.c at PLC initialization
*	and at PLC completion.
*****/
void
_XmHTMLImageUpdateChilds(XmHTMLImage *image)
{
	XmHTMLImage *tmp;

	_XmHTMLDebug(6, ("images.c: _XmHTMLImageUpdateChilds, updating childs of "
		"%s\n", image->url));

	for(tmp = image->child; tmp != NULL; tmp = tmp->child)
	{
		/*
		* update all necessary fields, *including* width and height
		* of the word represented by this image!
		*/
		tmp->pixmap        = image->pixmap;
		tmp->clip          = image->clip;
		tmp->frames        = image->frames;
		tmp->nframes       = image->nframes;
		tmp->current_frame = image->current_frame;
		tmp->current_loop  = image->current_loop;
		tmp->loop_count    = image->loop_count;
		tmp->options       = image->options;
		tmp->html_image    = image->html_image;
		tmp->context       = image->context;
		tmp->html          = image->html;
		tmp->width         = image->width;
		tmp->height        = image->height;
		tmp->swidth        = image->swidth;
		tmp->sheight       = image->sheight;
		/* and this is still a copy */
		tmp->options |= IMG_ISCOPY;
		/* update worddate for this image */
		updateImageWord(tmp);
	}
}

/*****
* Name: 		_XmHTMLReplaceOrUpdateImage
* Return Type: 	XmImageStatus
* Description: 	updates an image with new image data
* In:
*	html:		XmHTMLWidget
*	info:		existing image data to be updated. info must contain new
*				image data
*	new_info:	new image info. If NULL the image is updated, else it's
*				replaced.
*	elePtr:		element location, filled upon return when image dimensions
*				are already known.
* Returns:
*	XmIMAGE_OK if no layout recomputation is required, XmIMAGE_ALMOST if
*	it is, an error code otherwise.
*****/
XmImageStatus
_XmHTMLReplaceOrUpdateImage(XmHTMLWidget html, XmImageInfo *info,
	XmImageInfo *new_info, XmHTMLObjectTableElement *elePtr)
{
	XmHTMLImage *image = NULL;
	PIXMAP pixmap = None, clip = None;
	Boolean do_return = False;
	int img_width, img_height;

	_XmHTMLDebug(6, ("images.c: _XmHTMLReplaceOrUpdateImage, looking for %s\n",
		info->url));

	*elePtr = (XmHTMLObjectTableElement)NULL;

	/* given image dimensions */
	img_width  = (new_info != NULL ? new_info->width  : info->width);
	img_height = (new_info != NULL ? new_info->height : info->height);

	for(image = html->html.images; image != NULL; image = image->next)
	{
		_XmHTMLDebug(6, ("\tchecking %s\n", image->url));

		if(image->html_image == info)
		{
			_XmHTMLDebug(6, ("images.c: _XmHTMLReplaceOrUpdateImage, "
				"updating image %s\n", image->url));

			/*****
			* If this is an animation, suspend it first or we will probably
			* be trying to display a frame while it's being deleted if the
			* timer expires while we are updating the image!
			*****/
			if(ImageIsAnim(image))
			{
				if(image->proc_id)
				{
					ToolkitAbstraction *tka = HTML_ATTR(tka);
					tka->RemoveTimeOut(image->proc_id);
					image->proc_id = None;
				}
				image->options &= ~IMG_FRAMEREFRESH;
			}

			/*
			* get/set image dimensions.
			* If dimensions have been specified on the original <IMG> spec,
			* use those, else use the real image dimensions.
			*/
			if(ImageHasDimensions(image))
			{
				image->width  = image->swidth;
				image->height = image->sheight;
				do_return = True;
			}
			else
			{
				/* image dimensions haven't changed: a true image reload */
				if(image->width == img_width && image->height == img_height)
					do_return = True;
				image->swidth  = image->width  = img_width;
				image->sheight = image->height = img_height;
			}

			/* if this is the background image, update body_image ptr */
			if(ImageIsBackground(image))
				html->html.body_image = image;
			else
				/*
				* This image normally should have an owning object, so update
				* the worddata for this image as well.
				*/
				updateImageWord(image);

			/*
			* if this image is a copy and it's no longer delayed,
			* just return.
			*/
			if(ImageIsCopy(image) && !(ImageInfoDelayed(info)))
			{
				_XmHTMLDebug(6, ("images.c: _XmHTMLReplaceOrUpdateImage, "
					"image update not required: current image is a copy\n"));

				if(do_return && image->owner)
				{
					*elePtr = image->owner;
					return(XmIMAGE_OK);
				}
				return(XmIMAGE_ALMOST);
			}

			/* check if we are replacing this image */
			if(new_info != NULL)
			{
				/* release previous info */
				if(!ImageIsInternal(image) &&
					ImageInfoFreeLater(image->html_image))
					_XmHTMLFreeImageInfo(html, image->html_image, False);
				image->html_image = new_info;
			}

			/* free all pixmaps and allocated colors for this image */
			freePixmaps(html, image);

			/* no longer an internal image, nor a copy, nor an animation */
			image->options  &= ~(IMG_ISINTERNAL) & ~(IMG_ISCOPY);
			image->nframes   = 0;

			/* just to be sure */
			image->html_image->options &= ~(XmIMAGE_DELAYED) &
												~(XmIMAGE_SHARED_DATA);

			/* if this image has delayed creation, don't let it be freed */
			if(ImageInfoDelayedCreation(image->html_image))
			{
				/* may not be freed */
				image->html_image->options &= ~(XmIMAGE_IMMEDIATE_FREE);
				image->html_image->options &= ~(XmIMAGE_DEFERRED_FREE);
				image->options |= IMG_DELAYED_CREATION;
				html->html.delayed_creation = True;
			}

			/* Check if we are to create an animation or a plain image */
			if(image->html_image->nframes > 1)
			{
				_XmHTMLMakeAnimation(html, image, image->width, image->height);
			}
			else
			{
				if(!(ImageDelayedCreation(image)))
				{
					/*
					* Create the new pixmap. _XmHTMLInfoToPixmap will scale
					* the image if the real image dimensions differ from the
					* specified ones.
					* This is a serious error if it fails.
					*/
					if((pixmap = _XmHTMLInfoToPixmap(html, image,
						image->html_image, image->width, image->height, NULL,
						&clip)) == None)
						return(XmIMAGE_ERROR);

					/* store it */
					image->pixmap = pixmap;
					image->clip = clip;

					/* background image processing */
					if(ImageIsBackground(image))
						processBodyImage(html, image, image->width,
							image->height);
				}
				else
				{
					/*****
					* if the image dimensions are known, we can do alpha
					* processing right away. Otherwise, we need to set the
					* global delayed_creation flag which will trigger alpha
					* processing when the user calls XmHTMLRedisplay after
					* he has flushed all image.
					*****/
					if(do_return || ImageIsBackground(image))
					{
						initAlphaChannels(html, ImageIsBackground(image));
						doAlphaChannel(html, image);
					}
					else
						html->html.delayed_creation = True;
				}
			}

			/* update all copies of this image */
			_XmHTMLImageUpdateChilds(image);

			/* return owner if image dimensions haven't changed */
			if(do_return && image->owner)
			{
				*elePtr = image->owner;
				return(XmIMAGE_OK);
			}
			return(XmIMAGE_ALMOST);
		}
	}
	_XmHTMLWarning(__WFUNC__(html, "_XmHTMLReplaceOrUpdateImage"),
		XMHTML_MSG_73, info->url);
	return(XmIMAGE_UNKNOWN);
}

/*****
* Name:			_XmHTMLFreeImage
* Return Type:	void
* Description:	Free private image data
* In:
*	html:		widget for obtaining display, required for freeing the pixmap
*	image:		image data to free
* Returns:
*	nothing
* Note:
*	The html_image member is only freed if the may_free member of that
*	structure is set to true (which is automatically the case for images
*	loaded by the imageDefaultProc).
*****/
void
_XmHTMLFreeImage(XmHTMLWidget html, XmHTMLImage *image)
{
	/* sanity */
	if(image == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLFreeImage"), XMHTML_MSG_74);
		return;
	}

	/* always remove the animation timeout proc */
	if(image->proc_id)
	{
		ToolkitAbstraction *tka = HTML_ATTR(tka);
		tka->RemoveTimeOut(image->proc_id);
		image->proc_id = NullTimeout;
	}

	/* see if this is image has been copied */
	if(!ImageIsCopy(image) && !ImageInfoFreed(image))
	{
		_XmHTMLDebug(6, ("images.c: _XmHTMLFreeImage, freeing %s\n",
			image->url ? image->url : "<unknown image>"));

		/*
		* See if we are allowed to free the XmImageInfo structure
		* This is never the case for the internal images and *can* be
		* the case for external images if the user has set the
		* XmIMAGE_DEFERRED_FREE and/or XmIMAGE_IMMEDIATE_FREE bit.
		*/
		if(!ImageIsInternal(image) && image->html_image &&
			(ImageInfoFreeNow(image->html_image) ||
			ImageInfoFreeLater(image->html_image)))
			_XmHTMLFreeImageInfo(html, image->html_image, False);

		/*
		* Free all pixmaps and allocated colors for this image.
		* Also frees all XmImageFrame animation data.
		*/
		freePixmaps(html, image);

		if(image->url)
			free(image->url);
	}
	else
		_XmHTMLDebug(6, ("images.c: _XmHTMLFreeImage, freeing a copy.\n"));

	image->html_image = NULL;

	/* free allocated strings */
	free(image->alt);
	if(image->map_url)
		free(image->map_url);

	free(image);

	image = NULL;
}

/*****
* Name: 		_XmHTMLReleaseImage
* Return Type: 	void
* Description: 	releases the given image and updates the internal list
*				of images.
* In:
*	html:		XmHTMLWidget id
*	image:		image to release
* Returns:
*	nothing
*****/
void
_XmHTMLReleaseImage(XmHTMLWidget html, XmHTMLImage *image)
{
	/* sanity */
	if(image == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLReleaseImage"),
			XMHTML_MSG_74);
		return;
	}

	/* first remove this image from the list of images */
	if(image == html->html.images)
		html->html.images = image->next;
	else
	{
		XmHTMLImage *tmp = html->html.images;

		/* walk the list until we find it */
		for(; tmp->next != NULL && tmp->next != image; tmp = tmp->next);

		/* not found */
		if(tmp == NULL)
		{
			_XmHTMLWarning(__WFUNC__(html, "_XmHTMLReleaseImage"),
				XMHTML_MSG_75, image->url);
			return;
		}
		/*
		* Disconnect this image from the list.
		* tmp is the image just before the image to release.
		*/
		tmp->next = image->next;
	}

	/* now release it */
	_XmHTMLFreeImage(html, image);
}

/*****
* Name:			_XmHTMLLoadBodyImage
* Return Type:	void
* Description: 	loads the body image specified by the given url
* In:
*	html:		XmHTMLWidget id
*	url:		location of body image to load. When this is NULL, the current
*				background image is removed.
* Returns:
*	nothing.
*****/
void
_XmHTMLLoadBodyImage(XmHTMLWidget html, String url)
{
	Dimension width, height;
	String buf;
	XmHTMLImage *body_image;

	/* sanity check */
	if(url == NULL)
	{
		html->html.body_image = NULL;
		return;
	}
	_XmHTMLDebug(6, ("images.c: _XmHTMLLoadBodyImage, body image url is %s\n",
		url));

	/* kludge so _XmHTMLNewImage recognizes it */
	buf = malloc(strlen(url)+7);
	sprintf(buf, "src=\"%s\"", url);

	/* load it */
	if((body_image = _XmHTMLNewImage(html, buf, &width, &height)) != NULL)
		processBodyImage(html, body_image, width, height);

	free(buf);
}

/*****
* Name: 		_XmHTMLFreeImageInfo
* Return Type: 	void
* Description: 	free the given XmImageInfo structure
* In:
*	html:		XmHTMLWidget id;
*	info:		image to free;
*	external:	True when application code is freeing imageInfo, false if not.
* Returns:
*	nothing
*****/
void
_XmHTMLFreeImageInfo(XmHTMLWidget html, XmImageInfo *info, Boolean external)
{
	XmImageInfo *tmp;
	XmHTMLImage *image;

	_XmHTMLDebug(6, ("images.c: _XmHTMLFreeImageInfo Start, freeing %s\n",
		(info->url ? info->url : "<unknown image>")));

	/*****
	* always set the info free bit, even if we are being called from
	* application code.
	*****/
	if(XmIsHTML((Widget)html))		/* fix 09/01/97-01, rr */
	{
		for(image = html->html.images; image != NULL; image = image->next)
		{
			if(image->html_image == info)
				image->options |= IMG_INFOFREED;
		}
	}

	/* also free all animation frames for this image */
	while(info != NULL)
	{
		tmp = info->frame;

		/* always free this */
		if(info->url)
			free(info->url);
		info->url = NULL;
		/*
		* This will be true only for internal images of which the data
		* may *never* be freed. Internal images are created only *once* and
		* are used by every document that needs to display an internal image.
		* Fix thanks to Dick Porter <dick@cymru.net>
		*/
		if(!(ImageInfoShared(info)))
		{
			if(info->data)
				free(info->data);
			if(ImageInfoClipmask(info))
				free(info->clip);
			if(ImageInfoRGBSingle(info))
			{
				if(info->reds)
					free(info->reds);
			}
			else
			{
				if(info->reds)
					free(info->reds);
				if(info->greens)
					free(info->greens);
				if(info->blues)
					free(info->blues);
			}
			if(ImageInfoDelayedCreation(info) && info->alpha)
				free(info->alpha);
			free(info);
		}
		/* and reset the ptr. */
		info = NULL;
		info = tmp;
	}
	_XmHTMLDebug(6, ("images.c: _XmHTMLFreeImageInfo End\n"));
}

/*****
* Name:			_XmHTMLImageCheckDelayedCreation
* Return Type: 	void
* Description: 	walks the list of images and rereads them in order to process
*				any alpha channel information.
* In:
*	html:		XmHTMLWidget id;
* Returns:
*	nothing, but this routine unsets the delayed_creation flag if the
*	current document does not have a body image (when alpha channel info
*	is processed against a solid background color, no reprocessing is required
*	since the position if the image does not matter).
* Note:
*	It's the caller's responsibility to check whether or not the
*	delayed_creation flag has been set.
*****/
void
_XmHTMLImageCheckDelayedCreation(XmHTMLWidget html)
{
	XmHTMLImage *tmp;
	Boolean for_body_image = False;

	/* if we have a body image, but it's not yet available, do nothing */
	if(html->html.body_image && !ImageDelayedCreation(html->html.body_image) &&
		!BodyImageLoaded(html->html.body_image->html_image))
		return;

	/*****
	* First check if the body image is present. If it is and we should
	* process it now, the alphaChannel info must be initialized for background
	* image alpha processing.
	*****/
	if(html->html.body_image && ImageDelayedCreation(html->html.body_image))
		for_body_image = True;

	/*****
	* Always (re-)initialize the AlphaChannel buffer. If the body image
	* is an alpha-channeled image we must use the current background *color*
	* for it, and for all subsequent images we need to use the colormap of
	* the background *image*.
	*****/
	initAlphaChannels(html, for_body_image);

	/* now walk all images */
	for(tmp = html->html.images; tmp != NULL; tmp = tmp->next)
	{
		if(ImageDelayedCreation(tmp))
		{
			doAlphaChannel(html, tmp);
			/*****
			* if this was the body image, we need to re-initialize the
			* alphaChannel info, or images will use the background color
			* instead of the new background image. doAlphaChannel will reset
			* the delayed creation bit for alpha channelled images
			* automatically, so we don't need to do this here.
			* Note:
			*	You might wonder why we don't reprocess the images when
			*	we find out the background image has an alpha channel.
			*	We can get away with this 'cause the background image
			*	is *always* the first element in the list of images.
			*****/
			if(ImageIsBackground(tmp) && for_body_image)
				initAlphaChannels(html, (for_body_image = False));
		}
	}
	/*****
	* If we don't have a body image, all images have been processed and
	* do not need to be reprocessed when the document resizes.
	*****/
	if(html->html.body_image == NULL)
		html->html.delayed_creation = False;
}

#ifdef WITH_PS
XmImageInfo*
_XmHTMLImageCreateInfoFromPixmap(XmHTMLWidget html, Drawable pixmap,
	int width, int height)
{
	ToolkitAbstraction *tka = HTML_ATTR(tka);
	XIMAGE *image;
	int x, y, nused = 0;
	unsigned long pixel, used[XmHTML_MAX_IMAGE_COLORS];
	XCOLOR cmap[XmHTML_MAX_IMAGE_COLORS];

	if((image = XGetImage(tka->dpy, pixmap, 0, 0, width, height,
		AllPlanes, ZPixmap)) == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLImageCreateInfoFromPixmap"),
			"XGetImage failed.");
		return(NULL);
	}

	/* initialize to zero */
	memset(used, 0, sizeof(used));

	for(y = 0; y < height; y++)
	{
		for(x = 0; x < width; x++)
		{
			pixel = XGetPixel(image, x, y);

			if(nused < XmHTML_MAX_IMAGE_COLORS && !used[pixel])
			{
				cmap[nused].pixel = pixel;
				used[nused++] = 1;
			}
		}
	}
}
#endif

/********
****** Public XmHTML Functions
********/

/*****
* Name: 		XmHTMLImageDefaultProc
* Return Type: 	XmImageInfo*
* Description: 	XmHTML default image loading procedure
* In:
*	w:			Widget ID
*	file:		full name and location of image to load
*	*buf:		image data
*	size:		length of buf.
* Returns:
*	An XmImageInfo structure on success or NULL on failure
*****/
XmImageInfo*
XmHTMLImageDefaultProc(Widget w, String file, unsigned char *buf, int size)
{
	ImageBuffer data, *ib;
	static XmImageInfo *image;

	image = NULL;

	/* must be a valid filename */
	if(!file)
		return(NULL);

	if(size == 0)
	{
		if((ib = _XmHTMLImageFileToBuffer(file)) == NULL)
			return(NULL);
	}
	else
	{
		data.file = file;
		data.buffer = (Byte*)buf;
		data.size = (size_t)size;
		data.next = 0;
		data.may_free = False;
		ib = &data;
	}
	/* assume all images have an initial depth of 8 bits */
	ib->depth = 8;

	_XmHTMLDebug(6, ("images.c, XmHTMLImageDefaultProc, checking type of "
		"image %s\n", file));

	ib->type = _XmHTMLGetImageType(ib);

	/* sanity */
	RewindImageBuffer(ib);

	if(ib->type == IMAGE_UNKNOWN || ib->type == IMAGE_ERROR)
	{
		_XmHTMLDebug(6, ("images.c, XmHTMLImageDefaultProc, could not "
			"determine type of image %s\n", file));
		FreeImageBuffer(ib);
		return(NULL);
	}

	switch(ib->type)
	{
		case IMAGE_GIFANIM:
		case IMAGE_GIFANIMLOOP:
		case IMAGE_GZFANIM:
		case IMAGE_GZFANIMLOOP:
			image = readGifAnimation(w, ib);
			break;
		case IMAGE_FLG:
			/* bypasses the readImage + defaultImage proc entirely */
			image = _XmHTMLReadFLG((XmHTMLWidget)w, ib);
			break;
		case IMAGE_XPM:
		case IMAGE_XBM:
		case IMAGE_GIF:
		case IMAGE_GZF:
		case IMAGE_JPEG:
		case IMAGE_PNG:
			{
				XmHTMLRawImageData *img_data;

				/* Go and load the given image */
				if((img_data = readImage(w, ib)) != NULL)
				{
					/*****
					* If image creation hasn't been delayed (it can be delayed
					* if for example, a png image with an alpha channel or a
					* tRNS chunk is being processed), use the default image
					* proc to create an XmImageInfo structure for this image
					*****/
					if(img_data->delayed_creation == False)
						image = imageDefaultProc(w, img_data, file);
					else
						image = imageDelayedProc(w, img_data, ib);
					/*****
					* No longer needed, destroy. Don't free the members of
					* this structure as they are moved to the returned image.
					*****/
					free(img_data);
				}
			}
			break;
		default:
			break;
	}
	if(image)
	{
		image->type = ib->type;
		image->depth= ib->depth;
	}
	FreeImageBuffer(ib);
	return(image);
}

/*****
* Name:
* Return Type:
* Description:
* In:
*
* Returns:
*
*****/
static void
loadIcon(XmHTMLWidget html, IconEntity *icon)
{
	XmImageInfo *html_image = NULL;

#ifdef NOTYET
	/*****
	* Overriding icon entities not yet possible: there are a few
	* conditions that need to be fullfilled by the external loader
	* for this to work properly: dimension *must* be returned!
	*****/
	if(html->html.images_enabled && html->html.image_proc)
	{
		int w = -1, h = -1;

		/* call external loader for this image */
		html_image = html->html.image_proc((Widget)html, icon->escape,
			w, h, html->html.client_data);
	}
#endif

	if(html_image == NULL)
	{
		char **xpm_data, *name;
		XmHTMLRawImageData *data;

		name = icon->escape;
		xpm_data = icon->data;

		data = _XmHTMLCreateXpmFromData((Widget)html, xpm_data, name);

		/* load the icon */
		html_image = imageDefaultProc((Widget)html, data, name);
		html_image->depth = 4; /* at most 16 colors */

		/* never free this image or it's data */
		html_image->options &= ~XmIMAGE_DEFERRED_FREE;
		html_image->options |= XmIMAGE_SHARED_DATA;
	}
	icon->icon = html_image;
}

/*****
* Name:
* Return Type:
* Description:
* In:
*
* Returns:
*
*****/
String
_XmHTMLImageGetIconAttribs(Widget w, int index)
{
	static String attributes;
	static String fmt = "src=\"%s\" icon_index=%i width=%i height=%i align=\"%s\"";
	IconEntity *icon = &_XmHTMLIconEntities[index];
	int len, tmp;
	XmHTMLWidget html = (XmHTMLWidget)w;
	String valign;

	/* load the icon first */
	if(icon->icon == NULL)
		loadIcon(html, icon);

	/*****
	* Compute additional memory required to store icon name, index,
	* width & height
	*****/
	len = icon->len;

	tmp = index;
	while(tmp)
	{
		len++;
		tmp /= 10;	/* remove digit */
	}
	tmp = icon->icon->width;
	while(tmp)
	{
		len++;
		tmp /= 10;	/* remove digit */
	}
	tmp = icon->icon->height;
	while(tmp)
	{
		len++;
		tmp /= 10;	/* remove digit */
	}

	/* get requested vertical alignment */
	switch(HTML_ATTR(icon_valign))
	{
		case XmALIGNMENT_BASELINE_TOP:
		case XmALIGNMENT_CONTENTS_TOP:
			valign="top";
			tmp += 3;
			break;
		case XmALIGNMENT_BASELINE_BOTTOM:
		case XmALIGNMENT_CONTENTS_BOTTOM:
			valign="bottom";
			tmp += 6;
			break;
		case XmALIGNMENT_CENTER:
		default:
			valign="middle";
			tmp += 6;
			break;
	}

	/* add one for NULL */
	tmp += 1;

	attributes = (String)malloc((strlen(fmt) + icon->len + tmp) * sizeof(char));

	sprintf(attributes, fmt, icon->escape, index, icon->icon->width,
		icon->icon->height, valign);
	return(attributes);
}
