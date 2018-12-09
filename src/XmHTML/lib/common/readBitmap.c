#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* readBitmap.c : XmHTML X11 bitmap image loading routines
*
* This file Version	$Revision$
*
* Creation date:		Wed Feb 19 03:32:58 GMT+0100 1997
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
* Revision 1.10  1998/04/04 06:28:28  newt
* XmHTML Beta 1.1.3
*
* Revision 1.9  1997/10/23 00:25:19  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.8  1997/08/31 17:38:35  newt
* debug level change
*
* Revision 1.7  1997/08/01 13:08:03  newt
* Progressive Image loading changes.
*
* Revision 1.6  1997/05/28 01:53:55  newt
* Image depth storage.
*
* Revision 1.5  1997/04/29 14:31:03  newt
* Header files modifications.
*
* Revision 1.4  1997/03/28 07:23:52  newt
* Added a (Byte*) cast in bgets.
*
* Revision 1.3  1997/03/20 08:14:25  newt
* repaired bgets and sscanf in _XmHTMLReadBitmap
*
* Revision 1.2  1997/03/11 19:58:13  newt
* ImageBuffer changes
*
* Revision 1.1  1997/03/02 23:02:45  newt
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

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/
Byte bitmap_bits[8] = { 1, 2, 4, 8, 16, 32, 64, 128 };

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/

/*** Private Variable Declarations ***/
#define	MAX_LINE	81

/*****
* Name: 		bgets
* Return Type: 	String
* Description: 	memory buffer version of fgets
* In:
*	buf:		character buffer
*	max_len:	max chars to read
*	ib:			memory buffer
* Returns:
*	buf filled with at most max_len - 1 characters or NULL on end-of-buffer.
*****/
static String
bgets(String buf, int max_len, ImageBuffer *ib)
{
	Byte *chPtr;
	int len = 0;

	if(ib->size > ib->next)
	{
		chPtr = ib->buffer + ib->next;

		while(True)
		{
			if(len < max_len-1 && *chPtr != '\0' && *chPtr != '\n' &&
				ib->next + len < ib->size)
			{
				len++;
				chPtr++;
			}
			else
				break;
		}
		/* must include terminating character as well */
		if(*chPtr == '\n' || *chPtr == '\0')
			len++;
		(void)memcpy(buf, ib->buffer + ib->next, len);
		buf[len] = '\0';	/* NULL terminate */
		ib->next += len;
		return(buf);
	}
	return(NULL);
}

/*****
* Name: 		_XmHTMLReadBitmap
* Return Type: 	XmHTMLRawImageData*
* Description: 	X11 Bitmap reading routine
* In:
*
* Returns:
*	loaded image data
* Note:
*	Can't use any of the X[]Bitmap[] functions since we require the image data
*	to be in ZPixmap format.
*****/
XmHTMLRawImageData*
_XmHTMLReadBitmap(Widget html, ImageBuffer *ib)
{
	char line[MAX_LINE], name_and_type[MAX_LINE], *t;
	Byte *ptr;
	int cnt = 0, lim, bytes_per_line, raster_length;
	int i, bytes, value, blackbit, whitebit, width = 0, height = 0;
	XCOLOR fg_color, bg_color;
	Widget area;
	COLORMAP cmap;
	static XmHTMLRawImageData *img_data;
	ToolkitAbstraction *tka;

	area = HTML_ATTR(work_area);
	tka  = HTML_ATTR(tka);

	_XmHTMLDebug(15, ("_XmHTMLReadBitmap: start, file is %s\n", ib->file));

	/* Initialize image data */
	ib->depth = 2;

	/* check bitmap header */
	for( ; ; )
	{
		if(!(bgets(line, MAX_LINE, ib)))
			break;

		/* see if we have successfully read a line */
		if(strlen(line) == (MAX_LINE - 1))
		{
			/* probably not an xbm image, just return. */
			_XmHTMLDebug(15, ("_XmHTMLReadBitmap: end, not a xbm image\n"));
			return((XmHTMLRawImageData*)NULL);
		}

		/* check for xpm */
		if(!(strcmp(line, "//* XPM *//")))
		{
			/* an xpm pixmap, return */
			return((XmHTMLRawImageData*)NULL);
		}

		/* get width and height for this bitmap */
		if(sscanf(line, "#define %s %d", name_and_type, &value) == 2)
		{
			if (!(t = strrchr(name_and_type, '_')))
				t = name_and_type;
			else
				t++;
			if (!strcmp("width", t))
				width  = value;
			if (!strcmp("height", t))
				height = value;
			continue;
		}
		if(((sscanf(line, "static short %s = {", name_and_type)) == 1) ||
			((sscanf(line,"static char * %s = {", name_and_type)) == 1))
		{
			/* not a bitmap, return */
			return((XmHTMLRawImageData*)NULL);
		}

		/* last line of the bitmap header */
		if(sscanf(line,"static char %s = [",name_and_type) == 1)
			break;
	}

	/* bitmap has invalid dimension(s) */
	if(width == 0 || height == 0)
		return((XmHTMLRawImageData*)NULL);

	_XmHTMLDebug(15, ("readXbmBitmap: %s is plain X11 bitmap.\n", ib->file));

	/* allocate image */
	AllocRawImageWithCmap(img_data, width, height, 2);

	img_data->color_class = XmIMAGE_COLORSPACE_GRAYSCALE;
	img_data->bg = -1;

	/* Get fore- and background color values */
	GETP(fg_color) = HTML_ATTR(body_fg);
	GETP(bg_color) = HTML_ATTR(body_bg);

	/* get display colormap */
	cmap = TkaGetColormap(html);

	/* fill image colormap */
	tka->QueryColor(tka->dpy, cmap, &fg_color);
	tka->QueryColor(tka->dpy, cmap, &bg_color);

	blackbit = 0; /* fg_color.pixel; */
	whitebit = 1; /* bg_color.pixel; */

	GETR(img_data->cmap[0]) = GETR(fg_color);
	GETG(img_data->cmap[0]) = GETG(fg_color);
	GETB(img_data->cmap[0]) = GETB(fg_color);
	GETP(img_data->cmap[0]) = GETP(fg_color);

	GETR(img_data->cmap[1]) = GETR(bg_color);
	GETG(img_data->cmap[1]) = GETG(bg_color);
	GETB(img_data->cmap[1]) = GETB(bg_color);
	GETP(img_data->cmap[1]) = GETP(bg_color);

	bytes_per_line = ((img_data->width + 7) / 8);

	/* size of a scan line */
	raster_length =	bytes_per_line * img_data->height;

	/* read bitmap data */
	ptr = img_data->data;
	lim = bytes_per_line * 8;
	for(bytes = 0; bytes < raster_length; bytes++)
	{
		String chPtr, elePtr;
		if(!(bgets(line, MAX_LINE, ib)))
			break;
		elePtr = line;
		while((chPtr = strstr(elePtr, ",")) != NULL)
		{
			if(sscanf(elePtr, " 0x%x%*[,}]%*[ \r\n]", &value) != 1)
			{
				_XmHTMLDebug(15, ("readXbmBitmap: end, unexpected end of "
					"data.\n"));
				FreeRawImage(img_data);
				return((XmHTMLRawImageData*)NULL);
			}
			for(i = 0; i < 8; i++)
			{
				if(cnt < (img_data->width))
				{
					if(value & bitmap_bits[i])
						*ptr++ = blackbit;
					else
						*ptr++ = whitebit;
				}
				if(++cnt >= lim)
					cnt = 0;
			}
			elePtr = chPtr + 1;
		}
	}
	_XmHTMLDebug(15, ("_XmHTMLReadBitmap: end, %s loaded\n", ib->file));

	return(img_data);
}

/*****
* Progressive Bitmap loading routines
*****/

static int
_PLC_XBM_bgets(String buf, int max_len, PLC *plc)
{
	Byte *chPtr;
	int len = 0;
	PLCImageXBM *xbm = &(plc->object->plc_xbm_image);

	if(xbm->buf_pos < xbm->byte_count)
	{
		chPtr = xbm->buffer + xbm->buf_pos;

		while(True)
		{
			if(len < max_len-1 && *chPtr != '\0' && *chPtr != '\n' &&
				*chPtr != '}' && xbm->buf_pos + len < xbm->byte_count)
			{
				len++;
				chPtr++;
			}
			else
				break;
		}
		/* must include terminating character as well */
		if(*chPtr == '\n' || *chPtr == '}' || *chPtr == '\0')
			len++;

		(void)memcpy(buf, xbm->buffer + xbm->buf_pos, len);
		buf[len] = '\0';	/* NULL terminate */
		xbm->buf_pos += len;
		return(len);
	}

	/* not enough data, make a new request */
	plc->min_in = 0;
	plc->max_in = plc->input_size;
	(void)_PLCDataRequest(plc);

	return(0);
}

void
_PLC_XBM_Init(PLC *plc)
{
	char line[MAX_LINE], name_and_type[MAX_LINE], *t;
	int value, width = 0, height = 0, len;
	XCOLOR fg_color, bg_color;
	Widget area;
	COLORMAP cmap;
	PLCImageXBM *xbm;
	XmHTMLWidget html;
	ToolkitAbstraction *tka;

	_XmHTMLDebug(15, ("plc.c: _PLC_XBM_Init for %s\n", plc->url));

	/* this plc is active */
	plc->plc_status = PLC_ACTIVE;

	xbm = &(plc->object->plc_xbm_image);
	html = xbm->owner;
	area = HTML_ATTR(work_area);
	tka  = HTML_ATTR(tka);

	/* rewind input buffers, it may not be the first time we are doing this */
	_PLCRewindInputBuffer(plc);

	/*****
	* We don't know how large the raw data will be at this point, allocate
	* a default buffer so we can get at least the initial image data
	*****/
	if(xbm->buf_size == 0)
	{
		xbm->buf_size = PLC_MAX_BUFFER_SIZE;
		xbm->buffer = (Byte*)calloc(xbm->buf_size, sizeof(Byte));
	}

	/* read some more data, appending at data we have already read */
	if(xbm->buf_pos >= xbm->byte_count)
	{
		/* read data from input but not more than we can take */
		if((value = plc->left) > (xbm->buf_size - xbm->byte_count))
			value = xbm->buf_size - xbm->byte_count;

		if((len = _PLCReadOK(plc, xbm->buffer + xbm->byte_count, value)) == 0)
			return;	/* end of data, suspended or aborted */

		xbm->byte_count += len;
	}
	/* rewind input buffer */
	xbm->buf_pos = 0;

	/* check bitmap header */
	for( ; ; )
	{
		if((len = _PLC_XBM_bgets(line, MAX_LINE, plc)) == 0)
			return;

		/* see if we have successfully read a line */
		if(len == (MAX_LINE - 1))
		{
			/* probably not an xbm image, just return. */
			_XmHTMLDebug(15, ("_XmHTMLReadBitmap: end, not a xbm image\n"));
			plc->plc_status = PLC_ABORT;
			return;
		}

		/* check for xpm */
		if(!(strcmp(line, "//* XPM *//")))
		{
			/* an xpm pixmap, return */
			plc->plc_status = PLC_ABORT;
			return;
		}

		/* get width and height for this bitmap */
		if(sscanf(line, "#define %s %d", name_and_type, &value) == 2)
		{
			if (!(t = strrchr(name_and_type, '_')))
				t = name_and_type;
			else
				t++;
			if (!strcmp("width", t))
				width  = value;
			if (!strcmp("height", t))
				height = value;
			continue;
		}
		if(((sscanf(line, "static short %s = {", name_and_type)) == 1) ||
			((sscanf(line,"static char * %s = {", name_and_type)) == 1))
		{
			/* not a bitmap, return */
			plc->plc_status = PLC_ABORT;
			return;
		}

		/* last line of the bitmap header */
		if(sscanf(line,"static char %s = [",name_and_type) == 1)
			break;
	}
	/* start of real image data */
	xbm->data_start = xbm->buf_pos;

	/* bitmap has invalid dimension(s) */
	if(width == 0 || height == 0)
	{
		plc->plc_status = PLC_ABORT;
		return;
	}

	xbm->width  = width;
	xbm->height = height;

	/* two-color grayscale image */
	xbm->colorclass = XmIMAGE_COLORSPACE_GRAYSCALE;
	xbm->cmapsize = 2;
	xbm->cmap = (XCOLOR*)calloc(xbm->cmapsize, sizeof(XCOLOR));

	/* image is initially fully opaque */
	xbm->transparency = XmNONE;
	xbm->bg_pixel = -1;

	/*****
	* Resize incoming data buffer but don't touch buf_pos and byte_count
	* as the buffer already contains data
	*****/
	xbm->buf_size   = xbm->width * xbm->height;
	xbm->buffer     = (Byte*)realloc(xbm->buffer, xbm->buf_size * sizeof(Byte));

	/* image data buffer. */
	xbm->data_size   = xbm->width*xbm->height;
	xbm->data_pos    = 0;
	xbm->prev_pos    = 0;
	xbm->data = (Byte*)calloc(xbm->data_size + 1, sizeof(Byte));

	/* Get fore- and background color values */
	GETP(fg_color) = HTML_ATTR(body_fg);
	GETP(bg_color) = HTML_ATTR(body_bg);

	/* get display colormap */
	cmap = TkaGetColormap(html);

	/* fill image colormap */
	tka->QueryColor(tka->dpy, cmap, &fg_color);
	tka->QueryColor(tka->dpy, cmap, &bg_color);

	GETR(xbm->cmap[0]) = GETR(fg_color);
	GETG(xbm->cmap[0]) = GETG(fg_color);
	GETB(xbm->cmap[0]) = GETB(fg_color);
	GETP(xbm->cmap[0]) = GETP(fg_color);

	GETR(xbm->cmap[1]) = GETR(bg_color);
	GETG(xbm->cmap[1]) = GETG(bg_color);
	GETB(xbm->cmap[1]) = GETB(bg_color);
	GETP(xbm->cmap[1]) = GETP(bg_color);

	/* no of bytes per line for this bitmap */
	xbm->stride = ((xbm->width + 7) / 8);

	/* full raw data size */
	xbm->raster_length = xbm->stride * xbm->height;

	/* object has been initialized */
	plc->initialized = True;

	plc->curr_obj_func = 0;	/* move to XBM scanline reader */
}

void
_PLC_XBM_ScanlineProc(PLC *plc)
{
	char line[MAX_LINE];
	Byte *ptr;
	int lim, i, value, bytes, len = 0, cnt = 0;
	PLCImageXBM *xbm;

	_XmHTMLDebug(15, ("plc.c: _PLC_XBM_ScanlineProc for %s\n", plc->url));

	xbm = &(plc->object->plc_xbm_image);

	/* all data in buffer has been consumed, get some more */
	if(xbm->buf_pos >= xbm->byte_count)
	{
		if((value = plc->left) > (xbm->buf_size - xbm->byte_count))
			value = xbm->buf_size - xbm->byte_count;

		/* append at current position */
		if((len = _PLCReadOK(plc, xbm->buffer + xbm->byte_count, value)) == 0)
			return;	/* end of data, suspended or aborted */

		/* new data length */
		xbm->byte_count += len;
	}
	/* raw image data start */
	xbm->buf_pos = xbm->data_start;

	/* decoded image data start */
	ptr = xbm->data;
	xbm->data_pos = 0;
	lim = xbm->stride * 8;

	for(bytes = 0; bytes < xbm->raster_length &&
		xbm->data_pos < xbm->data_size; bytes++)
	{
		String chPtr, elePtr;

		/* read a new line of data */
		if((len = _PLC_XBM_bgets(line, MAX_LINE, plc)) == 0)
		{
			/* last known full scanline */
			xbm->data_pos = (xbm->data_pos > xbm->data_size ?
								xbm->data_size : xbm->data_pos);

			/* all data processed */
			if(plc->plc_data_status == STREAM_END)
				break;

			return;	/* suspended or aborted */
		}
		elePtr = line;

		while((chPtr = strstr(elePtr, ",")) != NULL)
		{
			if(sscanf(elePtr, " 0x%x%*[,}]%*[ \r\n]", &value) != 1)
			{
				_XmHTMLDebug(15, ("_PLC_XBM_ScanlineProc: unexpected end of "
					"data.\n"));
				plc->plc_status = PLC_ABORT;
				return;
			}
			for(i = 0; i < 8; i++)
			{
				if(cnt < xbm->width)
				{
					if(value & bitmap_bits[i])
						*ptr++ = 0;	/* blackbit */
					else
						*ptr++ = 1;	/* whitebit */
					xbm->data_pos++;
				}
				/* processed a full scanline */
				if(++cnt >= lim)
					cnt = 0;
			}
			elePtr = chPtr + 1;
		}
	}
	/* done with this image */
	plc->obj_funcs_complete = True;
}

void
_PLC_XBM_Destructor(PLC *plc)
{
	/* nothing special for XBM */
	return;
}

