#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* readGIF.c : XmHTML gif routines. Also containes a GIF to GZF converter.
*
* This file Version	$Revision$
*
* Creation date:		Wed Feb 26 01:57:10 GMT+0100 1997
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
*
* Copyright (C) 1994-1997 by Ripley Software Development 
* All Rights Reserved
*
* Based on code by David Koblas from the pbmplus package, of which the original
* copyright notice is as follows:
*
* +-------------------------------------------------------------------+
* | Copyright 1990 - 1994, David Koblas. (koblas@netcom.com)	      |
* |   Permission to use, copy, modify, and distribute this software   |
* |   and its documentation for any purpose and without fee is hereby |
* |   granted, provided that the above copyright notice appear in all |
* |   copies and that both that copyright notice and this permission  |
* |   notice appear in supporting documentation.  This software is    |
* |   provided "as is" without express or implied warranty.	          |
* +-------------------------------------------------------------------+
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
* Revision 1.11  1998/04/27 07:02:54  newt
* tka stuff
*
* Revision 1.10  1998/04/04 06:28:31  newt
* XmHTML Beta 1.1.3
*
* Revision 1.9  1997/10/23 00:25:20  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.8  1997/08/31 17:40:42  newt
* debug level change
*
* Revision 1.7  1997/08/30 01:29:17  newt
* _XmHTMLWarning proto & LZWStream changes.
*
* Revision 1.6  1997/08/01 13:10:14  newt
* Changes to incorporate the new XmHTMLGIFStream object and promote code-reuse
* between GIF/GZF decoders.
*
* Revision 1.5  1997/05/28 01:55:36  newt
* A lot of changes to incorporate the LZWStream GIF decoder (which does *NOT*
* require a Unisys license!).
* Added support for the GZF image type (reading *and* writing).
*
* Revision 1.4  1997/04/29 14:31:10  newt
* Header files modifications.
*
* Revision 1.3  1997/04/03 05:42:31  newt
* ReadImage calls now use *w and *h instead of LM_to_uint
*
* Revision 1.2  1997/03/11 19:58:30  newt
* ImageBuffer changes. Added support for reading animated gifs
*
* Revision 1.1  1997/03/02 23:02:49  newt
* Initial Revision
*
*****/ 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#if defined(HAVE_LIBPNG) || defined(HAVE_LIBZ)
#include <unistd.h>	/* unlink */
#include <zlib.h>
#endif

#include "toolkit.h"
#include XmHTMLPrivateHeader
#include "LZWStream.h"

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/
XmImageGifProc XmImageGifProc_plugin = NULL;
String XmImageGifzCmd_plugin = NULL;

/*** Private Datatype Declarations ****/

#define CM_RED		0
#define CM_GREEN	1
#define CM_BLUE		2

#define INTERLACE		0x40
#define LOCALCOLORMAP	0x80

/*** Private Function Prototype Declarations ****/
#define BitSet(byte, bit)		(((byte) & (bit)) == (bit))
/* original macro ((b)<<8)|(a) couldn't cope with negative numbers, this can */
#define LM_to_uint(a,b)			((((b)&0xFF) << 8) | ((a)&0xFF))

#define WriteOK(FILE,BUF,LEN)	fwrite(BUF,LEN,1,FILE)

/* read colormap from gif image */
static int ReadColorMap(ImageBuffer *ib, int number,
	Byte buffer[3][XmHTML_MAX_IMAGE_COLORS], int *gray);

/* process a gif extension */
static int DoExtension(ImageBuffer *ib, int label);

/* fill in the colormap */
static void CopyColormap(XCOLOR *colrs, int cmapSize,
	Byte cmap[3][XmHTML_MAX_IMAGE_COLORS]);

/* skip an image */
static void SkipImage(ImageBuffer *ib);

/* uncompressor driver */
static Byte *InflateRaster(XmHTMLWidget html, ImageBuffer *ib, int width,
	int height);
static Byte *InflateGIFExternal(XmImageGifProc inflate,
	ImageBuffer *ib, int dsize, int *nread);
static Byte *InflateGIFInternal(ImageBuffer *ib, int dsize, int *nread);

/* inflate GZF raster data */
static Byte *InflateGZFInternal(ImageBuffer *ib, int dsize, int *nread);

/* transform interlaced GIF/GZF to regular image data */
static Byte *DoImage(Byte *data, int len, int height);

/*** Private Variable Declarations ***/
static String zCmd;			/* uncompress program to use */

/* gif logical screen descriptor */
static struct
{
	unsigned int	Width;
	unsigned int	Height;
	Byte			ColorMap[3][XmHTML_MAX_IMAGE_COLORS];
	unsigned int	BitPixel;
	unsigned int	ColorResolution;
	unsigned int	Background;
	unsigned int	AspectRatio;
	XmHTMLWidget	html;
}GifScreen, GifAnimScreen;

/* gif extension data */
static struct
{
	int	transparent;
	int	delayTime;
	int	inputFlag;
	int	disposal;
	int loopCount;
}Gif89 = { -1, -1, -1, 0, 0};

#ifdef DEBUG
static int current_frame;
#endif

/*****
* Name: 		_XmHTMLGifReadOK
* Return Type: 	size_t
* Description: 	copy len bytes to buf from an ImageBuffer
* In: 
*	*bp:		data source
*	buf:		data destination
*	len:		no of bytes to copy
* Returns:
*	actual no of bytes read or 0 on failure or end of buffer.
*****/
size_t 
_XmHTMLGifReadOK(ImageBuffer *ib, Byte *buf, int len)
{
	if(ib->size > ib->next)
	{
		if(ib->next + len > ib->size)
			len = ib->size - ib->next;
		memcpy(buf, ib->buffer + ib->next, len);
		ib->next += len;
		return(len);
	}
	return(0);
}

/*****
* Name: 		_XmHTMLGifGetDataBlock
* Return Type: 	int
* Description: 	gets the next amount of data from the input buffer
* In: 
*	ib:			current ImageBuffer
*	buf:		storage buffer, filled upon return.
* Returns:
*	no of bytes copied into buf or 0 when no more data.
*****/
size_t
_XmHTMLGifGetDataBlock(ImageBuffer *ib, Byte *buf)
{
	Byte count = 0;

	if(!_XmHTMLGifReadOK(ib, &count, 1))
		return(0);

	if(((int)count != 0) && (!_XmHTMLGifReadOK(ib, buf, (int)count)))
		return(0);

	return((size_t)count);
}

/*****
* Name: 		SkipImage
* Return Type: 	void
* Description: 	skips past an image
* In: 
*	ib:			file descriptor of image to check
* Returns:
*	nothing
*****/
static void
SkipImage(ImageBuffer *ib)
{
	Byte c;	
	static char	buf[256];

	/* Initialize the Compression routines */
	if(!_XmHTMLGifReadOK(ib,&c,1))
		return;

	/* skip image */
	while((_XmHTMLGifGetDataBlock(ib, (Byte*)buf)) > 0);
}

/*****
* Name: 		_XmHTMLIsGifAnimated
* Return Type: 	unsigned char
* Description: 	checks whether a file is a GIF or animated GIF image
* In: 
*	ib:			file descriptor of image to check
* Returns:
*	IMAGE_UNKNOWN, IMAGE_GIF, IMAGE_GIFANIM or IMAGE_GIFANIMLOOP
*****/
unsigned char
_XmHTMLIsGifAnimated(ImageBuffer *ib)
{
	Byte buf[16], c;
	int imageCount = 0;
	int gray;

	/*
	* Initialize GIF89 extensions
	*/
	Gif89.transparent = -1;
	Gif89.delayTime = -1;
	Gif89.inputFlag = -1;
	Gif89.disposal = 0;
	Gif89.loopCount = 0;	/* infinite looping */

	/*
	* When we get here, we have already know this is a gif image
	* so don't test again.
	*/
	/* gif magic */
	(void)_XmHTMLGifReadOK(ib,buf,6);

	/* logical screen descriptor */
	(void)_XmHTMLGifReadOK(ib,buf,7);

	GifAnimScreen.Width	          = LM_to_uint(buf[0],buf[1]);
	GifAnimScreen.Height	      = LM_to_uint(buf[2],buf[3]);
	GifAnimScreen.BitPixel        = 2<<(buf[4]&0x07);
	GifAnimScreen.ColorResolution = (((buf[4]&0x70)>>3)+1);
	GifAnimScreen.Background      = buf[5];
	GifAnimScreen.AspectRatio     = buf[6];

	/* skip global colormap */
	if(BitSet(buf[4], LOCALCOLORMAP))
	{
		if(ReadColorMap(ib,GifAnimScreen.BitPixel,GifAnimScreen.ColorMap,
			&gray))
		{
			_XmHTMLDebug(15, ("readGIF.c, _XmHTMLIsGifAnimated, could not read "
				"global colormap\n"));
			return(IMAGE_UNKNOWN);	/* can't read colormap */
		}
	}

	/*
	* We know a gif image is an animation if either the Netscape2.0 loop
	* extension is present or if we have at least two images.
	* The first case is pretty easy to detect, the second one requires us
	* to process the giffile up to the second image. Although we do not
	* actually decode the image itself, there is a performance loss here.
	*/
	while(imageCount < 2)
	{
		/* read block identifier */
		if(!_XmHTMLGifReadOK(ib, &c, 1))
			break;

		if(c == ';')
			break;	/* gif terminator */

		if(c == '!')
		{
			if(!_XmHTMLGifReadOK(ib, &c, 1))
				return(IMAGE_UNKNOWN);
			if((DoExtension(ib, c)) == IMAGE_GIFANIMLOOP)
				return(IMAGE_GIFANIMLOOP);
			continue;
		}
		if(c != ',')
			continue;	/* invalid start character */

		/* get next block */
		if(!_XmHTMLGifReadOK(ib, buf, 9))
			break;

		if(BitSet(buf[8], LOCALCOLORMAP))
		{
			int foo;
			if(ReadColorMap(ib, GifAnimScreen.BitPixel,
				GifAnimScreen.ColorMap, &foo))
				return(IMAGE_UNKNOWN);	/* can't read colormap */
		}
		/* skip this image */
		SkipImage(ib);

		imageCount++;
	}
	return(imageCount >= 2 ? IMAGE_GIFANIM : IMAGE_GIF);
}

unsigned char
_XmHTMLIsGzfAnimated(ImageBuffer *ib)
{
	switch(_XmHTMLIsGifAnimated(ib))
	{
		case IMAGE_GIF:
			return(IMAGE_GZF);
		case IMAGE_GIFANIM:
			return(IMAGE_GZFANIM);
		case IMAGE_GIFANIMLOOP:
			return(IMAGE_GZFANIMLOOP);
		default:
			return(IMAGE_UNKNOWN);
	}
	return(IMAGE_UNKNOWN);
}

void
_XmHTMLGifAnimTerminate(ImageBuffer *ib)
{
	/* rewind image buffer */
	RewindImageBuffer(ib);
#ifdef DEBUG
	_XmHTMLDebug(15, ("readGIF, _XmHTMLGifAnimTerminate, read %i frames\n",
		current_frame));
	current_frame = 0;
#endif
}

int
_XmHTMLGifAnimInit(Widget html, ImageBuffer *ib, XmHTMLRawImageData *img_data)
{
	Byte buf[16], c;
	Boolean netscape = False;
	size_t curr_pos = 0;

	/* check if zlib has been compiled in */
#if !defined(HAVE_LIBPNG) && !defined(HAVE_LIBZ)
	if(ib->type == IMAGE_GZF || ib->type == IMAGE_GZFANIM ||
		ib->type == IMAGE_GZFANIMLOOP)
		return(-1);
#endif

#ifdef DEBUG
	current_frame = 0;
#endif

	/* rewind imagebuffer */
	RewindImageBuffer(ib);

	/* reset raw image data */
	ResetRawImage(img_data); 

	_XmHTMLDebug(15, ("readGIF, _XmHTMLGifAnimInit, initializing animated "
		"gif %s\n", ib->file));
	/*
	* Initialize GIF89 extensions
	*/
	Gif89.transparent = -1;
	Gif89.delayTime   = -1;
	Gif89.inputFlag   = -1;
	Gif89.disposal    = 0;
	Gif89.loopCount   = 0;	/* infinite by default */

	/* at this point, we *know* this is a valid gif or gzf image */
	/* skip GIF/GZF magic */
	ib->next = 6;

	/* read logical screen descriptor */
	(void)_XmHTMLGifReadOK(ib,buf,7);

	/* get width and height */
	GifAnimScreen.Width	          = LM_to_uint(buf[0],buf[1]);
	GifAnimScreen.Height          = LM_to_uint(buf[2],buf[3]);
	GifAnimScreen.BitPixel        = 2<<(buf[4]&0x07);
	GifAnimScreen.ColorResolution = (((buf[4]&0x70)>>3)+1);
	GifAnimScreen.Background      = buf[5];
	GifAnimScreen.AspectRatio     = buf[6];

	/* store logical screen dimensions */
	img_data->width  = GifAnimScreen.Width;
	img_data->height = GifAnimScreen.Height;

	/* parent widget for gif_proc */
	GifAnimScreen.html = XmIsHTML(html) ? (XmHTMLWidget)html : NULL;

	/* A global colormap */
	if(BitSet(buf[4], LOCALCOLORMAP))
	{
		int foo;
		if(ReadColorMap(ib, GifAnimScreen.BitPixel, GifAnimScreen.ColorMap,
			&foo))
		{
			/* error reading global colormap, too bad */
			_XmHTMLWarning(__WFUNC__(html, "_XmHTMLGifAnimInit"),
				XMHTML_MSG_105, "global", ib->file);
			return(-1);
		}
		/* store global colormap */
		AllocRawImageCmap(img_data, GifAnimScreen.BitPixel);
		CopyColormap(img_data->cmap, GifAnimScreen.BitPixel,
			GifAnimScreen.ColorMap);
	}
	else
	{
		/* no global colormap, too bad */
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLGifAnimInit"), XMHTML_MSG_106,
			"global", ib->file);
		return(-1);
	}

	/* save current position in block */
	curr_pos = ib->next;

	/* move past all initial headers */

	/* block descriptor */
	if(!_XmHTMLGifReadOK(ib,&c,1))
		return(-1);

	/* loop thru all extensions and global colormap */
	while(c == '!')
	{
		/* read extension type */
		if(!_XmHTMLGifReadOK(ib, &c, 1))
			return(-1);

		/*****
		* Problem: we know this gif is an animation, but it may be a
		* NETSCAPE2.0 loop extension or a series of images. The first has a
		* loop count in it, but the second one doesn't. DoExtension returns
		* IMAGE_GIFANIM if this is a NETSCAPE2.0 or IMAGE_GIF if it's a
		* series of images. So we need to see what we get returned: if it's 
		* GIFANIM we get the loop_count out of the Gif89 struct. If it's just
		* GIF we set it to 1.
		*****/
		if((DoExtension(ib, c)) == IMAGE_GIFANIMLOOP)
			netscape = True;

		/* get next block */
		if(!_XmHTMLGifReadOK(ib, &c, 1))
			return(-1);
	}

	/* save background pixel */
	img_data->bg = Gif89.transparent;

	/* and reset file pointer */
	ib->next = curr_pos;

	_XmHTMLDebug(15, ("readGIF.c, _XmHTMLGifAnimInit, Logical Screen "
		"Descriptor:\n\twidth: %i, height: %i, Background pixel: %i\n"
		"\tloopCount: %i\n", GifAnimScreen.Width, GifAnimScreen.Height, 
		GifAnimScreen.Background, Gif89.loopCount));

	return(netscape ? Gif89.loopCount : 1);
}

/*****
* Name: 		_XmHTMLGifAnimNextFrame
* Return Type: 	Boolean
* Description: 	reads the next frame of a gif animation
* In: 
*
* Returns:
*	True when frame is successfully read. False upon error or no more images.
*****/
Boolean
_XmHTMLGifAnimNextFrame(ImageBuffer *ib, XmHTMLRawImageData *img_data, int *x,
	int *y, int *timeout, int *dispose)
{
	Byte buf[16], c, localColorMap[3][XmHTML_MAX_IMAGE_COLORS];
	int useGlobalColormap, bitPixel;
	Byte *image = NULL;
	XmHTMLWidget html = GifAnimScreen.html;

	/* Initialize GIF89 extensions */
	Gif89.transparent = -1;
	Gif89.delayTime = -1;
	Gif89.inputFlag = -1;
	Gif89.disposal = 0;
	Gif89.loopCount = 0;

	/* read error */
	if(!_XmHTMLGifReadOK(ib, &c, 1))
		return(False);

	/* reset raw image data */
	ResetRawImage(img_data);

	/* run until we get to the image data start character */
	while(c != ',')
	{
		/* GIF terminator */
		if(c == ';')
			return(False);

		/* loop thru all extensions */
		if(c == '!')
		{
			/* read extension type */
			if(!_XmHTMLGifReadOK(ib, &c, 1))
				return(False);

			(void)DoExtension(ib, c);
		}
		if(!_XmHTMLGifReadOK(ib, &c, 1))
			return(False);
	}

	/* image descriptor */
	if(!_XmHTMLGifReadOK(ib,buf,9))
		return(False);

	/* offsets in the logical screen */
	*x = LM_to_uint(buf[0], buf[1]);
	*y = LM_to_uint(buf[2], buf[3]);

	/* width and height for this particular frame */
	img_data->width  = LM_to_uint(buf[4],buf[5]);
	img_data->height = LM_to_uint(buf[6],buf[7]);

	useGlobalColormap = ! BitSet(buf[8], LOCALCOLORMAP);

	bitPixel = 1<<((buf[8]&0x07)+1);

	/* read local colormap. Global colormap stuff is handled by caller */
	if(!useGlobalColormap)
	{
		int i;
		_XmHTMLDebug(15, ("readGIF.c, _XmHTMLGifAnimNextFrame, reading local "
			"colormap\n"));

		if(ReadColorMap(ib, bitPixel, localColorMap, &i))
		{
			_XmHTMLWarning(__WFUNC__(html, "_XmHTMLGifAnimNextFrame"), 
				XMHTML_MSG_105, "local", ib->file);
			return(False);
		}
		/* compare with global colormap and check if it differs */
		if((i = bitPixel) == GifAnimScreen.BitPixel)
		{
			/* compare. Will only be equal to bitPixel if the maps are equal */
			i = memcmp(localColorMap, GifAnimScreen.ColorMap,
				3*XmHTML_MAX_IMAGE_COLORS) + bitPixel;
		}
		/* local colormap differs from global one, use it */
		if(i != GifAnimScreen.BitPixel)
		{
			/* allocate colormap for this frame */
			AllocRawImageCmap(img_data, bitPixel);
			/* fill it */
			CopyColormap(img_data->cmap, bitPixel, localColorMap);
		}
	}

	/* get image depth (= codeSize in GIF images) */
	(void)_XmHTMLGifReadOK(ib,&c,1);
	ib->depth = (int)(c & 0xff);
	ib->next--;

	/* decompress raster data */
	image = InflateRaster(html, ib, img_data->width, img_data->height);

	/*****
	* Decompression failed. No warning message as the decoders will
	* spit one out.
	*****/
	if(image == NULL)
		return(False);

	/* convert interlaced image data to normal image data */
	if(BitSet(buf[8], INTERLACE))
		image = DoImage(image, img_data->width, img_data->height);

	/* save ptr */
	img_data->data = image;

	/* our timeouts are in milliseconds */
	*timeout = Gif89.delayTime*10;
	/* and they are less than zero sometimes! */
	*timeout = Abs(*timeout);
	*dispose = Gif89.disposal;
	img_data->bg = Gif89.transparent;

	_XmHTMLDebug(15, ("readGIF.c, _XmHTMLGifAnimNextFrame, frame %i read:\n"
		"\tx_offset: %i, y_offset: %i\n\twidth: %i, height: %i\n"
		"\tbackground: %i, timeout: %i, dispose: %i\n", current_frame, 
		*x, *y, img_data->width, img_data->height, img_data->bg, *timeout,
		*dispose));

#ifdef DEBUG
	current_frame++;
#endif
	return((img_data->data != NULL ? True : False));
}

/*****
* Name: 		_XmHTMLReadGIF
* Return Type: 	XmHTMLRawImageData*
* Description: 	read a gif file
* In: 
*	html:		XmHTMLWidget
*	ib:			image memory buffer
* Returns:
*	image data upon success. Null on failure
*****/
XmHTMLRawImageData*
_XmHTMLReadGIF(Widget html, ImageBuffer *ib)
{
	Byte buf[16], c, localColorMap[3][XmHTML_MAX_IMAGE_COLORS], *image;
	int useGlobalColormap, bitPixel;
	int imageCount = 0, imageNumber = 1, grayscale;
	register int i;
	static XmHTMLRawImageData *img_data;

	/* check if zlib has been compiled in */
#if !defined(HAVE_LIBPNG) && !defined(HAVE_LIBZ)
	if(ib->type == IMAGE_GZF || ib->type == IMAGE_GZFANIM ||
		ib->type == IMAGE_GZFANIMLOOP)
		return(NULL);
#endif

	/*
	* Initialize GIF89 extensions
	*/
	Gif89.transparent = -1;
	Gif89.delayTime = -1;
	Gif89.inputFlag = -1;
	Gif89.disposal = 0;

	/*
	* When we get here we already know this is a gif image, so we can
	* safely skip the magic check 
	*/
	ib->next = 6;

	/* read logical screen descriptor */
	(void)_XmHTMLGifReadOK(ib,buf,7);

	GifScreen.Width	          = LM_to_uint(buf[0],buf[1]);
	GifScreen.Height	      = LM_to_uint(buf[2],buf[3]);
	GifScreen.BitPixel	      = 2<<(buf[4]&0x07);
	GifScreen.ColorResolution = (((buf[4]&0x70)>>3)+1);
	GifScreen.Background      = buf[5];
	GifScreen.AspectRatio     = buf[6];

	/*
	* Allocate raw image data, can't use AllocRawImage as the image data
	* will be allocated for us
	*/
	img_data = (XmHTMLRawImageData*)malloc(sizeof(XmHTMLRawImageData));
	ResetRawImage(img_data);
	img_data->width      = GifScreen.Width;
	img_data->height     = GifScreen.Height;

	/* allocate colormap */
	AllocRawImageCmap(img_data, GifScreen.BitPixel);

	if(BitSet(buf[4], LOCALCOLORMAP))
	{
		/* Global Colormap */
		if(ReadColorMap(ib,GifScreen.BitPixel,GifScreen.ColorMap, &grayscale))
		{
			FreeRawImage(img_data);
			_XmHTMLWarning(__WFUNC__(html, "_XmHTMLReadGIF"), XMHTML_MSG_106,
				ib->file, "global");
			return(NULL);	/* can't read colormap */
		}

		for (i=0; i < GifScreen.BitPixel; i++)
		{
			/* upscale to 0-2^16 */
			img_data->cmap[i].red = GifScreen.ColorMap[0][i] << 8;
			img_data->cmap[i].green = GifScreen.ColorMap[1][i] << 8;
			img_data->cmap[i].blue = GifScreen.ColorMap[2][i] << 8;
		}
	}

	image = (Byte*)NULL;

	while(image == NULL)
	{
		if(!_XmHTMLGifReadOK(ib,&c,1))
			return(NULL);

		if (c == ';') /* GIF terminator */
		{
			if (imageCount < imageNumber)
			{
				FreeRawImage(img_data);
				_XmHTMLWarning(__WFUNC__(html, "_XmHTMLReadGIF"),
					XMHTML_MSG_107, ib->file, "pixel data");
				return(NULL);
			}
			break;
		}

		if(c == '!') /* Extension */
		{
			if(!_XmHTMLGifReadOK(ib,&c,1))
			{
				FreeRawImage(img_data);
				_XmHTMLWarning(__WFUNC__(html, "_XmHTMLReadGIF"),
					XMHTML_MSG_107, ib->file, "extension block type");
				return(NULL);
			}
			(void)DoExtension(ib, c);
			continue;
		}

		if (c != ',')
			continue; /* Not a valid start character */

		++imageCount;

		if(!_XmHTMLGifReadOK(ib,buf,9))
		{
			FreeRawImage(img_data);
			_XmHTMLWarning(__WFUNC__(html, "_XmHTMLReadGIF"),
				XMHTML_MSG_107, ib->file, "image descriptor");
			return(NULL);
		}

		useGlobalColormap = ! BitSet(buf[8], LOCALCOLORMAP);

		bitPixel = 1<<((buf[8]&0x07)+1);

		/*
		* We only want to set width and height for the imageNumber
		* we are requesting.
		*/
		if(imageCount == imageNumber)
		{
			img_data->width  = LM_to_uint(buf[4],buf[5]);
			img_data->height = LM_to_uint(buf[6],buf[7]);
		}

		/* read and fill a local or glocal colormap */
		if(!useGlobalColormap)
		{
			if(ReadColorMap(ib, bitPixel, localColorMap, &grayscale))
			{
				FreeRawImage(img_data);
				_XmHTMLWarning(__WFUNC__(html, "_XmHTMLReadGIF"),
					XMHTML_MSG_106, ib->file, "local");
				return(NULL);
			}

			/*
			* We only want to set the data for the
			* imageNumber we are requesting.
			*/
			if(imageCount == imageNumber)
			{
				/* see if we already have a colormap */
				if(img_data->cmap)
				{
					/* image has both global and local colormap */
					if(img_data->cmapsize != bitPixel)
					{
						/* free current colormap */
						free(img_data->cmap);
						/* add a new one */
						AllocRawImageCmap(img_data, bitPixel);
					}
				}
				else
					AllocRawImageCmap(img_data, bitPixel);
				CopyColormap(img_data->cmap, bitPixel, localColorMap);
			}
		}
		else
		{
			if(imageCount == imageNumber)
				CopyColormap(img_data->cmap, GifScreen.BitPixel,
					GifScreen.ColorMap); 
		}

		/* get image data for this image */
		if(imageCount == imageNumber)
		{
			/* get image depth (= codeSize in GIF images) */
			(void)_XmHTMLGifReadOK(ib,&c,1);
			ib->depth = (int)(c & 0xff);
			ib->next--;

			/* decompress raster data */
			image = InflateRaster((XmHTMLWidget)html, ib, img_data->width,
				img_data->height);

			/*****
			* Decompression failed. No warning message as the decoders will
			* spit one out
			*****/
			if(image == NULL)
			{
				FreeRawImage(img_data);
				return(NULL);
			}
			/* convert interlaced image data to normal image data */
			if(BitSet(buf[8], INTERLACE))
				image = DoImage(image, img_data->width, img_data->height);
		}
		else
			SkipImage(ib);	/* skip it */

	}
	img_data->bg = Gif89.transparent;
	img_data->data = image;
	img_data->color_class = (grayscale != 0 ?
		XmIMAGE_COLORSPACE_GRAYSCALE : XmIMAGE_COLORSPACE_INDEXED);
	return(img_data);
}

/*****
* Name: 		ReadColorMap
* Return Type: 	int
* Description: 	reads a GIF colormap
* In: 
*	ib:			current ImageBuffer;
*	number:		no of colors to read;
*	buffer:		colormap buffer;
* Returns:
*	0 on success and buffer is filled. 1 on failure.
*****/
static int
ReadColorMap(ImageBuffer *ib, int number,
	Byte buffer[3][XmHTML_MAX_IMAGE_COLORS], int *gray)
{
	register int i;
	Byte rgb[3];
	int is_gray = 0;

	for(i = 0; i < number; ++i)
	{
		if(!_XmHTMLGifReadOK(ib, rgb, sizeof(rgb)))
			return(1);

		buffer[CM_RED][i]   = rgb[0];
		buffer[CM_GREEN][i] = rgb[1];
		buffer[CM_BLUE][i]  = rgb[2];
		is_gray &= (rgb[0] == rgb[1] && rgb[1] == rgb[2]);
	}
	/* zero remainder */
	for(; i < XmHTML_MAX_IMAGE_COLORS; i++)
	{
		buffer[CM_RED][i]   = 0;
		buffer[CM_GREEN][i] = 0;
		buffer[CM_BLUE][i]  = 0;
	}
	*gray = is_gray;
	return(0);
}

/*****
* Name: 		DoExtension
* Return Type: 	int
* Description: 	process a gif extension block
* In: 
*	ib:			current ImageBuffer;
*	label:		extension block identifier;
* Returns:
*	gif type code upon success or IMAGE_UNKNOWN upon error.
*****/
static int
DoExtension(ImageBuffer *ib, int label)
{
	static char	buf[256];
	int ret_val = (int)IMAGE_GIF;

	switch(label)
	{
		case 0x01:		/* Plain Text Extension */
			break;
		case 0xff:		/* Application Extension */
			/*
			* Netscape Looping extension
			* Get first block
			*/
			(void)_XmHTMLGifGetDataBlock(ib, (Byte*)buf);
			if(!(strncmp((char*)buf, "NETSCAPE2.0", 11)))
			{
				ret_val = (int)IMAGE_GIFANIMLOOP;
				if((_XmHTMLGifGetDataBlock(ib, (Byte*)buf)) <= 0)
					ret_val = (int)IMAGE_UNKNOWN;	/* corrupt animation */
				else
					Gif89.loopCount = LM_to_uint(buf[1], buf[2]);
			}
			break;
		case 0xfe:		/* Comment Extension */
			while(_XmHTMLGifGetDataBlock(ib, (Byte*) buf) > 0);
			return(ret_val);
		case 0xf9:		/* Graphic Control Extension */
			(void)_XmHTMLGifGetDataBlock(ib, (Byte*) buf);
			Gif89.disposal    = (buf[0] >> 2) & 0x7;
			Gif89.inputFlag   = (buf[0] >> 1) & 0x1;
			Gif89.delayTime   = LM_to_uint(buf[1],buf[2]);
			if ((buf[0] & 0x1) != 0)
				Gif89.transparent = (int)((Byte)buf[3]);

			while(_XmHTMLGifGetDataBlock(ib, (Byte*) buf) > 0);
			return(ret_val);
		default:
			break;
	}
	while(_XmHTMLGifGetDataBlock(ib, (Byte*) buf) > 0);

	return(ret_val);
}

/*****
* Name: 		InflateGZFInternal
* Return Type: 	Byte*
* Description: 	uncompress deflated raster data
* In: 
*	ib:			current imageBuffer
*	dsize:		size of inflated (decompressed) raster data.
*	*nread:		real size of uncompressed image data.
* Returns:
*	uncompressed data upon success, NULL on failure.
*****/
static Byte*
InflateGZFInternal(ImageBuffer *ib, int dsize, int *nread)
{
	/* check if zlib has been compiled in */
#if !defined(HAVE_LIBPNG) && !defined(HAVE_LIBZ)
	*nread = 0;
	return(NULL);
#else
	static Byte *output_buf;
	Byte buf[256], c;
	z_stream stream;
	int err;

	*nread = 0;

	/* Skip code size, its never used */
	(void)_XmHTMLGifReadOK(ib, &c, 1);

	/* allocate output buffer */
	output_buf = (Byte*)calloc(dsize+1, sizeof(Byte));

	/* initialize inflate stuff */
	stream.zalloc = Z_NULL;
	stream.zfree  = Z_NULL;

	if((err = inflateInit(&stream)) != Z_OK)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "InflateGZFInternal"),
			XMHTML_MSG_108, ib->file, "Init", stream.msg);
		free(output_buf);
		return(NULL);
	}

	/* keep uncompressing until we reach the end of the compressed stream */
	while(True)
	{
		/* next compressed block of data */
		stream.avail_in = _XmHTMLGifGetDataBlock(ib, buf);
		stream.next_in  = buf;
		/* update output buffer */
		stream.next_out = output_buf + stream.total_out;
		stream.avail_out = dsize - stream.total_out;

		/* uncompress it */
		err = inflate(&stream, Z_PARTIAL_FLUSH);	

		/* check return value */
		if(err != Z_OK && err != Z_STREAM_END)
		{
			_XmHTMLWarning(__WFUNC__(NULL, "InflateGZFInternal"),
				XMHTML_MSG_108, ib->file, "", stream.msg);
			free(output_buf);
			return(NULL);
		}
		/* and break if inflate has finished uncompressing our data. */
		if(err == Z_STREAM_END)
			break;
	}
	/*
	* Skip remaining data. The deflate format signals the end of compressed
	* data itself, so we never reach the zero-data block in the above loop.
	*/
	while((_XmHTMLGifGetDataBlock(ib, (Byte*)buf)) > 0);

	/* deallocate zstream data */
	if((inflateEnd(&stream)) != Z_OK)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "InflateGZFInternal"),
			XMHTML_MSG_108, ib->file, "End",
			stream.msg ? stream.msg : "(unknown zlib error)");
	}
	*nread = stream.total_out;

	/* successfull uncompress, return uncompressed image data */
	return(output_buf);
#endif
}

/*****
* Name: 		InflateGIFInternal
* Return Type: 	Byte*
* Description: 	decodes LZW compressed raster data without using an LZW
*				decoder by using the "compress" utilitity.
* In: 
*	ib:			current imageBuffer
*	dsize:		size of inflated (decompressed) raster data.
*	*nread:		real size of uncompressed image data.
* Returns:
*	uncompressed data upon success, NULL on failure.
*****/
static Byte*
InflateGIFInternal(ImageBuffer *ib, int dsize, int *nread)
{
	LZWStream *lzw;
	static Byte *data;

	*nread = 0;

	/* create a new stream object */
	lzw = LZWStreamCreate(ib, zCmd);

	/* set read functions */
	lzw->readOK  = _XmHTMLGifReadOK;
	lzw->getData = _XmHTMLGifGetDataBlock;

	/* initialize uncompression */
	if((LZWStreamInit(lzw)) <= 0)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "InflateGIFInternal"), lzw->err_msg);
		LZWStreamDestroy(lzw);
		return(NULL);
	}

	/* convert data */
	LZWStreamConvert(lzw);

	/* get uncompressed data */
	if((data = LZWStreamUncompress(lzw, nread)) == NULL)
		_XmHTMLWarning(__WFUNC__(NULL, "InflateGIFInternal"), lzw->err_msg);

	/* destroy stream */
	LZWStreamDestroy(lzw);

	/* and return uncompressed data */
	return(data);
}

static Byte*
InflateGIFExternal(XmImageGifProc inflate, ImageBuffer *ib,
	int dsize, int *nread)
{
	static Byte *buffer;
	Byte buf[256], c;
	XmHTMLGIFStream gstream;
	int err;

	*nread = 0;

	/* get code size */
	(void)_XmHTMLGifReadOK(ib, &c, 1);

	/* allocate output buffer */
	buffer = (Byte*)calloc(dsize+1, sizeof(Byte));

	/* initialize GIFStream object */
	memset(&gstream, 0, sizeof(XmHTMLGIFStream));

	gstream.codesize  = (int)c;
	gstream.state     = GIF_STREAM_INIT;
	gstream.next_out  = buffer;
	gstream.avail_out = dsize + 1;
	gstream.is_progressive = False;

	/*****
	* and call external decoder so it can initialize its own data
	* structures
	*****/
	if((inflate(&gstream)) != GIF_STREAM_OK)
	{
		if(gstream.msg != NULL)
		{
			_XmHTMLWarning(__WFUNC__(NULL, "InflateGIFExternal"),
				XMHTML_MSG_109, ib->file,
				gstream.msg ? gstream.msg : "(unknown error)");
		}
		return(NULL);
	}
	gstream.state = GIF_STREAM_OK;

	/* keep uncompressing until we reach the end of the compressed stream */
	while(True)
	{
		/* next compressed block of data */
		gstream.avail_in  = _XmHTMLGifGetDataBlock(ib, buf);
		gstream.next_in   = buf;
		/* update output buffer */
		gstream.next_out  = buffer + gstream.total_out;
		gstream.avail_out = dsize - gstream.total_out;

		/* uncompress it */
		err = inflate(&gstream);	

		/* check return value */
		if(err != GIF_STREAM_OK && err != GIF_STREAM_END)
		{
			_XmHTMLWarning(__WFUNC__(NULL, "InflateGIFExternal"),
				XMHTML_MSG_110, gstream.msg ? gstream.msg : "(unknown error)");
			break;
		}
		/* and break if inflate has finished uncompressing our data. */
		if(err == GIF_STREAM_END || gstream.total_out == dsize)
			break;
	}
	/* total size of uncompressed data */
	*nread = gstream.total_out;

	/* Skip remaining data. */
	while((_XmHTMLGifGetDataBlock(ib, (Byte*)buf)) > 0);

	/* we have an external decoder, tell it to destroy itself */
	gstream.state     = GIF_STREAM_END;
	gstream.next_out  = NULL;
	gstream.avail_out = 0;
	gstream.next_in   = NULL;
	gstream.avail_in  = 0;

	/* call it */
	(void)inflate(&gstream);

	/* successfull uncompress, return uncompressed image data */
	return(buffer);
}

/*****
* Name: 		InflateRaster
* Return Type: 	Byte
* Description: 	GIF/GZF raster data decompressor driver. Selects the
*				appropriate decompressor to use for decompressing the GIF/GZF
*				LZW/deflate compressed raster data.
* In: 
*	html:		XmHTMLWidget id, used for selecting appropriate decompressor;
*	ib:			raw image data;
*	width:		width of decompressed image;
*	height:		height of decompressed image;
* Returns:
*	decompressed raster data in an allocated buffer. NULL upon failure.
*****/
static Byte*
InflateRaster(XmHTMLWidget html, ImageBuffer *ib, int width, int height)
{
	static Byte *data;
	int nread = 0;
	int dsize = width*height;	/* estimated decompressed data size */

	/*****
	* Uncompress image data according to image type. GZF images always
	* use the internal decoder, no overriding possible.
	* GIF images can choose between the external or internal decoder.
	*****/
	if(ib->type == IMAGE_GZF || ib->type == IMAGE_GZFANIM ||
		ib->type == IMAGE_GZFANIMLOOP)
		data = InflateGZFInternal(ib, dsize, &nread);
	else
	{
		if((html && XmIsHTML((Widget)html) && html->html.gif_proc) ||
			XmImageGifProc_plugin != NULL)
		{
			XmImageGifProc inflate;
			if(html && XmIsHTML((Widget)html) && html->html.gif_proc)
				inflate = html->html.gif_proc;
			else
				inflate = XmImageGifProc_plugin;

			data = InflateGIFExternal(inflate, ib, dsize, &nread);
		}
		else
		{
			/* uncompress command to use for LZWStream */
			if(html && XmIsHTML((Widget)html))
				zCmd = html->html.zCmd;
			else
				zCmd = XmImageGifzCmd_plugin;

			data = InflateGIFInternal(ib, dsize, &nread);
		}
	}

	/* too bad if conversion or uncompress failed */
	if(nread != dsize || data == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "InflateRaster"),
			XMHTML_MSG_104, ib->file, nread, dsize);
		if(data)
			free(data);
		data = NULL;
	}
	return(data);
}

/*****
* Name: 		CopyColormap
* Return Type: 	void
* Description: 	copies the colormap from a gif image to a private colormap
* In: 
*	colrs:		destination colormap;
*	cmapSize:	size of gif colormap;
*	cmap:		gif colormap;
* Returns:
*	nothing, but cmap has been copied to colrs. All color components
*	are scaled to the range 0-2^16
*****/
static void
CopyColormap(XCOLOR *colrs, int cmapSize, Byte cmap[3][XmHTML_MAX_IMAGE_COLORS])
{
	register int i;

	/* copy colormap */
	for(i = 0; i < cmapSize; i++)
	{
		GETR(colrs[i]) = (cmap[CM_RED][i]) << 8;
		GETG(colrs[i]) = (cmap[CM_GREEN][i]) << 8;
		GETB(colrs[i]) = (cmap[CM_BLUE][i]) << 8;
	}
}

/*
* interlaced image. Need to alternate uncompressed data to create the
* actual image.
*/
static Byte *
DoImage(Byte *data, int len, int height)
{
	static Byte *image;
	int xpos = 0, ypos = 0, pass = 0, step = 8;
	register int i;
	register Byte *dp, *dPtr;

	/* sanity check */
	if(data == NULL)
		return(NULL);

	dPtr = data;

	/* allocate image storage */
	image = (Byte *)calloc(len * height, sizeof(char));

	for(i = 0; i < height; i++)
	{
		if(ypos < height)
		{
			dp = &image[len * ypos];
			for(xpos = 0; xpos < len; xpos++)
				*dp++ = *dPtr++;
		}
		if((ypos += step) >= height)
		{
			if (pass++ > 0)
				step /= 2;
			ypos = step / 2;
		}
	}
	/* no longer needed */
	free(data);
	return(image);
}

/*******
*** GIF to GZF converter routines.
********/

#if defined(HAVE_LIBPNG) || defined(HAVE_LIBZ)
static void
writeColormap(ImageBuffer *ib, FILE *fp, int nentries)
{
	Byte rgb[3];
	int i = 0;
	for(; i < nentries; i++)
	{
		(void)_XmHTMLGifReadOK(ib, (Byte*)rgb, sizeof(rgb));
		WriteOK(fp, rgb, sizeof(rgb));
	}
}

static void
writeImage(Byte *image, FILE *fp, int size, int codeSize)
{
	Byte buf[256], *compressed;
	Byte *inPtr;
	int i = 0, j = 0;
	unsigned long csize;

	/* save codeSize as well */
	WriteOK(fp, &codeSize, 1);

	/* compress image data in one go */
	
	/* first allocate destination buffer */
	csize = size + (int)(0.15*size) + 12;
	compressed = (Byte*)malloc(csize*sizeof(Byte));

	if((compress(compressed, &csize, image, size)) != Z_OK)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "writeImage"), XMHTML_MSG_111);
		free(compressed);
		/* put block terminator */
		j = 0;
		WriteOK(fp, &j, 1);
		return;
	}
	inPtr = compressed;

	/* save image data in chunks of 255 bytes max. */
	for(i = 0; i < (int)csize ; i++)
	{
		buf[j++] = *inPtr++;
		if(j == 254)
		{
			fputc(j&0xff, fp);
			WriteOK(fp, buf, 254);
			j = 0;
		}
	}
	/* flush out remaining data */
	if(j)
	{
		WriteOK(fp, &j, 1);
		WriteOK(fp, buf, j);
	}

	_XmHTMLDebug(15, ("readGIF.c: Compressed %i bytes into %i bytes\n",
		size, (int)csize));

	/* and write the block terminator */
	j = 0;
	WriteOK(fp, &j, 1);

	free(compressed);
}

/*****
* Name:			GifToGzf
* Return Type:	Boolean
* Description:	converts a Gif87a, Gif89a, to Gzf87a, Gzf89a.
* In:
*	ib:			image memory buffer
*	file:		output filename
* Returns:
*	True when succesfull conversion, False otherwise.
*****/
static Boolean
GifToGzf(ImageBuffer *ib, String file)
{
	FILE *fp;
	Byte buf[256], c;	/* blocks in a Gif image contain max. 256 bytes */
	int i, done = 0, w, h, imageCount = 0, codeSize;
	Byte *image;

	if((fp = fopen(file, "w")) == NULL)
	{
		perror(file);
		return(False);
	}

	/* gif magic */
	(void)_XmHTMLGifReadOK(ib, buf, 6);
	if(!(strncmp((char*)buf, "GIF87a", 6)))
	{
		strcpy((char*)buf, "GZF87a");
		buf[6] = '\0';
		WriteOK(fp, buf, 6);
	}
	else if(!(strncmp((char*)buf, "GIF89a", 6)))
	{
		strcpy((char*)buf, "GZF89a");
		buf[6] = '\0';
		WriteOK(fp, buf, 6);
	}
	else
	{
		fclose(fp);
		unlink(file);
		return(False);
	}

	/* logical screen descriptor */
	(void)_XmHTMLGifReadOK(ib, buf, 7);
	WriteOK(fp, buf, 7);

	if(BitSet(buf[4], LOCALCOLORMAP))
	{
		/* colormap has this many entries of 3 bytes */
		writeColormap(ib, fp, 2<<(buf[4]&0x07));
	}

	while(True && !done)
	{
		/* block identifier */
		if(!_XmHTMLGifReadOK(ib,&c,1))
		{
			done = -1;
			break;
		}
		/* save it */
		WriteOK(fp, &c, 1);

		/* GIF terminator */
		if(c == ';')
		{
			_XmHTMLDebug(15, ("GifToGzf.c: got GIF terminator\n"));
			done = 1;
			break;
		}

		/* Extension */
		if(c == '!')
		{
			/* error */
			if(!_XmHTMLGifReadOK(ib,&c,1))
			{
				done = -1;
				break;
			}
			WriteOK(fp, &c, 1);

			while((i = _XmHTMLGifGetDataBlock(ib, (Byte*) buf)) > 0)
			{
				WriteOK(fp, &i, 1);
				WriteOK(fp, buf, i);
			}
			/* and write zero block terminator */
			c = 0;
			WriteOK(fp, &c, 1);
			continue;
		}

		if (c != ',')
			continue; /* Not a valid start character */

		/* image descriptor */
		if(!_XmHTMLGifReadOK(ib,buf,9))
		{
			/* error */
			done = -1;
			break;
		}
		WriteOK(fp, buf, 9);

		/* we have a local colormap */
		if(BitSet(buf[8], LOCALCOLORMAP))
			writeColormap(ib, fp, 1<<((buf[8]&0x07)+1));

		/* width and height for this particular frame */
		w = LM_to_uint(buf[4],buf[5]);
		h = LM_to_uint(buf[6],buf[7]);

		/* get image codeSize */
		(void)_XmHTMLGifReadOK(ib,&c,1);
		codeSize = (int)c;
		ib->next--;

		/* and convert the image data */
		if((image = InflateGIFInternal(ib, w*h, &i)) != NULL)
		{
			_XmHTMLDebug(15, ("GifToGzf.c: bytes expected: %i, retrieved: %i\n",
				w*h, i));

			writeImage(image, fp, i, codeSize);
			/* and free image data */
			free(image);

			_XmHTMLDebug(15, ("GifToGzf.c: converted image %i\n", imageCount));
		}
		else
			done = -1;
		++imageCount;
	}
	/* close output file */
	fclose(fp);

	/* and remove if we had an error */
	if(done == -1)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "GifToGzf"), XMHTML_MSG_112, ib->file);
		unlink(file);
		return(False);
	}
	return(True);
}

/*****
* Name: 		XmHTMLGIFtoGZF
* Return Type: 	Boolean
* Description: 	converts a CompuServe gif image to a GZF image.
* In: 
*	infile:		name of giffile;
*	buf:		giffile data;
*	size:		size of buffer (if any);
*	outfile:	name of destination file;
* Returns:
*	True when conversion was successfull, False if something failed.
*****/
Boolean
XmHTMLGIFtoGZF(String infile, unsigned char *buf, int size, String outfile)
{
	ImageBuffer data, *ib = NULL;
	Boolean ret_val = False;

	/* sanity check */
	if(size == 0 && infile == NULL)
		return(ret_val);

	if(size == 0)
	{
		if((ib = _XmHTMLImageFileToBuffer(infile)) == NULL)
			return(ret_val);
	}
	else
	{
		if(buf != NULL)
		{
			data.file = "<internally buffered image>";
			data.buffer = (Byte*)buf;
			data.size = (size_t)size;
			data.next = 0;
			data.may_free = False;
			ib = &data;
		}
		else
			return(ret_val);
	}

	_XmHTMLDebug(15, ("XmHTMLGIFtoGZF: converting GIF %s to GZF.\n", ib->file));

	/* and convert it */
	ret_val = GifToGzf(ib, outfile);

	_XmHTMLDebug(15, ("XmHTMLGIFtoGZF: image converted.\n"));

#ifdef DEBUG
	if(ret_val)
	{
		FILE *f;
		int osize = 0;

		/* get size increase/reduction */
		f = fopen(outfile, "r");
		fseek(f, 0L, SEEK_END);
		osize = ftell(f);
		fclose(f);
		_XmHTMLDebug(15, ("XmHTMLGIFtoGZF: size reduction: %5.2f%%\n",
			(1 - (float)(osize/(float)ib->size)) * 100));
	}
#endif

	/* release buffer */
	FreeImageBuffer(ib);

	return(ret_val);
}
#else

/* Empty func if zlib has not been compiled in. */
/* ARGSUSED */
Boolean
XmHTMLGIFtoGZF(String infile, unsigned char *buf, int size, String outfile)
{
	return(False);
}

#endif /* defined(HAVE_LIBPNG) || defined(HAVE_LIBZ) */
