#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* readGIFplc.c :		GIF/GZF progressive loading interfaces
*
* This file Version	$Revision$
*
* Creation date:		Fri Jun 13 16:31:35 GMT+0100 1997
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
* Revision 1.6  1998/04/27 07:02:56  newt
* tka stuff
*
* Revision 1.5  1998/04/04 06:28:32  newt
* XmHTML Beta 1.1.3
*
* Revision 1.4  1997/10/23 00:25:21  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.3  1997/08/31 17:41:48  newt
* cast fix on LZWStreamUncompress. Debug level change.
*
* Revision 1.2  1997/08/30 01:29:39  newt
* _XmHTMLWarning proto and LZWStream changes.
*
* Revision 1.1  1997/08/01 12:51:55  newt
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
#include "plc.h"

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/
#define INTERLACE		0x40
#define LOCALCOLORMAP	0x80

/*** Private Function Prototype Declarations ****/
#define BitSet(byte, bit)		(((byte) & (bit)) == (bit))
#define LM_to_uint(a,b)			((((b)&0xFF) << 8) | ((a)&0xFF))

static Boolean DoExtension(PLC *plc, int label);
static Boolean ReadColormap(PLC *plc, PLCImageGIF *gif);
static Boolean DoImage(PLCImage *gif, Byte *input);

/*** Private Variable Declarations ***/

/*****
* Name: 		_PLC_GIF_Init
* Return Type: 	void
* Description: 	image initializer for GIF images
* In:
*	plc:		current PLC
* Returns:
*	Nothing but PLC is updated.
* Note:
*	As this routine must be fully re-entrant, it needs a lot of checks
*	to make sure we have the data we want fully available.
*	The drawback is that if we are being suspended while doing this
*	initialization, everything must be reset and repeated the next time
*	this routine is called.
*****/
void
_PLC_GIF_Init(PLC *plc)
{
	Byte buf[16], c;
	PLCImageGIF *gif;

	gif = &(plc->object->plc_gif_image);

	_XmHTMLDebug(15, ("plc.c: _PLC_GIF_Init for %s\n", plc->url));

	/* this plc is active */
	plc->plc_status = PLC_ACTIVE;

	/*****
	* When this routine is called, the init method of this PLC has already
	* been called to determine the type of this PLC Image object. Therefore
	* we already have data available and we need to rewind the input buffer
	* back to the beginning.
	*****/
	_PLCRewindInputBuffer(plc);

	/* we know this is a gif image, so skip magic */
	gif->info->type = IMAGE_GIF;
	(void)_PLCReadOK(plc, buf, 6);

	/* read logical screen descriptor */
	(void)_PLCReadOK(plc, buf, 7);

	/* image dimensions */
	gif->width   = LM_to_uint(buf[0],buf[1]);
	gif->height  = LM_to_uint(buf[2],buf[3]);

	/* set colorspace and allocate a colormap */
	gif->colorclass = XmIMAGE_COLORSPACE_INDEXED;
	gif->cmapsize   = 2<<(buf[4]&0x07);

	/*
	* We may have been called before (but returned 'cause not enough data
	* was available).
	*/
	if(gif->cmap == NULL)
		gif->cmap = (XCOLOR*)calloc(gif->cmapsize, sizeof(XCOLOR));

	/* image is initially fully opaque */
	gif->transparency = XmNONE;
	gif->bg_pixel = -1;

	/*
	* Incoming data buffer. This is *way* too much as the incoming data
	* will be compressed (but it does make sure there is enough room)
	*/
	gif->buf_size   = gif->width*gif->height;
	gif->buf_pos    = 0;	/* current pos in data received so far */
	gif->byte_count = 0;	/* size of data received so far */
	if(gif->buffer == NULL)
		gif->buffer = (Byte*)calloc(gif->buf_size + 1, sizeof(Byte));

	/* check if a global colormap is available */
	if(BitSet(buf[4], LOCALCOLORMAP))
	{
		if(!(ReadColormap(plc, gif)))
		{
			/* premature end of data. */
			if(plc->plc_data_status == STREAM_END)
			{
				_XmHTMLWarning(__WFUNC__(gif->owner, "_PLC_GIF_Init"),
					XMHTML_MSG_106, plc->url, "global");
				plc->plc_status = PLC_ABORT;
			}
			return;	/* no global colormap! */
		}
	}

	/* process all extensions */
	c = 0;
	while(c != ',')
	{
		if(!_PLCReadOK(plc, &c, 1))
			return;

		if (c == ';') /* GIF terminator */
		{
			_XmHTMLWarning(__WFUNC__(gif->owner, "_PLC_GIF_Init"),
				XMHTML_MSG_107, plc->url, "pixel data");
			plc->plc_status = PLC_ABORT;
			return;
		}

		if(c == '!') /* Extension */
		{
			if(!_PLCReadOK(plc,&c,1))
			{
				if(plc->plc_data_status == STREAM_END)
				{
					_XmHTMLWarning(__WFUNC__(gif->owner, "_PLC_GIF_Init"),
						XMHTML_MSG_107, plc->url, "extension block type");
					plc->plc_status = PLC_ABORT;
				}
				return;
			}
			if(!(DoExtension(plc, c)))
			{
				if(plc->plc_data_status == STREAM_END)
				{
					_XmHTMLWarning(__WFUNC__(gif->owner, "_PLC_GIF_Init"),
						XMHTML_MSG_107, plc->url, "extension block");
					plc->plc_status = PLC_ABORT;
				}
				return;
			}
			continue;
		}
		if (c != ',')
			continue; /* Not a valid start character */
	}
	/* get image descriptor */
	if(!_PLCReadOK(plc, buf, 9))
		return;

	/* see if we are to use a local colormap */
	if(BitSet(buf[8], LOCALCOLORMAP))
	{
		/* local colormap size */
		gif->ncolors = 1<<((buf[8]&0x07)+1);

		/* do we also have a glocal colormap? */
		if(gif->cmap)
			free(gif->cmap);

		gif->cmapsize = gif->ncolors;
		gif->cmap = (XCOLOR*)calloc(gif->cmapsize, sizeof(XCOLOR));

		if(!(ReadColormap(plc, gif)))
		{
			/* premature end of data. */
			if(plc->plc_data_status == STREAM_END)
			{
				_XmHTMLWarning(__WFUNC__(gif->owner, "_PLC_GIF_Init"),
					XMHTML_MSG_106, plc->url, "local");
				plc->plc_status = PLC_ABORT;
			}
			return;	/* no global colormap! */
		}
	}
	gif->ncolors = gif->cmapsize;

	/* sanity check: image *must* have a colormap */
	if(gif->cmap == NULL)
	{
		_XmHTMLWarning(__WFUNC__(gif->owner, "_PLC_GIF_Init"),
			XMHTML_MSG_106, plc->url, "global or local");
		plc->plc_status = PLC_ABORT;
		return;	/* no global colormap! */
	}

	/* image depth (= codeSize in GIF images, unused in GZF images) */
	if(!(_PLCReadOK(plc, &c, 1)))
		return;

	gif->depth = (int)(c & 0xff);

	/* check interlacing */
	if(BitSet(buf[8], INTERLACE))
	{
		/* interlaced gifs require 4 passes and use an initial rowstride of 8 */
		gif->npasses = 4;
		gif->stride  = 8;
	}
	else
	{
		/* regular gif, 1 pass will get us the entire image */
		gif->npasses = 1;
		gif->stride  = 0;
	}
	gif->curr_pass = 0;
	gif->curr_scanline = 0;

	/*****
	* This routine is also used for GZF images, so before initializing
	* the LZWStream object we need to make sure we have been called for
	* a true GIF image.
	*****/
	if(plc->object->type == plcGIF)
	{
		XmHTMLWidget html  = plc->object->plc_any.owner;

		if(HTML_ATTR(gif_proc) != NULL)
		{
			gif->external_codec = True;
			gif->inflate = HTML_ATTR(gif_proc);
			if((gif->gstream =
				(XmHTMLGIFStream*)malloc(sizeof(XmHTMLGIFStream))) == NULL)
			{
				/* out of memory, too bad then */
				_XmHTMLWarning(__WFUNC__(gif->owner, "_PLC_GIF_Init"),
					XMHTML_MSG_113, plc->url, sizeof(XmHTMLGIFStream));
				plc->plc_status = PLC_ABORT;
				return;
			}

			/* initialize GIFStream object */
			memset(gif->gstream, 0, sizeof(XmHTMLGIFStream));

			gif->gstream->codesize  = (int)c;
			gif->gstream->state     = GIF_STREAM_INIT;
			gif->gstream->next_out  = gif->buffer;
			gif->gstream->avail_out = gif->buf_size + 1;
			gif->gstream->is_progressive = True;
			/*
			* and call external decoder so it can initialize its own data
			* structures
			*/
			if((gif->inflate(gif->gstream)) != GIF_STREAM_OK)
			{
				if(gif->gstream->msg != NULL)
				{
					_XmHTMLWarning(__WFUNC__(gif->owner, "_PLC_GIF_Init"),
						XMHTML_MSG_109, plc->url,
						gif->gstream->msg ? gif->gstream->msg :
						"(unknown error)");
				}
				/* external decoder initalization failed, abort and return */
				plc->plc_status = PLC_ABORT;
				return;
			}
			gif->gstream->state = GIF_STREAM_OK;
		}
		else
		{
			/* initialize local data buffer */
			gif->ib.file   = plc->url;
			gif->ib.buffer = gif->buffer;
			gif->ib.size   = 0;
			gif->ib.next   = 0;
			gif->ib.type   = IMAGE_GIF;
			gif->ib.depth  = gif->depth;
			gif->ib.may_free = False;

			/* initialize LZWStream object */
			if(gif->lstream == NULL)
			{
				if((gif->lstream = LZWStreamCreate(&(gif->ib),
									html->html.zCmd)) == NULL)
				{
					/* couldn't create stream, abort and return */
					plc->plc_status = PLC_ABORT;
					return;
				}
				/* set read functions */
				gif->lstream->readOK  = _XmHTMLGifReadOK;
				gif->lstream->getData = _XmHTMLGifGetDataBlock;
			}
			/* first byte in buffer is gif codesize */
			gif->ib.buffer[0] = c;
			gif->ib.size = 1;
		}
		/* allocate room for final image data */
		if(gif->data == NULL)
		{
			gif->data = (Byte*)calloc(gif->buf_size + 1, sizeof(Byte));

			/* don't allocate clipmask yet, it's done in the plc code */
		}
		gif->data_size = gif->buf_size;
		gif->data_pos  = 0;
	}

	/* object has been initialized */
	plc->initialized = True;

	plc->curr_obj_func = 0;	/* move to GIF scanline reader */
	return;
}

/*****
* Name: 		_PLC_GIF_ScanlineProc
* Return Type: 	void
* Description: 	GZF scanline processor (decompresses data and orders it)
* In:
*	plc:		current PLC
* Returns:
*	nothing.
*****/
void
_PLC_GIF_ScanlineProc(PLC *plc)
{
	Byte *input = NULL;
	PLCImageGIF *gif;
	Boolean done = False;
	int bytes_avail;

	_XmHTMLDebug(15, ("plc.c: _PLC_GIF_ScanlineProc for %s\n", plc->url));

	gif = &(plc->object->plc_gif_image);

	/*****
	* We can choose between two types of LZW decoders: the internal one or
	* an externally installed progressive decoder.
	* The internal decoder is fairly efficient for direct gif loading, but is
	* *HUGELY* slow for progressive gif loading: it forks of uncompress (or
	* gzip) each time LZWStreamUncompress is called. Therefore we collect
	* all data and call the internal decoder when all image data has been
	* received.
	* When an external gif decoder has been installed, we just proceed in the
	* same way as with the GZF scanline procedure: decode a chunk of data
	* each time and allow it to be transfered to the display.
	*****/
	if(gif->external_codec == False)
	{
		int len;
		Boolean have_all = False;
		bytes_avail = plc->left;

#ifndef PLC_WORKPROCS
		do
		{
#endif	/* !PLC_WORKPROCS */

			/*****
			* get a new block of compressed data. This will automatically make
			* a new request for input data when there isn't enough data left
			* in the current input buffer.
			*****/
			if((len = _PLCGetDataBlock(plc, gif->gbuf)) != 0)
			{
				/* store block size */
				gif->ib.buffer[gif->ib.size++] = (Byte)len;

				/* append newly received data */
				(void)memcpy(gif->ib.buffer + gif->ib.size,
					gif->gbuf, len);

				/* new buffer size */
				gif->ib.size += len;

				/* bytes left in current buffer (+1 for block header) */
				bytes_avail -= (len + 1);

				/* prevent image transfer function from doing anything */
				gif->prev_pos = gif->data_pos = 0;
			}
			else
			{
				if(plc->plc_status == PLC_SUSPEND ||
					plc->plc_status == PLC_ABORT)
					return;
				/*
				* if plc_status == PLC_ACTIVE, we have a zero data block which
				* indicates the raster data end. For now we just consider this
				* as the end signal and start decoding the raster data.
				* For gif animations this would be the place to move to the
				* next frame.
				*/
				have_all = True;

				/* plug in a zero length data block and GIF terminator */
				gif->ib.buffer[gif->ib.size++] = (Byte)0;
				gif->ib.buffer[gif->ib.size++] = (Byte)1;
				gif->ib.buffer[gif->ib.size++] = (Byte)';';

			}
#ifndef PLC_WORKPROCS
		}
		/* This test will be false if we made a new data request */
		while(bytes_avail == plc->left && have_all == False);
#endif	/* !PLC_WORKPROCS */

		/* we have got all image data, now decode it */
		if(have_all)
		{
			/* rewind image buffer */
			gif->ib.next = 0;

			/* now (re-)initialize the stream object */
			if((LZWStreamInit(gif->lstream)) <= 0)
			{
				/* this is an error */
				_XmHTMLWarning(__WFUNC__(plc->object->plc_any_image.owner,
					"_PLC_GIF_ScanlineProc"), gif->lstream->err_msg);
				plc->plc_status = PLC_ABORT;
				return;
			}

			/* convert data */
			LZWStreamConvert(gif->lstream);

			/* get uncompressed data */
			if((input = LZWStreamUncompress(gif->lstream,
				(int*)&gif->byte_count)) == NULL)
			{
				_XmHTMLWarning(__WFUNC__(plc->object->plc_any_image.owner,
					"_PLC_GIF_ScanlineProc"), gif->lstream->err_msg);
				/* rather fatal... */
				_XmHTMLWarning(__WFUNC__(plc->object->plc_any_image.owner,
					"_PLC_GIF_ScanlineProc"), XMHTML_MSG_114, plc->url);
				plc->plc_status = PLC_ABORT;
				return;
			}

			/* convert input data to raw image data */
			(void)DoImage((PLCImage*)gif, input);

			/* free input buffer */
			free(input);

			/* we are finished here */
			plc->obj_funcs_complete = True;
		}
	}
	else
	{
		/*****
		* External GIF decoder installed, proceeds in the same way as the
		* GZF decoder, except that now the external decoder is called
		* instead of the zlib routines.
		*****/
		XmHTMLGIFStream *gifstream = gif->gstream;
		int err;

		bytes_avail = plc->left;

#ifndef PLC_WORKPROCS
		do
		{
#endif	/* !PLC_WORKPROCS */
			/*****
			* get a new block of compressed data. This will automatically make
			* a new request for input data when there isn't enough data left
			* in the current input buffer.
			*****/
			if((gifstream->avail_in =
					_PLCGetDataBlock(plc, gif->gbuf)) == 0)
			{
				if(plc->plc_status == PLC_SUSPEND ||
					plc->plc_status == PLC_ABORT)
					return;
				/*
				* if plc_status == PLC_ACTIVE, we have a zero data block which
				* indicates the raster data end. Put in a 0 length data
				* block, the gif terminator, set the gifstream state so the
				* caller knows to wrap things up and proceed.
				*/
				gif->gbuf[0] = (Byte)0;
				gif->gbuf[1] = (Byte)1;
				gif->gbuf[2] = (Byte)';';
				gifstream->avail_in = 3;
				gifstream->state = GIF_STREAM_FINAL;
			}

			gifstream->next_in = gif->gbuf;

			/* bytes left in current buffer (+1 for block header) */
			bytes_avail -= (gifstream->avail_in + 1);

			/* adjust output buffer */
			gifstream->next_out  = gif->buffer + gifstream->total_out;
			gifstream->avail_out = gif->buf_size - gifstream->total_out;

			/* uncompress it */
			err = gif->inflate(gifstream);

			/* check return value */
			if(err != GIF_STREAM_OK && err != GIF_STREAM_END)
			{
				_XmHTMLWarning(__WFUNC__(NULL, "_PLC_GIF_ScanlineProc"),
					XMHTML_MSG_115, plc->url,
					gifstream->msg ? gifstream->msg : "(unknown error)");
				plc->plc_status = PLC_ABORT;
				return;
			}

			/* this many uncompressed bytes available now */
			gif->byte_count = gifstream->total_out;

			/* convert input data to raw image data */
			done = DoImage((PLCImage*)gif, gif->buffer);

			/* and break if inflate has finished uncompressing our data. */
			if(err == GIF_STREAM_END || done == True)
				plc->obj_funcs_complete = True;
#ifndef PLC_WORKPROCS
		}
		/* This test will be false if we made a new data request */
		while(bytes_avail == plc->left);
#endif	/* !PLC_WORKPROCS */
	}
}

/*****
* Name: 		_PLC_GIF_Destructor
* Return Type: 	void
* Description: 	GIF PLC virtual destructor method.
* In:
*	plc:		current PLC
* Returns:
*	nothing.
*****/
void
_PLC_GIF_Destructor(PLC *plc)
{
	PLCImageGIF *gif;

	_XmHTMLDebug(15, ("plc.c: _PLC_GIF_Destructor for %s\n", plc->url));

	gif = &(plc->object->plc_gif_image);

	if(gif->external_codec)
	{
		/* we have an external decoder, tell it to destroy itself */
		gif->gstream->state     = GIF_STREAM_END;
		gif->gstream->next_out  = NULL;
		gif->gstream->avail_out = 0;
		gif->gstream->next_in   = NULL;
		gif->gstream->avail_in  = 0;

		/* call external decoder */
		(void)gif->inflate(gif->gstream);

		/* destroy the stream object */
		free(gif->gstream);
	}
	else
	{
		/* destroy the stream object */
		LZWStreamDestroy(gif->lstream);
	}
}

#if defined(HAVE_LIBPNG) || defined(HAVE_LIBZ)
/*****
* Name: 		_PLC_GZF_Init
* Return Type: 	void
* Description: 	image initializer for GZF images
* In:
*	plc:		current PLC
* Returns:
*	nothing.
* Note:
*	As GZF images are GIF images in which only the compressed raster data
*	differs, we can simply use the gif initializer for this.
*****/
void
_PLC_GZF_Init(PLC *plc)
{
	_XmHTMLDebug(15, ("plc.c: _PLC_GZF_Init for %s\n", plc->url));

	_PLC_GIF_Init(plc);

	if(plc->plc_status == PLC_ACTIVE)
	{
		PLCImageGZF *gzf;

		/* get GZF image and zstream object */
		gzf = &(plc->object->plc_gzf_image);

		/* this is a GZF image */
		gzf->info->type = IMAGE_GZF;

		/* initialize z_stream */
		gzf->zstream.zalloc = Z_NULL;
		gzf->zstream.zfree  = Z_NULL;
		gzf->zstream.opaque = Z_NULL;

		/* abort if this fails: incorrect library version/out of memory */
		if((inflateInit(&gzf->zstream)) != Z_OK)
		{
			_XmHTMLWarning(__WFUNC__(gzf->owner, "_PLC_GZF_Init"),
				XMHTML_MSG_108, plc->url, "Init",
				gzf->zstream.msg ? gzf->zstream.msg :"(unknown zlib error)");
			plc->plc_status = PLC_ABORT;
			return;
		}
		/* allocate room for uncompressed data */
		gzf->data = (Byte*)calloc(gzf->buf_size + 1, sizeof(Byte));
		gzf->data_size = gzf->buf_size;
		gzf->data_pos  = 0;

		/* don't allocate clipmask yet, it's done in the plc code */
	}
}

/*****
* Name: 		_PLC_GZF_ScanlineProc
* Return Type: 	void
* Description: 	GZF scanline processor (decompresses data and orders it)
* In:
*	plc:		current PLC
* Returns:
*	nothing.
*****/
void
_PLC_GZF_ScanlineProc(PLC *plc)
{
	int err, bytes_avail;
	PLCImageGZF *gzf;
	Boolean done = False;

	_XmHTMLDebug(15, ("plc.c: _PLC_GZF_ScanlineProc for %s\n", plc->url));

	gzf = &(plc->object->plc_gzf_image);

	/* bytes left in current input buffer */
	bytes_avail = plc->left;

#ifndef PLC_WORKPROCS
	do
	{
#endif	/* !PLC_WORKPROCS */
		/*****
		* get a new block of compressed data. This will automatically make
		* a new request for input data when there isn't enough data left
		* in the current input buffer.
		*****/
		if((gzf->zstream.avail_in = _PLCGetDataBlock(plc, gzf->zbuf)) == 0)
		{
			/*
			* if plc_status == PLC_ACTIVE, we have a zero data block which
			* indicates the raster data end. For now we just consider this
			* as the end signal and return without further processing.
			* For gif animations this would be the place to move to the next
			* frame.
			*/
			return;	/* aborted, date ended or plc suspended */
		}
		gzf->zstream.next_in = gzf->zbuf;

		/* bytes left in current buffer (+1 for block header) */
		bytes_avail -= (gzf->zstream.avail_in + 1);

		/* adjust output buffer */
		gzf->zstream.next_out  = gzf->buffer + gzf->zstream.total_out;
		gzf->zstream.avail_out = gzf->buf_size - gzf->zstream.total_out;

		/* uncompress it */
		err = inflate(&gzf->zstream, Z_PARTIAL_FLUSH);

		/* check return value */
		if(err != Z_OK && err != Z_STREAM_END)
		{
			_XmHTMLWarning(__WFUNC__(NULL, "_PLC_GZF_ScanlineProc"),
				XMHTML_MSG_108, plc->url, "", gzf->zstream.msg);
			plc->plc_status = PLC_ABORT;
			return;
		}

		/* this many uncompressed bytes available now */
		gzf->byte_count = gzf->zstream.total_out;

		/* convert input data to raw image data */
		done = DoImage((PLCImage*)gzf, gzf->buffer);

		/* and break if inflate has finished uncompressing our data. */
		if(err == Z_STREAM_END || done == True)
			plc->obj_funcs_complete = True;
#ifndef PLC_WORKPROCS
	}
	/* This test will be false if we made a new data request */
	while(bytes_avail == plc->left);
#endif	/* !PLC_WORKPROCS */
}

/*****
* Name: 		_PLC_GZF_Destructor
* Return Type: 	void
* Description: 	GZF PLC virtual destructor method.
* In:
*	plc:		current PLC
* Returns:
*	nothing.
* Note:
*	GZF images are virtually equal to GIF images, so we can safely use the
*	GIF destructor method to destroy a GZF-plc.
*****/
void
_PLC_GZF_Destructor(PLC *plc)
{
	PLCImageGZF *gzf;

	_XmHTMLDebug(15, ("plc.c: _PLC_GZF_Destructor for %s\n", plc->url));

	gzf = &(plc->object->plc_gzf_image);

	/* clean up zlib */
	if((inflateEnd(&gzf->zstream)) != Z_OK)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "_PLC_GZF_Destructor"),
			XMHTML_MSG_108, plc->url, "End", gzf->zstream.msg);
	}
}

#else

/*****
* Dummy functions when GZF support has not been selected.
* All will abort the PLC when a GZF image is encountered (although the
* dummy ScanlineProc and Destructor will *never* be used).
*****/
void
_PLC_GZF_Init(PLC *plc)
{
	plc->plc_status = PLC_ABORT;
}

void
_PLC_GZF_ScanlineProc(PLC *plc)
{
	plc->plc_status = PLC_ABORT;
}

void
_PLC_GZF_Destructor(PLC *plc)
{
	plc->plc_status = PLC_ABORT;
}

#endif	/* !HAVE_LIBPNG && !HAVE_LIBZ */

/*********
*** Private functions used by both GIF and GZF progressive loaders.
*********/

/*****
* Name: 		DoExtension
* Return Type: 	int
* Description: 	process a gif extension block
* In:
*	plc:		current PLC
*	label:		extension block identifier;
* Returns:
*	True when extension was valid. False otherwise.
*****/
static Boolean
DoExtension(PLC *plc, int label)
{
	static char	buf[256];
	PLCImageGIF *gif;
	gif = &(plc->object->plc_gif_image);

	_XmHTMLDebug(15, ("plc.c: DoExtension for %s\n", plc->url));

	switch(label)
	{
		case 0x01:		/* Plain Text Extension */
			break;
		case 0xff:		/* Application Extension */
			/*
			* Netscape Looping extension
			* Get first block
			*/
			(void)_PLCGetDataBlock(plc, (Byte*)buf);
			if(!(strncmp((char*)buf, "NETSCAPE2.0", 11)))
			{
				if((_PLCGetDataBlock(plc, (Byte*)buf)) <= 0)
					return(False); /* corrupt animation/end of data */
			}
			break;
		case 0xfe:		/* Comment Extension */
			while(_PLCGetDataBlock(plc, (Byte*) buf) > 0);
			return(True);
		case 0xf9:		/* Graphic Control Extension */
			(void)_PLCGetDataBlock(plc, (Byte*) buf);
			if ((buf[0] & 0x1) != 0)
			{
				PLCImageGIF *gif;
				gif = &(plc->object->plc_gif_image);
				gif->bg_pixel = (int)((Byte)buf[3]);
				gif->transparency = XmIMAGE_TRANSPARENCY_BG;
			}

			while(_PLCGetDataBlock(plc, (Byte*) buf) > 0);
			return(True);
		default:
			break;
	}
	while(_PLCGetDataBlock(plc, (Byte*) buf) > 0);

	return(True);
}

/*****
* Name: 		ReadColormap
* Return Type: 	Boolean
* Description: 	reads the colormap of a GIF/GZF image.
* In:
*	plc:		current PLC
*	gif:		plc image object.
* Returns:
*	True when colormap has been read, False if not.
*****/
static Boolean
ReadColormap(PLC *plc, PLCImageGIF *gif)
{
	register int i;
	Byte rgb[3];

	_XmHTMLDebug(15, ("plc.c: ReadColormap for %s\n", plc->url));

	/* read it */
	for(i = 0; i < gif->cmapsize; ++i)
	{
		if(!_PLCReadOK(plc, rgb, sizeof(rgb)))
			return(False);

		GETR(gif->cmap[i]) = rgb[0];
		GETG(gif->cmap[i]) = rgb[1];
		GETB(gif->cmap[i]) = rgb[2];
	}
	return(True);
}

/*****
* Name: 		DoImage
* Return Type: 	Boolean
* Description:	transforms raw gif raster data to final gif image data.
* In:
*	gif:		PLC image object data;
*	input:		raw raster data.
* Returns:
*	True when all raw image data has been processed, False if not.
*****/
static Boolean
DoImage(PLCImage *gif, Byte *input)
{
	Boolean done = False;
	Byte *inPtr, *outPtr;
	register int i;

	/* convert data to actual image data */
	if(gif->npasses > 1)	/* image is interlaced */
	{
		int pass, stride, xpos, ypos, skip;
		int nfill;
		Byte *src, *dest, *data;
		register int j;

		/* last known position in uncompressed data stream */
		inPtr = input;

		/* Interlaced images are always fully recomputed */
		data   = gif->data;		/* image data */
		ypos   = 0;				/* current scanline */
		pass   = 0;				/* current interlacing pass */
		stride = 8;				/* interlacing data stride */
		skip   = gif->width;	/* initial data index */
		xpos   = 0;

		for(i = j = 0; i < gif->height && j < gif->byte_count; i++, j += skip)
		{
			if(ypos < gif->height)
			{
				outPtr = &data[skip * ypos];
				for(xpos = 0; xpos < skip; xpos++)
					*outPtr++ = *inPtr++;
			}
			if((ypos += stride) >= gif->height)
			{
				if(pass++ > 0)
					stride /= 2;
				ypos = stride / 2;
			}
		}
		/*
		* tidy things up by filling in empty spaces between the
		* interlacing passes. When we have completed a pass we have
		* data available for expanding the entire image.
		*/
		if(pass)
		{
			gif->prev_pos = 0;
			gif->data_pos = gif->data_size;
			ypos = gif->height;
		}
		else
			/* current image data ends at this position */
			gif->data_pos = ypos * gif->width;

		/*****
		* This piece of code will copy the current line to a number of
		* following, unfilled lines.
		* For pass 0, it will copy a line 7 times (stride = 8), for pass
		* 1 it will copy a line 3 times (stride = 4), and for pass 2 it
		* will copy a line only once (stride = 2). The last pass is a
		* no-op.
		*****/
		for(i = 0; i < ypos; i += stride)
		{
			src = &data[skip * i];
			for(nfill = 1; nfill < stride &&
				i + nfill < gif->height; nfill++)
			{
				dest = &data[skip * (i + nfill)];
				memmove(dest, src, skip);
			}
		}
		/* Break out if we have performed all passes and processed all data */
		done = (pass == gif->npasses && j >= gif->data_size);
	}
	else
	{
		int max_byte;

		/* last known position in uncompressed data stream */
		inPtr = input + gif->prev_pos;
		/* last known position in image data stream */
		outPtr= gif->data + gif->prev_pos;

		/* last byte to use */
		max_byte = (int)(gif->byte_count/gif->width)*gif->width;

		/* direct data copy */
		for(i = gif->prev_pos; i < max_byte; i++)
			*outPtr++ = *inPtr++;

		/* last full scanline */
		gif->data_pos = max_byte;

		/* Break out if we have performed all passes and processed all data */
		done = (gif->data_pos >= gif->data_size);
	}
	return(done);
}

