#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* readJPEGplc.c :		JPEG progressive loading interfaces
*
* This file Version	$Revision$
*
* Creation date:		Thu Jun 19 14:56:52 GMT+0100 1997
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
* Revision 1.6  1998/04/27 07:03:02  newt
* tka stuff
*
* Revision 1.5  1998/04/04 06:28:34  newt
* XmHTML Beta 1.1.3
*
* Revision 1.4  1997/10/23 00:25:24  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.3  1997/08/31 17:42:00  newt
* debug level change
*
* Revision 1.2  1997/08/30 01:30:27  newt
* _XmHTMLWarning proto changes.
*
* Revision 1.1  1997/08/01 12:51:58  newt
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
#ifdef HAVE_XCCP_H
#include "XCC.h"
#endif
#include "plc.h"

#ifdef HAVE_LIBJPEG
/*****
* All routines in this file are fully re-entrant.
*****/

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/
/* our own memory manager */
typedef struct _plc_jpeg_source_mgr{
	struct jpeg_source_mgr pub;		/* public fields */
	PLC		*plc;					/* ptr to current PLC */
	Byte	*buffer;				/* jpeg input buffer */
	int		buf_size;				/* size of jpeg input buffer */
	int		nskip;					/* bytes to be skipped in the input buf. */
}plc_jpeg_source_mgr;

/*** Private Function Prototype Declarations ****/
static void _PLC_JPEG_ErrorExit(j_common_ptr cinfo);
static void _PLC_JPEG_InitSource(j_decompress_ptr cinfo);
static void _PLC_JPEG_TermSource(j_decompress_ptr cinfo);
static void _PLC_JPEG_SetSource(j_decompress_ptr cinfo, PLC *plc);
static void _PLC_JPEG_SkipInputData(j_decompress_ptr cinfo, long nskip);
static boolean _PLC_JPEG_FillInputBuffer(j_decompress_ptr cinfo);

/*****
* Additional PLC object function. Performs two-pass color quantization
* using FS dithering.
*****/
static void _PLC_JPEG_FinalPass(PLC *plc);

/*** Private Variable Declarations ***/

/*****
* Name: 		_PLC_JPEG_ErrorExit
* Return Type: 	void
* Description: 	JPEG error override. Called when libjpeg signals an error.
* In:
*	cinfo:		current decompressor.
* Returns:
*	nothing.
*****/
static void
_PLC_JPEG_ErrorExit(j_common_ptr cerr)
{
	char err_msg[JMSG_LENGTH_MAX];
	j_decompress_ptr cinfo   = (j_decompress_ptr)cerr;
	plc_jpeg_source_mgr *src = (plc_jpeg_source_mgr*)cinfo->src;
	plc_jpeg_err_mgr *jerr   = (plc_jpeg_err_mgr*)cinfo->err;
	PLC *plc = src->plc;

	/* create the error message */
	(*jerr->pub.format_message)(cerr, err_msg);

	/* show it */
	_XmHTMLWarning(__WFUNC__(plc->object->plc_any_image.owner,
		"_PLC_JPEG_ErrorExit"), XMHTML_MSG_121, "jpeg", plc->url, err_msg);

	/* jump to saved restart point */
	longjmp(jerr->setjmp_buffer, 1);
}

/*****
* Name:			_PLC_JPEG_InitSource
* Return Type: 	void
* Description: 	jpeg init_buffer method. Allocates the input buffer.
* In:
*	cinfo:		JPEG decompresser info.
* Returns:
*	nothing.
* Note:
*	This routine is only called once by the decompressor.
*****/
static void
_PLC_JPEG_InitSource(j_decompress_ptr cinfo)
{
	plc_jpeg_source_mgr *src = (plc_jpeg_source_mgr*)cinfo->src;
	PLC *plc = src->plc;

	_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_InitSource for %s\n", plc->url));

	if(src->buf_size == 0)
	{
		/*****
		* Private fields.
		* We use our own private jpeg input buffer.
		*****/
		src->buf_size = plc->input_size;
		src->buffer   = (Byte*)malloc(src->buf_size * sizeof(Byte));

		/* public fields */
		src->pub.next_input_byte = src->buffer;
		src->pub.bytes_in_buffer = 0;
	}
}

/* What's 42? */
#define TATTUQOLTUAE do { \
	/* end of data, insert a fake EOI marker and return True */ \
	if(plc->plc_data_status == STREAM_END) \
	{ \
		_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_FillInputBuffer, end of " \
			"input, inserting fake EOI marker.\n")); \
		src->buffer[0] = (JOCTET)0xFF; \
		src->buffer[1] = (JOCTET)JPEG_EOI; \
		src->pub.next_input_byte = src->buffer; \
		src->pub.bytes_in_buffer = 2; \
		return(TRUE); \
	} \
}while(0)

/*****
* Name:			_PLC_JPEG_FillInputBuffer
* Return Type: 	boolean (JPEG boolean is int, XmHTML Boolean is a char)
* Description: 	jpeg fill_input_buffer method. We use the Suspended I/O
*				facility of the jpeg library.
* In:
*	cinfo:		JPEG decompresser info.
* Returns:
*	False when decompression of the current decompressor should be suspended
*	(not enough data or data is being skipped), True when the decompresser
*	should continue.
* Note:
*	This is the core of the JPEG PLC object. It updates the JPEG input buffer
*	by making appropriate data requests from the current PLC, inserting a
*	fake jpeg EOI marker when all data has been read or by skipping bogus
*	input data if the jpeg skip_input_data method has been called.
*	The hardest part to figure out for progressive JPEG support was that, if
*	input is to be suspended, the pub.next_input_byte and pub.bytes_in_buffer
*	fields *MUST* be set to NULL and 0 respectively. Havoc occurs otherwise.
*	(this took me two days to figure out...)
*****/
static boolean
_PLC_JPEG_FillInputBuffer(j_decompress_ptr cinfo)
{
	plc_jpeg_source_mgr *src = (plc_jpeg_source_mgr*)cinfo->src;
	PLC *plc = src->plc;
	Byte *dest;
	int len;
	size_t status;

	_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_FillInputBuffer for %s\n", plc->url));

	/*****
	* If the current input buffer is empty, we need to suspend the current
	* JPEG stream. If there is any data left in the jpeg stream, it's saved
	* so it can be restored upon the next invocation of this routine.
	*
	* buffer is of type JOCTET, which is an unsigned char (Byte) on all Unix
	* systems.
	* next_input_byte points to the next byte that is to be read from
	* the buffer. Data before this byte can be discarded safely, while
	* data after this byte must be saved.
	* bytes_in_buffer indicates how many bytes are left in the current
	* buffer.
	*****/
	if(plc->left == 0)
	{
		_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_FillInputBuffer, suspending "
			"input, left = %i, bytes_in_buffer = %i\n", plc->left,
			src->pub.bytes_in_buffer));

		/*****
		* Save current state by backtracking in the main PLC input buffer.
		* This ensures we have things right when a STREAM_RESIZE request
		* was made and that we stay aligned with the incoming data.
		*****/
		if(src->pub.bytes_in_buffer)
		{
			plc->left    = src->pub.bytes_in_buffer;
			plc->next_in = plc->buffer + (plc->size - plc->left);
		}

		/* issue a new data request */
		plc->min_in = 0;
		plc->max_in = plc->input_size;
		(void)_PLCDataRequest(plc);

		/* check end of data */
		TATTUQOLTUAE;

		/* need to resize jpeg buffer if PLC input buffer size changed */
		if(plc->input_size != src->buf_size)
		{
			_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_FillInputBuffer, resizing "
				"jpeg input buffer from %i to %i bytes\n", src->buf_size,
				plc->input_size));

			src->buf_size = plc->input_size;
			src->buffer   = (Byte*)realloc(src->buffer,
								src->buf_size * sizeof(Byte));
		}

		/* suspend decoder */
		src->pub.next_input_byte = NULL;
		src->pub.bytes_in_buffer = 0;
		return(FALSE);
	}

	/* move data left to the beginning of the current jpeg buffer */
	if(src->pub.bytes_in_buffer)
	{
		Byte *next_inpb;
		int bib;

		next_inpb = (Byte*)src->pub.next_input_byte;
		bib = src->pub.bytes_in_buffer;

		_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_FillInputBuffer, backtracking "
			"%i bytes (current).\n", bib));

		/* move data left to the beginning of the buffer */
		src->buffer = (Byte*)memmove(src->buffer, src->pub.next_input_byte,
						src->pub.bytes_in_buffer);
		src->pub.next_input_byte = src->buffer;
	}

	/*****
	* We have data available. We can do two things here:
	* 1. skip input data if the jpeg skip_input_data method has been told to
	*    skip more data than was available (in which nskip contains the no of
	*    bytes to be skipped).
	* 2. copy data from the PLC input buffer to the JPEG input buffer.
	*****/

	/* do we still have to skip input data? */
	if(src->nskip)
	{
		/* sanity */
		src->pub.bytes_in_buffer = 0;
		src->pub.next_input_byte = NULL;

		/* maximum no of bytes we may consume */
		len = (src->buf_size > plc->left ? plc->left : src->buf_size);

		/* don't consume more bytes than we may */
		if(len > src->nskip)
			len = src->nskip;

		_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_FillInputBuffer, skipping "
			"%i bytes (out of %i total).\n", len, src->nskip));

		dest = src->buffer;

		/* make a data request. This should *never* fail. */
		if((status = _PLCReadOK(plc, dest, len)) == 0)
		{
			TATTUQOLTUAE;

			/* this should *never* happen */
			_XmHTMLWarning(__WFUNC__(plc->object->plc_any_image.owner,
				"_PLC_JPEG_FillInputBuffer"), XMHTML_MSG_116, "skipping",
				"data", len);

			plc->plc_status = PLC_ABORT;
			return(FALSE);
		}

		/* this amount of data to be skipped upon next call */
		src->nskip -= status;

		/* still skipping data or no more data */
		if(src->nskip || plc->left == 0)
		{
			_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_FillInputBuffer, suspending "
				"input, nskip: %i, left: %i\n", src->nskip, plc->left));
			return(FALSE);
		}

		/* no more data to be skipped and input available, fall through */
	}

	/* maximum no of bytes we can consume */
	len = src->buf_size - src->pub.bytes_in_buffer;

	/*****
	* Maximum no of bytes available from input (prevents ReadOK from issueing
	* a data request by itself).
	*****/
	if(len > plc->left)
		len = plc->left;

	_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_FillInputBuffer, requesting "
		"%i bytes.\n", len));

	dest = src->buffer + src->pub.bytes_in_buffer;

	/*****
	* This one should *never* fail, so it's an error if it does.
	* Check for STREAM_END anyway.
	*****/
	if((status = _PLCReadOK(plc, dest, len)) == 0)
	{
		TATTUQOLTUAE;

		/* we should *never* get here */
		_XmHTMLWarning(__WFUNC__(plc->object->plc_any_image.owner,
			"_PLC_JPEG_FillInputBuffer"), XMHTML_MSG_116, "filling",
			"buffer", len);

		/* suspend decoder */
		src->pub.next_input_byte = NULL;
		src->pub.bytes_in_buffer = 0;

		/* abort this PLC */
		plc->plc_status = PLC_ABORT;
		return(FALSE);
	}

	/* update source manager fields */
	src->pub.next_input_byte = (JOCTET*)src->buffer;
	src->pub.bytes_in_buffer += (size_t)status;

	/* continue processing */
	return(TRUE);
}

/*****
* Name:			_PLC_JPEG_SkipInputData
* Return Type: 	void
* Description: 	jpeg skip_input_data method.
* In:
*	cinfo:		jpeg decompressor info;
*	nskip:		no of bytes to be skipped in the input;
* Returns:
*	nothing.
* Note:
*	Due to the fact that this routine may *not* be suspended, we must save
*	the amount of data to skip to a public field in the source manager
*	which will then skip past the amount requested.
*****/
static void
_PLC_JPEG_SkipInputData(j_decompress_ptr cinfo, long nskip)
{
	plc_jpeg_source_mgr *src = (plc_jpeg_source_mgr*)cinfo->src;

#ifdef DEBUG
	PLC *plc = src->plc;
	_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_SkipInputData for %s, nskip = %li\n",
		plc->url, nskip));
#endif

	if(nskip != 0)
	{
		if(nskip > src->pub.bytes_in_buffer)
		{
			/* this much data needs to be skipped by FillInputBuffer */
			src->nskip = (nskip - src->pub.bytes_in_buffer);

			/* skip input data */
			src->pub.bytes_in_buffer = 0;
			src->pub.next_input_byte = 0;
		}
		else
		{
			/* skip input data */
			src->pub.next_input_byte += (size_t)nskip;
			src->pub.bytes_in_buffer -= (size_t)nskip;
		}
	}
}

/*****
* Name:			_PLC_JPEG_TermSource
* Return Type: 	void
* Description: 	jpeg term_source method.
* In:
*	cinfo:		JPEG decompressor info;
* Returns:
*	nothing.
*****/
static void
_PLC_JPEG_TermSource(j_decompress_ptr cinfo)
{
	plc_jpeg_source_mgr *src = (plc_jpeg_source_mgr*)cinfo->src;

#ifdef DEBUG
	PLC *plc = src->plc;
	_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_TermSource for %s\n", plc->url));
#endif

	if(src->buf_size)
		free(src->buffer);
	src->buffer   = NULL;
	src->buf_size = 0;
}

/*****
* Name:			_PLC_JPEG_SetSource
* Return Type: 	void
* Description: 	jpeg_stdio_src variant for JPEG PLC.
* In:
*	cinfo:		JPEG decompressor info;
*	plc:		current PLC
* Returns:
*	nothing.
*****/
static void
_PLC_JPEG_SetSource(j_decompress_ptr cinfo, PLC *plc)
{
	plc_jpeg_source_mgr *src;

	_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_SetSource for %s\n", plc->url));

	if(cinfo->src == NULL)
	{	/* first time for this JPEG object */
		cinfo->src=(struct jpeg_source_mgr*)
			(*cinfo->mem->alloc_small) ((j_common_ptr)cinfo,
				JPOOL_PERMANENT, sizeof(plc_jpeg_source_mgr));
	}

	src           = (plc_jpeg_source_mgr*)cinfo->src;
	src->plc      = plc;
	src->buffer   = NULL;
	src->buf_size = 0;
	src->nskip    = 0;
	src->pub.init_source       = _PLC_JPEG_InitSource;
	src->pub.fill_input_buffer = _PLC_JPEG_FillInputBuffer;
	src->pub.skip_input_data   = _PLC_JPEG_SkipInputData;
	src->pub.resync_to_restart = jpeg_resync_to_restart;	/* default */
	src->pub.term_source       = _PLC_JPEG_TermSource;
	src->pub.bytes_in_buffer   = 0;
	src->pub.next_input_byte   = NULL;
}

/*****
* Name:			ReadJPEGColormap
* Return Type: 	void
* Description: 	reads the colormap allocated for a JPEG image.
* In:
*	jpeg:	PLC image object data;
*	cinfo:		current decompressor data.
* Returns:
*	Nothing, but the cmap and cmapsize fields of the PLC object are updated.
*	An existing colormap is replaced.
*****/
static void
ReadJPEGColormap(PLCImageJPEG *jpeg, struct jpeg_decompress_struct *cinfo)
{
	register int i;

	_XmHTMLDebug(15, ("plc.c: ReadJPEGColormap\n"));

	/* free previous colormap */
	if(jpeg->cmap)
	{
		free(jpeg->cmap);
		jpeg->cmap = NULL;
	}

	/* allocate new colormap */
	jpeg->cmapsize = cinfo->actual_number_of_colors;
	jpeg->cmap = (XCOLOR*)calloc(jpeg->cmapsize, sizeof(XCOLOR));

	/* fill colormap. Upscale RGB to 16bits precision */
	if(cinfo->out_color_components == 3)
	{
		int cshift = 16 - cinfo->data_precision;
		jpeg->colorclass = XmIMAGE_COLORSPACE_RGB;

		for (i=0; i < jpeg->cmapsize; i++)
		{
			GETR(jpeg->cmap[i]) = cinfo->colormap[0][i] << cshift;
			GETG(jpeg->cmap[i]) = cinfo->colormap[1][i] << cshift;
			GETB(jpeg->cmap[i]) = cinfo->colormap[2][i] << cshift;
			GETP(jpeg->cmap[i]) = (Pixel)i;
		}
	}
	else
	{
		int cshift = 16 - cinfo->data_precision;
		jpeg->colorclass = XmIMAGE_COLORSPACE_GRAYSCALE;
		for(i = 0; i < jpeg->cmapsize; i++)
		{
			GETR(jpeg->cmap[i]) = GETG(jpeg->cmap[i]) =
				GETB(jpeg->cmap[i]) = cinfo->colormap[0][i] << cshift;
			GETP(jpeg->cmap[i]) = (Pixel)i;
		}
	}

	/* get image depth */
	jpeg->depth = 1;
	while(jpeg->cmapsize > (1 << jpeg->depth))
		jpeg->depth++;
}

/*****
* Name:			_PLC_JPEG_Init
* Return Type:	void
* Description:	JPEG PLC initialization routine. Allocates data structures,
*				reads initial JPEG info.
* In:
*	plc:		current PLC
* Returns:
*	nothing.
*****/
void
_PLC_JPEG_Init(PLC *plc)
{
	struct jpeg_decompress_struct *cinfo;
	plc_jpeg_err_mgr *jerr;
	PLCImageJPEG *jpeg;
	XmHTMLWidget html;
	int i = 0;

	_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_Init for %s\n", plc->url));

	/* this plc is active */
	plc->plc_status = PLC_ACTIVE;
	plc->max_in     = PLC_MAX_BUFFER_SIZE;

	/* ptrs to various objects and substructures */
	jpeg  = &(plc->object->plc_jpeg_image);
	cinfo = &(jpeg->cinfo);
	jerr  = &(jpeg->jerr);
	html  = jpeg->owner;

	/*****
	* If the jpeg decompressor hasn't been allocated yet, do it now.
	* This test may fail if we have been suspended while reading the JPEG
	* header.
	* Note that we only rewind the input buffer only *once* as the jpeg
	* decompressor keeps its own internal state when I/O suspension occurs
	* in any of libjpeg's internal routines. Rewinding the input buffer
	* every time would disrupt the decompressor.
	*****/
	if(jpeg->init == False)
	{
		/* we might have already processed some incoming data, reset */
		_PLCRewindInputBuffer(plc);

		cinfo->err = (struct jpeg_error_mgr *)jpeg_std_error(&(jerr->pub));
		jerr->pub.error_exit = _PLC_JPEG_ErrorExit;

		jpeg_create_decompress(cinfo);

		_PLC_JPEG_SetSource(cinfo, plc);
		jpeg->init = True;
	}

	/*****
	* exit override must be done for each invocation of this routine,
	* set_jmp saves the current environment, which differs each time this
	* routine is called.
	*****/
	if(setjmp(jerr->setjmp_buffer))
	{
		/*****
		* JPEG signalled an error, abort this PLC.
		* Necessary cleanup will be done by _PLC_JPEG_Destructor.
		*****/
		_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_Init: end, libjpeg internal "
			"error.\n"));

		/* abort this PLC */
		plc->plc_status = PLC_ABORT;

		return;
	}

	/*****
	* Read the header. We only want to have images (e.i. the TRUE).
	* libjpeg will backtrack to a suitable place in the input stream
	* when I/O suspension occurs in this call.
	*****/
	if((i = jpeg_read_header(cinfo, TRUE)) != JPEG_HEADER_OK)
	{
		/* an error of some kind, abort this PLC */
		if(i != JPEG_SUSPENDED)
			plc->plc_status = PLC_ABORT;

		/* no more data, aborted or suspended */
		return;
	}
	/* we know this is a JPEG image */
	jpeg->info->type = IMAGE_JPEG;

	/* jpeg images are always fully opaque */
	jpeg->transparency = XmNONE;
	jpeg->bg_pixel = -1;

	/*****
	* Set jpeg options
	*****/
	/*****
	* We want to have buffered output if we want to be able to display
	* the data read so far.
	*****/
	cinfo->buffered_image = TRUE;

	/*****
	* We only want colormapped output, and we only do single-pass color
	* quantization for all but the last passes (two pass quantization requires
	* the entire image data to be present, making progressive loading of JPEG
	* images useless). A final color optimization pass can be performed when
	* the XmNprogressivePerfectColor resource has been set to True, so if we
	* want to be able to allow this we must make preparations for this (hence
	* the enable_ settings).
	*****/
	cinfo->quantize_colors    = TRUE;
	cinfo->enable_1pass_quant = TRUE;
	cinfo->enable_2pass_quant = TRUE;
	cinfo->two_pass_quantize  = FALSE;
	cinfo->dither_mode        = JDITHER_ORDERED;
	cinfo->colormap           = NULL;
	cinfo->output_gamma       = HTML_ATTR(screen_gamma);
	cinfo->desired_number_of_colors = HTML_ATTR(max_image_colors);

	/*****
	* Now initialize the decompressor using the options set above.
	* This routine will never suspend in buffered image mode.
	*****/
	jpeg_start_decompress(cinfo);

	/* check colorspace. We only support RGB and GRAYSCALE */
	if(cinfo->out_color_space != JCS_RGB &&
		cinfo->out_color_space != JCS_GRAYSCALE)
	{
		J_COLOR_SPACE j_cs = cinfo->out_color_space;

		_XmHTMLWarning(__WFUNC__(html, "_PLC_JPEG_Init"), XMHTML_MSG_117,
			(j_cs == JCS_UNKNOWN ? "unspecified" :
			(j_cs == JCS_YCbCr ? "YCbCr/YUV" :
			(j_cs == JCS_CMYK ? "CMYK" : "YCCK"))), plc->url);

		/* to bad, abort it */
		plc->plc_status = PLC_ABORT;
		return;
	}

	/*****
	* Image colorclass and colormap. As the RGB image data will be colormapped
	* (pixelized that is), XmHTML classifies this image as being indexed.
	*****/
	jpeg->ncolors  = cinfo->desired_number_of_colors;

	/* image dimensions */
	jpeg->width  = cinfo->output_width;
	jpeg->height = cinfo->output_height;

	/*****
	* Decoded data buffer and counters.
	*****/
	/* input buffer data stride; output_comp should always be 1 */
	jpeg->stride = cinfo->output_width * cinfo->output_components;

	jpeg->data_pos  = 0;	/* current pos in data received so far */
	jpeg->prev_pos  = 0;	/* size of data received so far */
	jpeg->data_size = jpeg->stride*jpeg->height;
	jpeg->data      = (Byte*)calloc(jpeg->data_size, sizeof(Byte));

	/* object has been initialized */
	plc->obj_funcs[1]  = (PLCProc)_PLC_JPEG_FinalPass;
	plc->initialized   = True;
	plc->curr_obj_func = 0;	/* move to scanline reader */
}

/*****
* Name:			_PLC_JPEG_ScanlineProc
* Return Type: 	void
* Description: 	JPEG scanline processor
* In:
*	plc:		current PLC
* Returns:
*	nothing.
*****/
void
_PLC_JPEG_ScanlineProc(PLC *plc)
{
	struct jpeg_decompress_struct *cinfo;
	plc_jpeg_err_mgr *jerr;
	PLCImageJPEG *jpeg = &(plc->object->plc_jpeg_image);
	Byte *r;
	JSAMPROW buffer[1];			/* row pointer array for read_scanlines */

#ifdef DEBUG
	int lines_processed;
#endif

	_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_ScanlineProc for %s\n", plc->url));

	cinfo = &(jpeg->cinfo);
	jerr  = &(jpeg->jerr);

	/*****
	* exit override must be done for each invocation of this routine,
	* set_jmp saves the current environment, which differs each time this
	* routine is called.
	*****/
	if(setjmp(jerr->setjmp_buffer))
	{
		/*****
		* JPEG signalled an error, abort this PLC.
		* Necessary cleanup will be done by _PLC_JPEG_Destructor.
		*****/
		_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_ScanlineProc: end, libjpeg "
			"internal error.\n"));

		/* abort this PLC */
		plc->plc_status = PLC_ABORT;

		return;
	}

	/* a new pass */
	if(cinfo->input_scan_number != cinfo->output_scan_number)
	{
		_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_ScanlineProc, initializing scan "
			"%i\n", cinfo->input_scan_number));

		cinfo->do_block_smoothing = TRUE;

		/* rewind output buffer */
		jpeg->prev_pos = 0;
		jpeg->data_pos = 0;

		/* will never suspend as we are only using one-pass quantization */
		jpeg_start_output(cinfo, cinfo->input_scan_number);

		/*****
		* read colormap only if this is the first pass. As we are using
		* dithered ordering, it will be the same for each pass. The final
		* colormap will be read by _PLC_JPEG_FinalPass.
		*****/
		if(cinfo->input_scan_number == 1)
			ReadJPEGColormap(jpeg, cinfo);
	}
#ifdef DEBUG
	else
		_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_ScanlineProc, doing scan %i.\n",
			cinfo->output_scan_number));
	lines_processed = cinfo->output_scanline;
#endif

	r = jpeg->data + jpeg->data_pos;

	/* keep processing scanlines as long as we have data available */
	while(cinfo->output_scanline < cinfo->output_height)
	{
		buffer[0] = r;

		/*****
		* Read a new scanline and break if jpeg_read_scanlines couldn't read
		* anymore scanlines (input suspended) or when the PLC gets
		* de-activated.
		*****/
		if((jpeg_read_scanlines(cinfo, buffer, 1)) == 0)
		{
			_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_ScanlineProc, input "
					"suspended.\n"));
			break;
		}
		/* next data slot */
		r += jpeg->stride;
	}

	_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_ScanlineProc, processed scanline %i "
		"to %i\n", lines_processed, cinfo->output_scanline));

	/*****
	* Note: since we have set the buffered_image flag, data_pos will probably
	* become equal to data_size after the first scan or so.
	****/
	jpeg->data_pos = cinfo->output_scanline*jpeg->stride;

	/* pass completed */
	if(cinfo->output_scanline == cinfo->output_height)
	{
		_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_ScanlineProc, scan %i done.\n",
			cinfo->output_scan_number));
		/* skip to next scan */
		if((jpeg_finish_output(cinfo)) == FALSE)
			return;		/* no more data, suspended or aborted, return */
	}

	/* and this PLC has finished when no more input is available */
	if(jpeg_input_complete(cinfo) &&
		cinfo->input_scan_number == cinfo->output_scan_number)
	{
		XmHTMLWidget html = jpeg->owner;

		/* only move to final dithering pass if requested */
		switch(HTML_ATTR(perfect_colors))
		{
			case XmAUTOMATIC:
				/*****
				* We decide by ourselves: if the image has less than 1/3 of
				* the requested image colors, we dither, else we don't.
				*****/
				if(3*jpeg->nused-1 < jpeg->cmapsize)
				{
					plc->curr_obj_func = 1;
					break;
				}
				plc->plc_status = PLC_COMPLETE;
				plc->obj_funcs_complete = True;
				break;
			case XmALWAYS:
				/* always dither */
				plc->curr_obj_func = 1;
				break;
			case XmNEVER: /* no dithering wanted at all */
			default:
				plc->plc_status = PLC_COMPLETE;
				plc->obj_funcs_complete = True;
				break;
		}
		_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_ScanlineProc, image %s read.\n",
			plc->url));
	}
}

/*****
* Name:			_PLC_JPEG_FinalPass
* Return Type: 	void
* Description: 	performs last pass on the decoded image data to do proper
*				color quantization.
* In:
*	plc:		current PLC
* Returns:
*	nothing.
* Note:
*	This routine is only used when the XmNprogressivePerfectColor resource
*	has been set to True (it's a very time consuming operation and causes
*	a colorflash while the results aren't always good. It is False by default).
*****/
static void
_PLC_JPEG_FinalPass(PLC *plc)
{
	struct jpeg_decompress_struct *cinfo;
	plc_jpeg_err_mgr *jerr;
	PLCImageJPEG *jpeg = &(plc->object->plc_jpeg_image);
	Byte *r;
	JSAMPROW buffer[1];			/* row pointer array for read_scanlines */
	XmHTMLWidget html;
	XmHTMLImage *image;
	XmImageInfo *info;
	int i;

	_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_FinalPass for %s\n", plc->url));

	cinfo = &(jpeg->cinfo);
	jerr  = &(jpeg->jerr);
	html  = jpeg->owner;
	image = jpeg->image;
	info  = jpeg->info;

	/*****
	* exit override must be done for each invocation of this routine,
	* set_jmp saves the current environment, which differs each time this
	* routine is called.
	*****/
	if(setjmp(jerr->setjmp_buffer))
	{
		/*****
		* JPEG signalled an error, abort this PLC.
		* Necessary cleanup will be done by _PLC_JPEG_Destructor.
		*****/
		_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_FinalPass : end, libjpeg "
			"internal error.\n"));

		/* abort this PLC */
		plc->plc_status = PLC_ABORT;

		return;
	}

	/*****
	* The decoded image is by now completely decoded and has been buffered
	* in the current decompressor. Set proper color quantization params
	* and initialize the final pass.
	*****/
	cinfo->quantize_colors    = TRUE;
	cinfo->two_pass_quantize  = TRUE;
	cinfo->dither_mode        = JDITHER_FS;
	cinfo->colormap           = NULL;
	cinfo->desired_number_of_colors = HTML_ATTR(max_image_colors);

	/* sanity */
	if(image->npixels == 0)
	{
		_XmHTMLWarning(__WFUNC__(html, "_PLC_JPEG_FinalPass"), XMHTML_MSG_118);
		plc->plc_status = PLC_ABORT;
		return;
	}

	/*****
	* Will never suspend as the image is already totally decoded, so it's
	* an error if it *does* suspend!
	*****/
	if((jpeg_start_output(cinfo, cinfo->input_scan_number)) == FALSE)
	{
		_XmHTMLWarning(__WFUNC__(html, "_PLC_JPEG_FinalPass"), XMHTML_MSG_119);

		/* PLC completed. */
		plc->plc_status = PLC_COMPLETE;
		plc->obj_funcs_complete = True;
		return;
	}

	/* rewind output buffer */
	jpeg->prev_pos = 0;
	jpeg->data_pos = 0;

	/* keep processing scanlines as long as we have data available */
	r = jpeg->data;
	while(cinfo->output_scanline < cinfo->output_height)
	{
		buffer[0] = r;
		if((jpeg_read_scanlines(cinfo, buffer, 1)) == 0)
		{
			_XmHTMLWarning(__WFUNC__(html, "_PLC_JPEG_FinalPass"),
				XMHTML_MSG_120);
			plc->plc_status = PLC_ABORT;
			return;
		}
		r += jpeg->stride;
	}

	/* finish this final pass */
	if((jpeg_finish_output(cinfo)) == FALSE)
	{
		/* this is *very* unlikely to happen */
		_XmHTMLWarning(__WFUNC__(html, "_PLC_JPEG_FinalPass"), XMHTML_MSG_120);
		plc->plc_status = PLC_COMPLETE;
		return;
	}

	/* This will equal jpeg->data_size; it's the final image data. */
	jpeg->data_pos = cinfo->output_scanline*jpeg->stride;

	/* get rid of current XmImageInfo RGB arrays */
	if(info->reds)
		free(info->reds);
	info->reds = info->greens = info->blues = (Dimension*)NULL;

	/* now reset used colors array */
	for(i = 0; i < XmHTML_MAX_IMAGE_COLORS; i++)
	{
		jpeg->used[i] = 0;
		jpeg->xcolors[i] = 0L;
	}

	jpeg->nused = 1;

	/* read new colormap */
	ReadJPEGColormap(jpeg, cinfo);

	/* allocate new RGB arrays */
	info->reds   = (Dimension*)calloc(3*jpeg->cmapsize, sizeof(Dimension));
	info->greens = info->reds   + jpeg->cmapsize;
	info->blues  = info->greens + jpeg->cmapsize;

	/* pass completed */
	_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_FinalPass, done reading image %s.\n",
			plc->url));

	plc->obj_funcs_complete = True;
}

/*****
* Name: 		_PLC_JPEG_Destructor
* Return Type: 	void
* Description: 	JPEG PLC virtual destructor method.
* In:
*	plc:		current PLC
* Returns:
*	nothing.
*****/
void
_PLC_JPEG_Destructor(PLC *plc)
{
	struct jpeg_decompress_struct *cinfo;
	plc_jpeg_source_mgr *src;
	PLCImageJPEG *jpeg;
	XmHTMLImage *image;

	_XmHTMLDebug(15, ("plc.c: _PLC_JPEG_Destructor for %s\n", plc->url));

	jpeg  = &(plc->object->plc_jpeg_image);
	cinfo = &(jpeg->cinfo);
	src   = (plc_jpeg_source_mgr*)cinfo->src;
	image = jpeg->image;

	/* wipe out source manager buffers if not already done */
	if(src->buf_size)
		free(src->buffer);
	src->buffer   = NULL;
	src->buf_size = 0;

	/* and destroy the decompressor */
	jpeg_destroy_decompress(cinfo);

	plc->plc_status = PLC_COMPLETE;
}

#else /* !HAVE_LIBJPEG */

/*****
* Dummy JPEG PLC support functions. Only _PLC_JPEG_Init will be called by the
* main PLC cycler. The other two are here to prevent linker errors.
*****/
void
_PLC_JPEG_Init(PLC *plc)
{
	plc->plc_status = PLC_ABORT;
}

void
_PLC_JPEG_ScanlineProc(PLC *plc)
{
	plc->plc_status = PLC_ABORT;
}

void
_PLC_JPEG_Destructor(PLC *plc)
{
	plc->plc_status = PLC_ABORT;
}

#endif /* HAVE_LIBJPEG */
