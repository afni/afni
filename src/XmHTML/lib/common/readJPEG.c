#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* readJPEG.c : XmHTML jpeg image loading from a memory buffer
*
* This file Version	$Revision$
*
* Creation date:		Wed Feb 19 03:13:58 GMT+0100 1997
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
* memory manager:		Dick Porter
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
* Revision 1.12  1998/04/27 07:03:00  newt
* tka stuff
*
* Revision 1.11  1998/04/04 06:28:33  newt
* XmHTML Beta 1.1.3
*
* Revision 1.10  1997/10/23 00:25:23  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.9  1997/08/31 17:41:56  newt
* debug level change
*
* Revision 1.8  1997/08/01 13:11:26  newt
* Added XmImage support. Now also takes JPEG data precision into account.
*
* Revision 1.7  1997/05/28 01:55:41  newt
* Image depth support.
*
* Revision 1.6  1997/04/29 14:31:12  newt
* Header files modifications.
*
* Revision 1.5  1997/04/03 05:42:48  newt
* ?
*
* Revision 1.4  1997/03/28 07:24:38  newt
* Bugfix in readJPEG: return buffer allocation now uses width*height instead
* of width*width...
*
* Revision 1.3  1997/03/20 08:15:06  newt
* _XmHTMLReadJPEG is now a dummy func when HAVE_JPEG isn't defined
*
* Revision 1.2  1997/03/11 19:58:44  newt
* ImageBuffer changes, Dick Porter (dick@cymru.net)
*
* Revision 1.1  1997/03/02 23:02:52  newt
* Initial Revision
*
*****/ 
/*****
* This entire file is wrapped between a #ifdef HAVE_LIBJPEG/#endif pair.
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_LIBJPEG
#include <jpeglib.h>
#include <setjmp.h>
#endif

#include "toolkit.h"
#include XmHTMLPrivateHeader

#ifdef HAVE_XCCP_H
#include "XCCP.h"
#endif

#ifdef HAVE_LIBJPEG
/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/
struct my_error_mgr {
	struct jpeg_error_mgr pub;	/* "public" fields */
	jmp_buf setjmp_buffer;		/* for return to caller */
};

/*** Private Function Prototype Declarations ****/

/*** Private Variable Declarations ***/
typedef struct my_error_mgr * my_error_ptr;

static void
my_error_exit (j_common_ptr cinfo)
{
	my_error_ptr myerr = (my_error_ptr) cinfo->err;
	longjmp(myerr->setjmp_buffer, 1);
}

typedef struct {
	struct jpeg_source_mgr pub;	/* public fields */
	JOCTET *buffer;
} buffer_source_mgr;

typedef buffer_source_mgr *buffer_src_ptr;
static JOCTET jpeg_EOI_buffer[2];

static void
jpeg_buffer_init_source(j_decompress_ptr cinfo)
{
	/* This should really only be done once */
	jpeg_EOI_buffer[0]=(JOCTET)0xFF;
	jpeg_EOI_buffer[1]=(JOCTET)JPEG_EOI;
}

/*
* We cant give any more data, because all we know about the image is already
* in the buffer!
*
* Therefore, if fill_input_buffer is called, we have a bogus JPEG image, and
* all we can do is try and fake some EOIs.
*/
static boolean
jpeg_buffer_fill_input_buffer(j_decompress_ptr cinfo)
{
	buffer_src_ptr src=(buffer_src_ptr)cinfo->src;

	src->buffer=jpeg_EOI_buffer;
	src->pub.next_input_byte=src->buffer;
	src->pub.bytes_in_buffer=2;

	return(TRUE);
}

/*
* Skip over uninteresting data in the JPEG stream. If we have to seek past
* the end of our buffer, then we have a bogus image. The call to 
* jpeg_buffer_fill_input_buffer handles this case for us.
*/
static void 
jpeg_buffer_skip_input_data(j_decompress_ptr cinfo, long num_bytes)
{
	buffer_src_ptr src=(buffer_src_ptr)cinfo->src;

	if(num_bytes>0)
	{

		/* we have been told to seek off the end of the image! */
		if(num_bytes>(long)src->pub.bytes_in_buffer)
		{
			(void)jpeg_buffer_fill_input_buffer(cinfo);
		}
		else
		{
			src->pub.next_input_byte+=(size_t)num_bytes;
			src->pub.bytes_in_buffer-=(size_t)num_bytes;
		}
	}
}

static void
jpeg_buffer_term_source(j_decompress_ptr cinfo)
{
	/* no-op */
}

/*
* Set up input from a memory buffer
*/
static void 
jpeg_buffer_src(j_decompress_ptr cinfo, Byte *data, unsigned int len)
{
	buffer_src_ptr src;

	if(cinfo->src==NULL)
	{	/* first time for this JPEG object */
		cinfo->src=(struct jpeg_source_mgr *)
			(*cinfo->mem->alloc_small) ((j_common_ptr)cinfo,
				JPOOL_PERMANENT, sizeof(buffer_source_mgr));
	}

	src=(buffer_src_ptr)cinfo->src;
	src->buffer=(JOCTET *)data;
	src->pub.init_source=jpeg_buffer_init_source;
	src->pub.fill_input_buffer=jpeg_buffer_fill_input_buffer;
	src->pub.skip_input_data=jpeg_buffer_skip_input_data;
	src->pub.resync_to_restart=jpeg_resync_to_restart;	/* default */
	src->pub.term_source=jpeg_buffer_term_source;
	src->pub.bytes_in_buffer=len;
	src->pub.next_input_byte=data;
}

/*****
* Name: 		readJPEG
* Return Type: 	XmHTMLRawImageData*
* Description: 	reads a JPEG buffer and returns image data.
* In: 
*
* Returns:
*	loaded image data on success, NULL on failure.
*****/
XmHTMLRawImageData*
_XmHTMLReadJPEG(Widget html, ImageBuffer *ib)
{
	struct jpeg_decompress_struct cinfo;
	struct my_error_mgr jerr;
	Byte *r;
	JSAMPROW buffer[1];			/* row pointer array for read_scanlines */
	int i, row_stride;			/* physical row width in output buffer */
	static XmHTMLRawImageData *img_data;
	img_data = NULL;

	_XmHTMLDebug(15, ("readJPEG.c: _XmHTMLreadJPEG Start, loading %s\n",
		ib->file));

	/* We set up the normal JPEG error routines, then override error_exit. */
	cinfo.err = jpeg_std_error(&jerr.pub);
	jerr.pub.error_exit = my_error_exit;

	/* Establish the setjmp return context for my_error_exit to use. */
	if(setjmp(jerr.setjmp_buffer)) 
	{
		/* 
		* JPEG signalled an error. Destroy image data, free any allocated
		* buffers and return NULL.
		*/
		_XmHTMLDebug(15, ("_XmHTMLreadJPEG: end, libjpeg internal error.\n"));
		jpeg_destroy_decompress(&cinfo);

		if(img_data)
			FreeRawImage(img_data);
		return((XmHTMLRawImageData*)NULL);
	}

	jpeg_create_decompress(&cinfo);

	jpeg_buffer_src(&cinfo, ib->buffer, ib->size);

	jpeg_read_header(&cinfo, TRUE);

	cinfo.quantize_colors = TRUE; 
	cinfo.two_pass_quantize = TRUE;

	/*
	* Get configuration defaults: color quantization and gamma correction.
	*/
	if(XmIsHTML(html))
	{
		XmHTMLWidget htmlw = (XmHTMLWidget)html;
		/* color quantization */
		cinfo.desired_number_of_colors = htmlw->html.max_image_colors;

		/* gamma correction */
		cinfo.output_gamma = htmlw->html.screen_gamma;

		/* no dither selection for JPEG. Always use FS */
		cinfo.dither_mode = JDITHER_FS;

		/*****
		* If we have a palette, use it. This will skip a dither pass
		* when the actual image is created later on.
		*****/
		if(htmlw->html.map_to_palette != XmDISABLED)
		{
			cinfo.out_color_components = 3; 
			cinfo.actual_number_of_colors = htmlw->html.xcc->num_palette;

			/* allocate the palette */
			cinfo.colormap = (JSAMPARRAY)
				(*cinfo.mem->alloc_sarray) ((j_common_ptr)&cinfo,
				JPOOL_PERMANENT, htmlw->html.xcc->num_palette, 3);
			/*****
			* Copy the palette. The JSAMPARRAY type is a 2D matrix of 8bit
			* RGB values, so we don't need to upscale the palette (Remember,
			* the palette used for dithering is 8bit instead of 16bit).
			*****/
			for(i = 0; i < htmlw->html.xcc->num_palette; i++)
			{
				cinfo.colormap[0][i] = htmlw->html.xcc->palette[i].red;
				cinfo.colormap[1][i] = htmlw->html.xcc->palette[i].green;
				cinfo.colormap[2][i] = htmlw->html.xcc->palette[i].blue;
			}
		}
	}
	else
	{
		/* external image support. Requires an xmimage configuration */
		if(_xmimage_cfg == NULL)
		{
			jpeg_destroy_decompress(&cinfo);
			if(img_data)
				FreeRawImage(img_data);
			return((XmHTMLRawImageData*)NULL);
		}

		/* color quantization */
		if(_xmimage_cfg->flags && ImageQuantize)
			cinfo.desired_number_of_colors = _xmimage_cfg->ncolors;
		else
			cinfo.desired_number_of_colors = XmHTML_MAX_IMAGE_COLORS;

		/* gamma correction */
		if(_xmimage_cfg->flags & ImageScreenGamma)
			cinfo.output_gamma = _xmimage_cfg->gamma;
		else
			cinfo.output_gamma = XmHTML_DEFAULT_GAMMA;

		/* dither mode. Default is ordered dithering */
		if(_xmimage_cfg->flags & ImageFSDither)
			cinfo.dither_mode = JDITHER_FS;
		else
			cinfo.dither_mode = JDITHER_ORDERED;
	}

	jpeg_start_decompress(&cinfo);

	row_stride = cinfo.output_width * cinfo.output_components;

	/* fix 03/24/97-01, rr */
	/* allocate raw image */
	AllocRawImage(img_data, cinfo.output_height, row_stride);

	r = img_data->data;

	while (cinfo.output_scanline < cinfo.output_height) 
	{
		buffer[0] = r;
		jpeg_read_scanlines(&cinfo, buffer, 1);
		r += row_stride;
	}

	/* update raw image data */
	img_data->width  = cinfo.output_width;
	img_data->height = cinfo.output_height;
	ib->depth = cinfo.data_precision;

	/* add colormap */
	AllocRawImageCmap(img_data, cinfo.actual_number_of_colors);

	/* set up X colormap. Upscale RGB to 16bits precision */
	if(cinfo.out_color_components == 3) 
	{
		int cshift = 16 - cinfo.data_precision;
		img_data->color_class = XmIMAGE_COLORSPACE_RGB;
		for (i=0; i < img_data->cmapsize; i++) 
		{
			img_data->cmap[i].red   = cinfo.colormap[0][i] << cshift;
			img_data->cmap[i].green = cinfo.colormap[1][i] << cshift;
			img_data->cmap[i].blue  = cinfo.colormap[2][i] << cshift;
		}
	}
	else 
	{
		int cshift = 16 - cinfo.data_precision;
		img_data->color_class = XmIMAGE_COLORSPACE_GRAYSCALE;
		for(i = 0; i < img_data->cmapsize; i++) 
		{
			img_data->cmap[i].red = img_data->cmap[i].green = 
				img_data->cmap[i].blue = cinfo.colormap[0][i] << cshift;
		}
	}
	
	jpeg_finish_decompress(&cinfo);
	jpeg_destroy_decompress(&cinfo);

	_XmHTMLDebug(15, ("_XmHTMLreadJPEG: end, image loaded.\n"));

	return(img_data);
}

#else	/* !HAVE_LIBJPEG */

/* empty func if JPEG isn't supported */
/* ARGSUSED */
XmHTMLRawImageData*
_XmHTMLReadJPEG(Widget html, ImageBuffer *ib)
{
	return((XmHTMLRawImageData*)NULL);
}

#endif	/* HAVE_LIBJPEG */
