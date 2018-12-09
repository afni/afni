#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* readPNG.c : XmHTML png image loading routines
*
* This file Version	$Revision$
*
* Creation date:		Wed Feb 19 03:21:11 GMT+0100 1997
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
* Revision 1.11  1998/04/27 07:03:17  newt
* Small bugfix for libpng 1.0.1 + tka stuff
*
* Revision 1.10  1998/04/04 06:28:35  newt
* XmHTML Beta 1.1.3
*
* Revision 1.9  1997/10/23 00:25:24  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.8  1997/08/31 17:42:05  newt
* debug level change
*
* Revision 1.7  1997/08/30 01:33:04  newt
* Fixed a few small bugs in alpha channel support & 24->8bit image conversion.
* Streamlined the ppm_quant code and added QuickRGB.
* Fixed bad proto for _XmHTMLReReadPNG when PNG support has been disabled.
*
* Revision 1.6  1997/08/01 13:12:08  newt
* Total rewrite: XmHTML now has *FULL* PNG support.
*
* Revision 1.5  1997/05/28 01:55:45  newt
* Image depth support.
*
* Revision 1.4  1997/04/29 14:31:13  newt
* Header files modifications.
*
* Revision 1.3  1997/03/20 08:15:18  newt
* Integrated png image support.
*
* Revision 1.2  1997/03/11 19:58:58  newt
* ImageBuffer changes
*
* Revision 1.1  1997/03/02 23:02:56  newt
* Initial Revision
*
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_LIBPNG
#include <png.h>
#include <setjmp.h>
#include <math.h>		/* required for full alpha channel processing */
#endif

#include "toolkit.h"
#include XmHTMLPrivateHeader

#ifdef HAVE_LIBPNG

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/

/* various macros used by the quantization routines */
#define STORE_COLOR(R,G,B,P) do{ \
	GETR(img_data->cmap[P]) = (R); GETG(img_data->cmap[P]) = (G); \
	GETB(img_data->cmap[P]) = (B); GETP(img_data->cmap[P]) = P; \
}while(0)

/*** Private Variable Declarations ***/
/* background gamma correction used in alpha channel processing */
#define BG_GAMMA_CORRECTION		2.2222222222

/* the maximum value a color component can have */
#define MAX_RGB_VAL	255

/*****
* Name: 		my_png_error
* Return Type: 	void
* Description: 	png error function
* In:
*	png_ptr:	current png stream;
*	msg:		message to be displayed;
* Returns:
*	nothing.
* Note:
*	This routine displays a warning message and terminates PNG reading
*	by jumping to the point where the error function was set.
*****/
static void
my_png_error(png_structp png_ptr, String msg)
{
	ImageBuffer *ib = (ImageBuffer*)png_get_io_ptr(png_ptr);

	_XmHTMLWarning(__WFUNC__(NULL, "png_error"), XMHTML_MSG_121, "png",
		ib->file, msg);
	longjmp(png_ptr->jmpbuf, 1);
}

/*****
* Name: 		my_png_read
* Return Type: 	void
* Description: 	function called by png when it needs another chunk of data
* In:
*	png_ptr:	current png stream;
*	data:		return buffer;
*	len:		no of bytes to be copied.
* Returns:
*	nothing but len bytes are copied into the return buffer.
* Note:
*	This function is used instead of the default png reader (which uses fread)
*	as we have the file data in memory.
*****/
static void
my_png_read(png_structp png_ptr, png_bytep data, png_size_t len)
{
	ImageBuffer *ib = (ImageBuffer*)png_get_io_ptr(png_ptr);
	int size = (int)len;

	if(ib->size > ib->next)
	{
		if(ib->next + size > ib->size)
			size = ib->size - ib->next;
		memcpy(data, ib->buffer + ib->next, size);
		ib->next += size;
		return;
	}
	my_png_error(png_ptr, "Read Error");
}

/*****
* Name: 		_XmHTMLReadPNG
* Return Type: 	XmHTMLRawImageData*
* Description: 	reads a PNG (Portable Network Graphics) image
* In:
*
* Returns:
*	loaded image data upon success or NULL on failure
*****/
XmHTMLRawImageData*
_XmHTMLReadPNG(Widget html, ImageBuffer *ib)
{
	png_structp png_ptr;
	png_infop info_ptr;
	Byte *data;
	int i, idx, npass;
	int width, height, color_type;
	int ncolors, max_colors;
	float gamma, fg_gamma;
	Boolean has_alpha = False, has_cmap = False, do_gamma = True;
	png_bytep *row_ptrs;
	char msg[128];
	static XmHTMLRawImageData *img_data;

	img_data = NULL;
	data = 0;

	_XmHTMLDebug(15, ("readPNG.c: _XmHTMLreadPNG Start, loading %s\n",
		ib->file));

	/*
	* get configuration defaults
	*/
	if(XmIsHTML(html))
	{
		max_colors = HTML_ATTR(max_image_colors);
		gamma      = HTML_ATTR(screen_gamma);
	}
	else
	{
		/* external image support. Requires an xmimage configuration */
		if(_xmimage_cfg == NULL)
			return(NULL);

		if(_xmimage_cfg->flags && ImageQuantize)
			max_colors = _xmimage_cfg->ncolors;
		else
			max_colors = XmHTML_MAX_IMAGE_COLORS;
		if(_xmimage_cfg->flags & ImageScreenGamma)
			gamma = _xmimage_cfg->gamma;
		else
			gamma = XmHTML_DEFAULT_GAMMA;
	}

	/* We set up the normal PNG error routines, then override with longjmp. */
	png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,
		NULL, NULL, NULL);

	/* Create and initialize the info structure */
	if((info_ptr = png_create_info_struct(png_ptr)) == NULL)
	{
		/* failed, too bad */
		png_destroy_read_struct(&png_ptr, NULL, NULL);
		return((XmHTMLRawImageData*)NULL);
	}
	/* now set error handler */
	if(setjmp(png_ptr->jmpbuf))
	{
		/*
		* PNG signalled an error. Destroy image data, free any allocated
		* buffers and return NULL.
		*/
		_XmHTMLDebug(15, ("_XmHTMLreadPNG: end, libpng internal error.\n"));
		png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
		if(img_data)
			FreeRawImage(img_data);
		if(data)
			free(data);
		return((XmHTMLRawImageData*)NULL);
	}
	/*
	* We have the entire image in memory, so we need to set our own
	* ``fread'' function for libpng to use.
	*/
	png_set_read_fn(png_ptr, ib, &my_png_read);

	/* by now we already know this is a png */
	ib->next = 8;
	png_set_sig_bytes(png_ptr, 8);

	/* get png info */
	png_read_info(png_ptr, info_ptr);

	/* allocate raw image data */
	img_data = (XmHTMLRawImageData*)malloc(sizeof(XmHTMLRawImageData));

	ResetRawImage(img_data);

	/* save width & height */
	width  = img_data->width  = info_ptr->width;
	height = img_data->height = info_ptr->height;

	/* image depth */
	ib->depth = info_ptr->bit_depth;

	/* no of colors */
	ncolors = img_data->cmapsize = info_ptr->num_palette;

	/* type of image */
	color_type = info_ptr->color_type;

	/*
	* The fun stuff. This is based on readPNG by Greg Roelofs as found
	* in xpaint 2.4.8, which falls under the same distribution note
	* as readGIF by David Koblas (which is the original author of xpaint).
	* It has been quite heavily modified but it was an invaluable starting
	* point!
	*/
	switch(color_type)
	{
		case PNG_COLOR_TYPE_PALETTE:
			_XmHTMLDebug(15, ("readPNG.c: PNG_COLOR_TYPE_PALETTE\n"));
			img_data->color_class = XmIMAGE_COLORSPACE_INDEXED;
			/*
			* paletted images never contain more than 256 colors but
			* check anyway.
			*/
			if(ncolors > 256)
			{
				sprintf(msg, "PNG_COLOR_TYPE_PALETTE: %i colors reported "
					"while max is 256.", ncolors);
				my_png_error(png_ptr, msg);
			}

			/*
			* Paletted images with transparency info are expanded to
			* RGB with alpha channel.
			* Actual image creation is postponed until the image is
			* needed.
			*/
			if(info_ptr->valid & PNG_INFO_tRNS)
			{
				_XmHTMLDebug(15, ("readPNG.c: tRNS chunk present\n"));
				png_set_expand(png_ptr);
				has_alpha = True;
				do_gamma = False;
			}
			else
			{
				/* store colormap and allocate buffer for read image data */
				AllocRawImageCmap(img_data, ncolors);
				for(i = 0; i < ncolors; i++)
				{
					GETR(img_data->cmap[i]) = info_ptr->palette[i].red;
					GETG(img_data->cmap[i]) = info_ptr->palette[i].green;
					GETB(img_data->cmap[i]) = info_ptr->palette[i].blue;
				}
				has_cmap = True;
				data = (Byte*)malloc(width*height*sizeof(Byte));
			}
			break;
		case PNG_COLOR_TYPE_RGB:
			_XmHTMLDebug(15, ("readPNG.c: PNG_COLOR_TYPE_RGB\n"));
			img_data->color_class = XmIMAGE_COLORSPACE_RGB;
			if(ib->depth == 16)
			{
				_XmHTMLDebug(15, ("readPNG: stripping 48-bit RGB image to "
					"24 bits\n"));
				png_set_strip_16(png_ptr);
				ib->depth = 8;
			}
			/* image data */
			data = (Byte*)malloc(width*height*sizeof(Byte)*3);
			break;

		case PNG_COLOR_TYPE_GRAY:
			_XmHTMLDebug(15, ("readPNG.c: PNG_COLOR_TYPE_GRAY\n"));
			img_data->color_class = XmIMAGE_COLORSPACE_GRAYSCALE;
			if(ib->depth == 16)
			{
				_XmHTMLDebug(15, ("readPNG: stripping 16-bit grayscale image "
					"to 8 bits\n"));
				png_set_strip_16(png_ptr);
				ib->depth = 8;
			}
			/*
			* grayscale with transparency is expanded to RGB with alpha
			* channel.
			*/
			if(info_ptr->valid & PNG_INFO_tRNS)
			{
				_XmHTMLDebug(15, ("readPNG.c: tRNS chunk present\n"));
				png_set_gray_to_rgb(png_ptr);
				png_set_expand(png_ptr);
				has_alpha = True;
				do_gamma = False;
				break;
			}

			/* fill in appropriate grayramp */
			switch(ib->depth)
			{
				case 1:
					/* allocate colormap */
					ncolors = 2;
					AllocRawImageCmap(img_data, ncolors);
					/* fill it */
					STORE_COLOR(0, 0, 0, 0);
					STORE_COLOR(255, 255, 255, 1);
					break;
				case 2:
					/* allocate colormap */
					ncolors = 4;
					AllocRawImageCmap(img_data, ncolors);
					/* fill it */
					STORE_COLOR(0, 0, 0, 0);
					STORE_COLOR(85,  85,  85,  1);  /* 255/3 */
					STORE_COLOR(170, 170, 170, 2);
					STORE_COLOR(255, 255, 255, 3);
					break;
				case 4:
					/* allocate colormap */
					ncolors = 16;
					AllocRawImageCmap(img_data, ncolors);
					/* fill it */
					for (i = 0;  i < 16;  ++i)
					{
						idx = i * 17;  /* 255/15 */
						STORE_COLOR(idx, idx, idx, i);
					}
					break;
				case 8:
					/* allocate colormap */
					ncolors = 256;
					AllocRawImageCmap(img_data, ncolors);
					/* fill it */
					for (i = 0;  i < 256;  ++i)
						STORE_COLOR(i, i, i, i);
					break;
			}
			/* valid colormap */
			has_cmap = True;

			/* image data */
			data = (Byte*)malloc(width*height*sizeof(Byte));
			break;

		case PNG_COLOR_TYPE_RGB_ALPHA:
			_XmHTMLDebug(15, ("readPNG.c: PNG_COLOR_TYPE_RGB_ALPHA\n"));
			img_data->color_class = XmIMAGE_COLORSPACE_RGB;
			do_gamma = False;
			has_alpha = True;

			/* strip 16bit down to 8bits */
            if(ib->depth == 16)
                png_set_strip_16(png_ptr);
			break;
		case PNG_COLOR_TYPE_GRAY_ALPHA:
			_XmHTMLDebug(15, ("readPNG.c: PNG_COLOR_TYPE_GRAY_ALPHA\n"));
			img_data->color_class = XmIMAGE_COLORSPACE_GRAYSCALE;
			do_gamma = False;
			has_alpha = True;

			/* expand to rgb */
			png_set_gray_to_rgb(png_ptr);
			break;
		default:
			sprintf(msg, "bad PNG image: unknown color type (%d)",
				info_ptr->color_type);
			my_png_error(png_ptr, msg);
			break;
	}

	/*
	* We only substitute background pixel if we don't have an alpha channel
	* Doing that for alpha channel images would change the colortype of the
	* current image, leading to weird results.
	*/
	if(!has_alpha && info_ptr->valid & PNG_INFO_bKGD)
	{
		png_set_background(png_ptr, &(info_ptr->background),
			PNG_BACKGROUND_GAMMA_FILE, 1, 1.0);
		img_data->bg = info_ptr->background.index;
	}

	/* handle gamma correction */
	if(info_ptr->valid & PNG_INFO_gAMA)
		fg_gamma = info_ptr->gamma;
	else
		fg_gamma = 0.45;

	/* set it */
	if(do_gamma)
		png_set_gamma(png_ptr, gamma, fg_gamma);

	/* dithering gets handled by caller */

	/* one byte per pixel */
	if(info_ptr->bit_depth < 8)
		png_set_packing(png_ptr);

	/* no tRNS chunk handling, we've expanded it to an alpha channel. */

	/* handle interlacing */
	if(info_ptr->interlace_type)
		npass = png_set_interlace_handling(png_ptr);

	/* and now update everything */
	png_read_update_info(png_ptr, info_ptr);

	/* has possibly changed if we have promoted GrayScale or tRNS chunks */
	color_type = info_ptr->color_type;

	/* new color_type? */
	if(color_type == PNG_COLOR_TYPE_RGB_ALPHA)
	{
		png_bytep png_data;
		/*
		* RGB image with Alpha channel. Actual decoding of this image is
		* delayed until it's required.
		*
		* This approach is needed as at this point we don't know the position
		* of the image in the document and hence we can't correctly handle
		* proper alpha channel processing.
		*
		* Setting the delayed_creation flag instructs XmHTML to create an
		* empty XmHTMLImage structure and delays the actual image composition
		* until the position of this image is known. At that point the painter
		* will call doAlphaChannel to do the actual image creation.
		*/
		row_ptrs = (png_bytep*)malloc(height*sizeof(png_bytep));
		png_data = (png_bytep)malloc(height*info_ptr->rowbytes);

		for(i = 0; i < height; i++)
			row_ptrs[i] = (png_bytep)png_data + i*info_ptr->rowbytes;

		/* read it */
		png_read_image(png_ptr, row_ptrs);

		img_data->data = png_data;			/* raw image data */

		/* no longer needed */
		free(row_ptrs);

		/* set flag so XmHTML will know what to do with this image */
		img_data->delayed_creation = True;

		/* PNG cleanup */
		png_read_end(png_ptr, info_ptr);
		png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

		_XmHTMLDebug(15, ("_XmHTMLreadPNG: end, image creation delayed.\n"));

		/* store image gamma value. We need it later on */
		img_data->fg_gamma = fg_gamma;
		img_data->type = IMAGE_PNG;
		return(img_data);
	}

	/* no alpha channel */
	row_ptrs = (png_bytep*)malloc(height*sizeof(png_bytep));

	for(i = 0; i < height; ++i)
		row_ptrs[i] = (png_bytep)data + i*info_ptr->rowbytes;

	/* read it */
	png_read_image(png_ptr, row_ptrs);

	/* no longer needed */
	free(row_ptrs);

	/*****
	* We're lucky: having a colormap means we have an indexed image.
	*****/
	if(has_cmap)
	{
		img_data->data = data;
		/* upscale colormap to 16bit values */
		for(i = 0; i < img_data->cmapsize; i++)
		{
			GETR(img_data->cmap[i]) <<= 8;
			GETG(img_data->cmap[i]) <<= 8;
			GETB(img_data->cmap[i]) <<= 8;
		}
	}
	else
	{
		/*****
		* RGB image. Convert to paletted image.
		* First allocate a buffer which will receive the final image data.
		*****/
		img_data->data = (Byte*)malloc(width*height*sizeof(Byte));

		/* convert it. On return, the colormap will hold 16bit RGB values */
		if(XmIsHTML(html))
			_XmHTMLConvert24to8(data, img_data,
				HTML_ATTR(max_image_colors), HTML_ATTR(rgb_conv_mode));
		else
			_XmHTMLConvert24to8(data, img_data, XmHTML_MAX_IMAGE_COLORS,
				XmBEST);

		/* and we no longer need the image data itself */
		free(data);
	}

	/* PNG cleanup */
	png_read_end(png_ptr, info_ptr);
	png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

	_XmHTMLDebug(15, ("_XmHTMLreadPNG: end, image loaded.\n"));

	return(img_data);
}

/*****
* Name: 		_XmHTMLReReadPNG
* Return Type: 	XmHTMLRawImageData*
* Description: 	rereads a PNG (Portable Network Graphics) image
* In:
*	html:		current XmHTMLWidget
*	ib:			current image buffer;
*	x,y:		absolute document position for this image;
*	is_body..:	set when this image *is* the body image.
* Returns:
*	loaded image data upon success or NULL on failure
* Note:
*	This function is *only* called for RGB images with a tRNS chunk or
*	alpha channel support. It's purpose is to reprocess the raw image data
*	so we can properly deal with the alpha channel. For overall PNG comments
*	see the above routine.
*	This routine is called by doAlphaChannel().
*****/
XmHTMLRawImageData*
_XmHTMLReReadPNG(XmHTMLWidget html, XmHTMLRawImageData *raw_data, int x,
	int y, Boolean is_body_image)
{
	Byte *png, *rgb, *pp;			/* various ptrs */
	Byte *currLine;					/* temporary buffer */
	int i, j, k;					/* various counters */
	int width, height;				/* image dimensions */
	float gamma, fg_gamma;			/* display and image gamma */
	int background[3];				/* background pixel: R, G, B */
	int foreground[4];				/* image pixel: R, G, B, A */
	int fbpix[3];					/* final image pixel: R, G, B */
	int fb_maxsample;
	XmHTMLImage *bg_image = NULL;
	int bg_width = 0, bg_height = 0;
	Byte *bg_data = NULL;
	AlphaPtr alpha_buffer;
	static XmHTMLRawImageData *img_data; /* return data */

	/* various alpha channel processing variables */
	int ialpha;
	float alpha, compalpha;
	float gamfg, linfg, gambg, linbg, comppix, gcvideo;

	_XmHTMLDebug(15, ("readPNG.c: _XmHTMLRereadPNG Start.\n"));

	/* background alpha channel stuff */
	alpha_buffer = html->html.alpha_buffer;

	/* display gamma */
	gamma = html->html.screen_gamma;
	/* file gamma */
	fg_gamma = raw_data->fg_gamma;

	fb_maxsample = alpha_buffer->fb_maxsample;

	/*
	* If we do not have a body image (or this image *is* the body image),
	* use the background color. Else we need to get the RGB components
	* of the colors used by the background image.
	*/
	if(!is_body_image && alpha_buffer->ncolors)
	{
		bg_image  = html->html.body_image;
		bg_width  = bg_image->width;
		bg_height = bg_image->height;
		bg_data   = bg_image->html_image->data;
		_XmHTMLDebug(15, ("readPNG.c: using background image.\n"));
	}
	else
	{
		background[0] = alpha_buffer->background[0];
		background[1] = alpha_buffer->background[1];
		background[2] = alpha_buffer->background[2];
		_XmHTMLDebug(15, ("readPNG.c: using background color.\n"));
	}

	/* get image width */
	width  = raw_data->width;
	height = raw_data->height;

	AllocRawImage(img_data, width, height);

	/* raw png image data (source buffer) */
	png = raw_data->data;
	/* intermediate destination buffer, contains alpha-corrected values */
	currLine = (Byte*)malloc(width*height*sizeof(Byte)*3);
	rgb = currLine;
	/* final destination buffer */
	pp = img_data->data;

	/*****
	* Actual alpha channel processing. This code is based on the alpha
	* channel example in the official W3C PNG Recommendation which uses
	* floating point all over the place.
	*
	* Performance increases can be gained by:
	* - using lookup tables;
	* - handling body-image/body-color separate;
	* - integrate dithering.
	*****/
	for(i = 0; i < height; i++)
	{
		/* do a single scanline */
		for(j = 0; j < width; ++j)
		{
			/* tile position */
			if(bg_data)
			{
				/* compute correct tile offset */
				int dx = (j+x) % bg_width;
				int dy = (i+y) % bg_height;
				int i = dy * bg_width + dx;
				int idx = (int)bg_data[i];
				background[0] = GETR(alpha_buffer->bg_cmap[idx]);
				background[1] = GETG(alpha_buffer->bg_cmap[idx]);
				background[2] = GETB(alpha_buffer->bg_cmap[idx]);
			}

			/* get foreground color */
			foreground[0] = *png++;		/* red */
			foreground[1] = *png++;		/* green */
			foreground[2] = *png++;		/* blue */
			foreground[3] = *png++;		/* alpha */

			/*
			* Get integer version of alpha.
			* Check for opaque and transparent special cases;
			* no compositing needed if so.
			*
			* We show the whole gamma decode/correct process in floating
			* point, but it would more likely be done with lookup tables.
			*/
			ialpha = foreground[3];
			if(ialpha == 0)
			{
				/* Foreground is transparent. replace with background */
				fbpix[0] = background[0];
				fbpix[1] = background[1];
				fbpix[2] = background[2];
			}
			else if(ialpha == MAX_RGB_VAL)
			{
				/* Copy foreground pixel to frame buffer. */
				for(k = 0; k < 3; k++)
				{
					gamfg    = (float) foreground[k] / MAX_RGB_VAL;
					linfg    = pow(gamfg, 1.0/fg_gamma);
					comppix  = linfg;
					gcvideo  = pow(comppix, 1.2/gamma);
					fbpix[k] = (int) (gcvideo * fb_maxsample + 0.5);
				}
			}
			else
			{
				/*
				* Compositing is necessary.
				* Get floating-point alpha and its complement.
				* Note: alpha is always linear; gamma does not affect it.
				*/
				alpha = (float) ialpha / MAX_RGB_VAL;
				compalpha = 1.0 - alpha;

				for(k = 0; k < 3; k++)
				{
					/*
					* Convert foreground and background to floating
					* point, then linearize (undo gamma encoding).
					*/
					gamfg = (float) foreground[k] / MAX_RGB_VAL;
					linfg = pow(gamfg, 1.0/fg_gamma);
					gambg = (float) background[k] / MAX_RGB_VAL;
					linbg = pow(gambg, BG_GAMMA_CORRECTION);

					/* Composite */
					comppix = linfg * alpha + linbg * compalpha;

					/*
					* Gamma correct for display.
					* Convert to integer frame buffer pixel.
					* We assume a viewing gamma of 1.2
					*/
					gcvideo = pow(comppix, 1.2/gamma);
					fbpix[k] = (int) (gcvideo * fb_maxsample + 0.5);
				}
			}
			/* and store it */
			*rgb++ = (Byte)fbpix[0];
			*rgb++ = (Byte)fbpix[1];
			*rgb++ = (Byte)fbpix[2];
		}
	}
	/* and reduce to 8bit paletted image */
	_XmHTMLConvert24to8(currLine, img_data, HTML_ATTR(max_image_colors),
				HTML_ATTR(rgb_conv_mode));
	free(currLine);

	/* copy untouched fields */
	img_data->bg   = raw_data->bg;
	img_data->type = raw_data->type;
	img_data->color_class = raw_data->color_class;

	_XmHTMLDebug(15, ("_XmHTMLReReadPNG: end, image loaded.\n"));

	return(img_data);
}

#else	/* !HAVE_LIBPNG */

/* empty func if PNG isn't supported */
/* ARGSUSED */
XmHTMLRawImageData*
_XmHTMLReadPNG(Widget html, ImageBuffer *ib)
{
	return((XmHTMLRawImageData*)NULL);
}

/* ARGSUSED */
XmHTMLRawImageData*
_XmHTMLReReadPNG(XmHTMLWidget html, XmHTMLRawImageData *raw_data, int x,
	int y, Boolean is_body_image)
{
	return((XmHTMLRawImageData*)NULL);
}
#endif /* HAVE_LIBPNG */
