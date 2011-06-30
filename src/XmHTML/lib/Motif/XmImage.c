#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* XmImage.c : external XmImage routines
*
* This file Version	$Revision$
*
* Creation date:		Mon May  5 15:53:00 GMT+0100 1997
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
* Revision 1.1  2011/06/30 16:10:30  rwcox
* Cadd
*
* Revision 1.6  1998/04/27 06:58:06  newt
* tka stuff
*
* Revision 1.5  1998/04/04 06:27:57  newt
* XmHTML Beta 1.1.3
*
* Revision 1.4  1997/10/23 00:24:49  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.3  1997/08/30 00:44:15  newt
* Changes due to changes in XmImageInfo & XmImage structures.
*
* Revision 1.2  1997/08/01 12:56:33  newt
* reduced data storage.
*
* Revision 1.1  1997/05/28 01:27:09  newt
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

#include "toolkit.h"
#include XmHTMLPrivateHeader
#include "XCC.h"

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/
XmImageConfig *_xmimage_cfg;

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/

/* make a full copy of the given info */
static XmImageInfo *copyInfo(XmImageInfo *info);

/* convert given image info to an XmImage */
static XmImage *infoToImage(Widget w, XmImageInfo *image_info, Dimension width,
	Dimension height);

/*** Private Variable Declarations ***/

/*****
* Name: 		copyInfo
* Return Type: 	XmImageInfo*
* Description: 	makes a complete copy of the given XmImageInfo
* In: 
*	src:		XmImageInfo to copy
* Returns:
*	a new XmImageInfo structure copied from the given source info.
*****/
static XmImageInfo *
copyInfo(XmImageInfo *info)
{
	static XmImageInfo *info_return;
	XmImageInfo *copy, *current, *src = info;
	int size, frame_count = 0, i, which_frame = AllFrames;

	copy = current = NULL;

	if(_xmimage_cfg && (_xmimage_cfg->flags & ImageFrameSelect))
		which_frame = _xmimage_cfg->which_frames >= info->nframes ?
			LastFrame : _xmimage_cfg->which_frames;

	/* if this is an animation, check what frames we have to copy */
	if(info->nframes && which_frame != AllFrames)
	{
		if(which_frame == FirstFrame)
			frame_count = 1;
		else
		{
			/* get specified frame */
			for(src = info; src != NULL && src->frame != NULL &&
				frame_count != which_frame; src = src->frame, frame_count++);
		}
	}
	else
		frame_count = (info->nframes ? info->nframes : 1);

	for(i = 0; src != NULL && i < frame_count; src = src->frame, i++)
	{
		copy = (XmImageInfo*)malloc(sizeof(XmImageInfo));

		copy->url        = strdup(src->url);
		copy->type       = src->type;
		copy->width      = src->width;
		copy->height     = src->height;
		copy->swidth     = src->swidth;
		copy->sheight    = src->sheight;
		copy->bg         = src->bg;
		copy->ncolors    = src->ncolors;
		copy->scolors    = src->scolors;
		copy->depth      = src->depth;
		copy->options    = src->options;
		copy->x          = src->x;
		copy->y          = src->y;
		copy->loop_count = src->loop_count;
		copy->dispose    = src->dispose;
		copy->timeout    = src->timeout;
		copy->nframes    = src->nframes;
		copy->frame      = NULL;
		copy->alpha      = (Byte*)0;
		copy->fg_gamma   = src->fg_gamma;

		/* allocate and copy all remaining data */
		size = copy->width*copy->height*sizeof(Byte);
		copy->data = (Byte*)malloc(size);
		copy->data = (Byte*)memcpy(copy->data, src->data, size);
		if(ImageInfoClipmask(copy))
		{
			copy->clip = (Byte*)malloc(size);
			copy->clip = (Byte*)memcpy(copy->clip, src->clip, size);
		}
		else
			copy->clip = (Byte*)0;
		if(ImageInfoRGBSingle(copy))
		{
			size         = 3*copy->ncolors*sizeof(Dimension);
			copy->reds   = (Dimension*)malloc(size);
			copy->reds   = (Dimension*)memcpy(copy->reds, src->reds, size);
			copy->greens = copy->reds + copy->ncolors;
			copy->blues  = copy->greens + copy->ncolors;
		}
		else
		{
			size         = copy->ncolors*sizeof(Dimension);
			copy->reds   = (Dimension*)malloc(size);
			copy->reds   = (Dimension*)memcpy(copy->reds, src->reds, size);
			copy->greens = (Dimension*)malloc(size);
			copy->greens = (Dimension*)memcpy(copy->greens, src->greens, size);
			copy->blues  = (Dimension*)malloc(size);
			copy->blues  = (Dimension*)memcpy(copy->blues, src->blues, size);
		}
		/*
		* This new info is a true copy, so everything *must* be freed, even
		* when this image is a copy of an internal image.
		* We don't deal with alpha channels, so be sure to unset that bit.
		*/
		copy->options &= ~XmIMAGE_SHARED_DATA;
		copy->options &= ~XmIMAGE_DELAYED;
		/* We don't deal with alpha channels, so be sure to unset that bit! */
		copy->options &= ~XmIMAGE_DELAYED_CREATION;

		if(current)
		{
			current->frame = copy;
			current = copy;
		}
		else
		{
			current = info_return = copy;
		}
	}
	/*
	* We need to set frame count if we aren't using every frame. 
	* _XmHTMLMakeAnimation uses the frame count to create the frames in an
	* animation, so if we wouldn't adjust the frame count we'd get a sigsegv.
	* or an XProtocolError.
	*/
	if(info->nframes && which_frame != AllFrames)
		info_return->nframes = i;

	_XmHTMLDebug(6, ("XmImage.c, copyInfo, copied %i frames\n", i));
	return(info_return);
}

/*****
* Name: 		infoToImage
* Return Type: 	XmImage
* Description: 	converts the given XmImageInfo to an XmImage
* In: 
*	w:			widget parent for this image
*	image_info:	XmImageInfo to be converted
*	width:		width to which the new image is to be scaled. 0 = no scaling.
*	height:		height to which the new image is to be scaled. 0 = no scaling.
* Returns:
*	A newly created XmImage upon success. NULL on failure.
*****/
static XmImage*
infoToImage(Widget w, XmImageInfo *image_info, Dimension width,
	Dimension height)
{
	static XmImage *image;
	XmHTMLImage *html_image = NULL;
	Colormap cmap = None;
	Visual *visual = NULL;
	Display *dpy = XtDisplay(w);
	Pixmap pixmap = None, clip = None;
	Dimension wi = width, hi = height;

	/* allocate and initialize a new image */
	html_image = (XmHTMLImage*)malloc(sizeof(XmHTMLImage));
	(void)memset(html_image, 0, sizeof(XmHTMLImage));

	/***** 
	* Create an XCC for this image. 
	* Try to get as much as we can for this widget: colormap and
	* visual. The only thing we know for sure a widget *will* have is a
	* colormap, every *Widget* is a subclass of Core.
	*****/
	XtVaGetValues(w,
		XmNcolormap, &cmap,
		XmNvisual, &visual,
		NULL);
	/* walk widget tree or get default visual */
	if(visual == NULL)
		visual = XCCGetParentVisual((Widget)w);

	/* create XCC */
	html_image->xcc = XCCCreate(w, visual, cmap);

	/* store original location in private image info */
	html_image->url        = strdup(image_info->url);
	html_image->html       = (XmHTMLWidget)w;	/* potentially dangerous */
	html_image->html_image = image_info;
	html_image->options    = (Byte)0;

	/* Check if this image is an animation */
	if(image_info->nframes > 1)
		html_image->options |= IMG_ISANIM;

	/* store real image dimensions */
	html_image->width = image_info->swidth;
	html_image->height = image_info->sheight;

	/* store current image dimensions */
	html_image->swidth  = image_info->width;
	html_image->sheight = image_info->height;

	/*****
	* When the specified width and height are non-zero, the image read is
	* scaled (stretched or shrunked) in the given direction.
	* Otherwise the current dimensions are used to minimise data loss.
	*****/
	if(wi == 0)
		wi = html_image->width;
	if(hi == 0)
		hi = html_image->height;

	/*****
	* Go and create the image.
	* We have two options here: the image can be an animation or just
	* a plain image.
	*****/
	if(ImageIsAnim(html_image))
		_XmHTMLMakeAnimation((XmHTMLWidget)w, html_image, wi, hi);
	else
	{
		/* _XmHTMLInfoToPixmap will scale the image if required */
		if((pixmap = _XmHTMLInfoToPixmap((XmHTMLWidget)w, html_image,
			image_info, wi, hi, NULL, &clip)) == None)
		{
			/* destroy this image completely */
			_XmHTMLFreeImage((XmHTMLWidget)w, html_image);
			return(NULL);
		}
		html_image->pixmap = pixmap;
		html_image->clip = clip;
	}

	_XmHTMLDebug(6, ("XmImage.c: infoToImage, image %s loaded, "
		"dimensions: %ix%i\n", html_image->url, html_image->width,
		html_image->height));

	/*
	* now create a XmImage, copy the required members and free the
	* XmHTMLImage structure
	*/
	image = (XmImage*)malloc(sizeof(XmImage));

	/* image data */
	image->file    = html_image->url;
	image->type    = html_image->html_image->type;
	image->pixmap  = html_image->pixmap;
	image->clip    = html_image->clip;
	image->options = html_image->options;
	image->width   = wi;	/* used image width */
	image->height  = hi;	/* used image height */
	image->depth   = image_info->depth;
	image->ncolors = image_info->ncolors;

	/* original image data: real no of colors, width and height */
	image->scolors = image_info->scolors;
	image->swidth  = html_image->width;
	image->sheight = html_image->height;

	/* private data */
	image->npixels = html_image->npixels;	/* no of truly allocated colors */
	image->xcc     = html_image->xcc;

	/* animation data */
	image->frames        = html_image->frames;
	image->nframes       = html_image->nframes;
	image->current_frame = 0;
	image->current_loop  = 0;
	image->loop_count    = html_image->loop_count;
	image->proc_id       = None;
	image->w             = w;
	image->context       = XtWidgetToApplicationContext(w);

	/* free the XmHTMLImage and corresponding XmImageInfo */
	_XmHTMLFreeImageInfo((XmHTMLWidget)w, image_info, False);
	free(html_image);
	html_image = NULL;
	image_info = NULL;

	/* see if we have to create a GC for this image */
	if(_xmimage_cfg && (_xmimage_cfg->flags & ImageCreateGC))
	{
		XGCValues xgc;
		Pixel fg = None, bg = None;

		XtVaGetValues(w, 
			XmNforeground, &fg,
			XmNbackground, &bg,
			NULL);

		/* specified background pixel overrides */
		if(_xmimage_cfg->flags & ImageBackground)
			bg = _xmimage_cfg->bg_color;

		xgc.function = GXcopy;
		xgc.plane_mask = AllPlanes;
		xgc.foreground = fg;
		xgc.background = bg;
		image->gc = XCreateGC(dpy, XtWindow(w), GCFunction | GCPlaneMask |
			GCForeground | GCBackground, &xgc);

	}
	else
		image->gc = NULL;
	/* and return the newly created image */
	return(image);
}

/********
****** Public Functions
********/

/*****
* Name: 		XmImageCreate
* Return Type: 	XmImage
* Description: 	creates and fills an image structure without bothering XmHTML.
*				
* In: 
*	w:			widget id
*	file:		image filename
*	width:		requested image width
*	height:		requested image height
*	config:		XmImage configuration.
* Returns:
*	a newly created image, all fields filled.
*****/
XmImage*
XmImageCreate(Widget w, String file, Dimension width, Dimension height,
	XmImageConfig *config)
{
	static XmImage *image;		/* returned image */
	XmImageInfo *image_info = NULL, *info = NULL;
	Boolean info_copied = False;

	/* See if we have a source for this image. If not, just return */
	if(file == NULL || !*file)
		return(NULL);

	/* sanity */
	if(w == NULL)
	{
		_XmHTMLBadParent(NULL, "XmImageCreate");
		return(NULL);
	}

	if(config == NULL)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "XmImageCreate"), XMHTML_MSG_21,
			"NULL XmImageConfig", "XmImageCreate");
		return(NULL);
	}

	/* set as global XmImage configuration */
	_xmimage_cfg = config;

	/* check for GIF plugins */
	if(_xmimage_cfg->flags & ImageGifDecodeProc)
		XmImageGifProc_plugin = _xmimage_cfg->gif_proc;
	else
		XmImageGifProc_plugin = (XmImageGifProc)NULL;
	if(_xmimage_cfg->flags & ImageGifzCmd)
		XmImageGifzCmd_plugin = _xmimage_cfg->z_cmd;
	else
		XmImageGifzCmd_plugin = NULL;

	/* check for background color. If the bit isn't set, plug in a
	* default background color */
	if(!(_xmimage_cfg->flags & ImageBackground))
	{
		Pixel bg = None;
		XtVaGetValues(w, XmNbackground, &bg, NULL);
		
		_xmimage_cfg->bg_color = bg;
	}

	/* We need a toolkit abstraction for this */
	_xmimage_cfg->tka = XmHTMLTkaCreate();
	XmHTMLTkaSetDisplay(_xmimage_cfg->tka, w);
	XmHTMLTkaSetDrawable(_xmimage_cfg->tka, XtWindow(w));

	/* read the image */
	if((info = XmHTMLImageDefaultProc(w, file, NULL, 0)) == NULL)
		return(NULL);

	/* copy frame info if frames have been selected */
	if(info->nframes && _xmimage_cfg &&
		(_xmimage_cfg->flags & ImageFrameSelect))
	{
		image_info = copyInfo(info);
		info_copied = True;
	}
	else
		image_info = info;

	image = infoToImage(w, image_info, width, height);

	/* need to free imageInfo if we've copied something */
	if(info_copied)
		_XmHTMLFreeImageInfo((XmHTMLWidget)w, info, False);

	if(_xmimage_cfg)
	{
		image->tka = _xmimage_cfg->tka;
		_xmimage_cfg->tka = NULL;
	}
	/* and return newly created image */
	return(image);
}

/*****
* Name: 		XmImageCreateFromInfo
* Return Type: 	XmImage
* Description: 	creates and fills an image structure without bothering XmHTML.
*				
* In: 
*	w:			widget id
*	info:		XmImageInfo structure.
*	width:		requested image width
*	height:		requested image height
*	config:		XmImage configuration.
* Returns:
*	a newly created image, all fields filled.
*****/
XmImage*
XmImageCreateFromInfo(Widget w, XmImageInfo *info, Dimension width,
	Dimension height, XmImageConfig *config)
{
	static XmImage *image;		/* returned image */
	XmImageInfo *image_info = NULL;
	ToolkitAbstraction *tka;

	/* See if we have valid info for this image. If not, just return */
	if(info == NULL)
		return(NULL);

	/* sanity */
	if(w == NULL || config == NULL)
	{
		String func = "XmImageCreateFromInfo";
		if(config == NULL)
			_XmHTMLWarning(__WFUNC__(NULL, func), XMHTML_MSG_21,
				"NULL XmImageConfig", func);
		else
			_XmHTMLBadParent(w, func);
		return(NULL);
	}

	/* set as global XmImage configuration */
	_xmimage_cfg = config;

	/* no need to check for GIF plugins, data has already been read */

	/*****
	* When the image type is IMAGE_UNKNOWN, but a url field is present,
	* go and load it when the url type equals ANCHOR_FILE_LOCAL.
	*****/
	if(info->type == IMAGE_UNKNOWN && info->url &&
		(XmHTMLGetURLType(info->url)) == ANCHOR_FILE_LOCAL)
		return(XmImageCreate(w, info->url, width, height, config));
	else
		image_info = copyInfo(info);

	tka = _xmimage_cfg->tka = XmHTMLTkaCreate();
	XmHTMLTkaSetDisplay(tka, w);
	XmHTMLTkaSetDrawable(tka, XtWindow(w));

	/*
	* Create an XmImage from the given XmImageInfo structure.
	* infoToImage will release the XmImageInfo structure that is fed to it.
	*/
	image = infoToImage(w, image_info, width, height);

	image->tka = tka;

	/*
	* Terrible hack for substituting background color when it has been
	* selected. Luckily we only have to do this when the given imageInfo
	* is transparent, e.i., has got a clipmask.
	* It works by creating a new pixmap with the correct dimensions, doing
	* a FillRect with the given background color and copying the original
	* pixmap to this new pixmap with the clipmask of the original image.
	*/
	if((_xmimage_cfg->flags & ImageBackground) && ImageInfoClipmask(info))
	{
		GC gc;
		Pixmap pixmap;

		_XmHTMLDebug(6, ("XmImageCreateFromInfo: substituting background "
			"colors\n"));

		/* we need a gc for this */
		if(image->gc)
			gc = image->gc;
		else
		{
			gc = tka->CreateGC(tka->dpy, tka->win, 0, 0);
			tka->SetFunction(tka->dpy, gc, tka->gc_func[GC_GXcopy]);
		}

		/* create destination pixmap */
		if((pixmap = tka->CreatePixmap(tka->dpy, tka->win, image->width,
				image->height, XCCGetDepth(image->xcc))) != None)
		{
			/* do a fillrect in the given background color */
			tka->SetForeground(tka->dpy, gc, _xmimage_cfg->bg_color);
			tka->FillRectangle(tka->dpy, pixmap, gc, 0, 0, image->width,
					image->height);

			/* set clipmask */
			tka->SetClipOrigin(tka->dpy, gc, 0, 0);
			tka->SetClipMask(tka->dpy, gc, image->clip);

			/* and copy original pixmap to the new pixmap */
			tka->CopyArea(tka->dpy, image->pixmap, pixmap, gc, 0, 0,
					image->width, image->height, 0, 0);

			/* release original pixmap */
			FreePixmap(tka->dpy, image->pixmap);

			/* and save new one */
			image->pixmap = pixmap;
		}
		/* clean up gc if we weren't supposed to create it in the first place */
		if(gc != image->gc)
			tka->FreeGC(tka->dpy, gc);
		else
		{
			/* reset the original gc */
			Pixel fg = None;

			XtVaGetValues(w, XmNforeground, &fg, NULL);

			tka->SetClipMask(tka->dpy, gc, None);
			tka->SetForeground(tka->dpy, gc, fg);
			tka->SetBackground(tka->dpy, gc, _xmimage_cfg->bg_color);
		}
	}
	/* and return the newly created image */
	return(image);
}

/*****
* Name: 		XmImageDestroy
* Return Type: 	void
* Description: 	completely destroys the given image.
* In: 
*	image:		image to destroy
* Returns:
*	nothing.
*****/
void
XmImageDestroy(XmImage *image)
{
	ToolkitAbstraction *tka = NULL;

	if(image == NULL)
		return;

	/*
	* Each image should have a toolkit abstraction associated with it.
	* Not very economic. Should change this sometime.
	*/
	if((tka = image->tka) == NULL)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "XmImageDestroy"), XMHTML_MSG_25,
			image->file);
		return;
	}

	/* free any gc's created for this image */
	if(image->gc)
		tka->FreeGC(tka->dpy, image->gc);

	/* clear an outstanding timeout proc */
	if(image->proc_id)
		XtRemoveTimeOut(image->proc_id);

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
		free(image->frames);
	}
	else
	{
		FreePixmap(tka->dpy, image->pixmap);
		FreePixmap(tka->dpy, image->clip);
	}

	/* free xcc */
	XCCFree(image->xcc);

	/* free allocated strings */
	free(image->file);

	free(image);

	image = NULL;
}

/*****
* Name: 		XmImageSetBackground
* Return Type: 	int
* Description: 	sets the given image as the background image in the given
*				destination drawable.
* In: 
*	src:		background image;
*	dest:		destination drawable;
*	src_x, src_y:
*				Specify the x and y coordinates, which are relative to the
*				origin of the source rectangle and specify its upper-left
*				corner. Typically used for scrolling backgrounds.
*	width, height:
*				dimensions of rectangle to be filled.
*	dest_x, dest_y:
*				specify the x and y coordinates, which are relative to the
*				origin of the destination drawable and specify its upper-left
*				corner.
* Returns:
*	-1 on error, 0 on success
* Note:
*	See XmHTML.c, routine PaintBackground for the how and why of this
*	function.
*****/
int
XmImageSetBackgroundImage(XmImage *src, Drawable dest, int src_x,
	int src_y, int width, int height, int dest_x, int dest_y)
{
	XGCValues values;
	unsigned long valuemask;
	Display *dpy;
	int tile_width, tile_height, x_dist, y_dist, ntiles_x, ntiles_y;
	int x_offset, y_offset, tsx, tsy;

	if(src == NULL || src->gc == NULL)
		return(-1);

	tile_width  = src->width;
	tile_height = src->height;

	/* absolute screen positions */
	x_dist = src_x + dest_x;
	y_dist = src_y + dest_y;

	/* no of tiles drawn so far */
	ntiles_x = (int)(x_dist/tile_width);
	ntiles_y = (int)(y_dist/tile_height);

	/* absolute tile offsets */
	x_offset = x_dist - ntiles_x * tile_width;
	y_offset = y_dist - ntiles_y * tile_height;

	/* relative tile offsets */
	tsx = dest_x - x_offset;
	tsy = dest_y - y_offset;

	dpy = XtDisplay(src->w);

	valuemask = GCTile | GCFillStyle | GCTileStipXOrigin | GCTileStipYOrigin;
	values.fill_style = FillTiled;
	values.tile = src->pixmap;
	values.ts_x_origin = tsx;
	values.ts_y_origin = tsy;

	XChangeGC(dpy, src->gc, valuemask, &values);

	/* a plain fillrect will redraw the background portion */
	XFillRectangle(dpy, dest, src->gc, dest_x, dest_y, width, height);

	return(0);
}

/*****
* Name: 		XmImageDrawImage
* Return Type: 	int
* Description: 	blits an XmImage to the given destination drawable
* In: 
*	image:		image to be rendered
*	dest:		destination drawable
*	src_x, src_y:
*				x and y coordinates relative to origin of image and specify
*				its upper-left corner.
*	dest_x, dest_y;
*				specify the x and y coordinates, which are relative to the
*				origin of the destination drawable and specify its upper-left
*				corner.
* Returns:
*	-1 on error, 0 on success.
*****/
int
XmImageDrawImage(XmImage *image, Drawable dest, int src_x,
	int src_y, int dest_x, int dest_y)
{
	Display *dpy;

	/* sanity check */
	if(image == NULL || image->gc == NULL)
		return(-1);

	dpy = XtDisplay(image->w);

	/* use the clipmask if we have one */
	if(image->clip)
	{
		XGCValues xgc;
		unsigned long valuemask;

		xgc.clip_mask = image->clip;
		xgc.clip_x_origin = dest_x;
		xgc.clip_y_origin = dest_y;
		valuemask = GCClipMask | GCClipXOrigin | GCClipYOrigin;
		XChangeGC(dpy, image->gc, valuemask, &xgc);

		/* copy to destination */
		XCopyArea(dpy, image->pixmap, dest, image->gc, src_x, src_y,
			image->width, image->height, dest_x, dest_y);

		/* and reset gc */
		xgc.clip_mask = None;
		xgc.clip_x_origin = 0;
		xgc.clip_y_origin = 0;
		XChangeGC(dpy, image->gc, valuemask, &xgc);
	}
	else
		/* copy to destination */
		XCopyArea(dpy, image->pixmap, dest, image->gc, src_x, src_y,
			image->width, image->height, dest_x, dest_y);
	return(0);
}

Boolean
XmImageExport(XmImageInfo *info, String file, Dimension width,
	Dimension height, unsigned char type)
{
	if(info == NULL)
		return(False);

	/* we *never* support exporting to GIF (f*ck Unisys) */
	if(type == IMAGE_GIF || type == IMAGE_GIFANIM || type == IMAGE_GIFANIMLOOP)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "XmImageExport"), XMHTML_MSG_26);
		return(False);
	}

	/* check for supported image types */
#ifndef HAVE_LIBPNG
	if(type == IMAGE_PNG)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "XmImageExport"), XMHTML_MSG_27,
			"PNG", "png");
		return(False);
	}
#endif
#ifndef HAVE_LIBJPEG
	if(type == IMAGE_JPEG)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "XmImageExport"), XMHTML_MSG_27,
			"JPEG", "jpeg");
		return(False);
	}
#endif
#if !defined(HAVE_LIBPNG) && !defined(HAVE_LIBZ)
	if(type == IMAGE_GZF || type == IMAGE_GZFANIM || type == IMAGE_GZFANIMLOOP)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "XmImageExport"), XMHTML_MSG_27,
			"GZF", "z");
		return(False);
	}
#endif

	return(False);
}
