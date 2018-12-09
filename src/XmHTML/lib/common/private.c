#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* private.c : Private XmHTML functions that don't depend on X Intrinsics
*             or Motif.
*
* This file Version	$Revision$
*
* Creation date:		Tue Apr 14 16:13:32 GMT+0100 1998
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
*
* Copyright (C) 1994-1998 by Ripley Software Development
* All Rights Reserved
*
* This file is part of the XmHTML Widget Library
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
* Revision 1.3  2012/03/01 17:56:31  ziad
* Cput
*
* Revision 1.2  2011/11/10 14:37:55  ziad
* Cput
*
* Revision 1.1  2011/06/30 16:10:38  rwcox
* Cadd
*
* Revision 1.1  1998/04/27 06:54:13  newt
* Initial Revision
*
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Our private header files */
#include "toolkit.h"
#include XmHTMLPrivateHeader

/*** External Function Prototype Declarations ***/
extern void _XmHTMLScrollForm(XmHTMLWidget html);
extern void _XmHTMLAutoSizeWidget(XmHTMLWidget html);

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/
void PaintBackground(XmHTMLWidget html, int x, int y, int width, int height);

/*** Private Variable Declarations ***/

/*****
* Name: 		_XmHTMLRefresh
* Return Type: 	void
* Description: 	main screen refresher: given an exposure rectangle, this
*				routine determines the proper paint engine start and end
*				points and calls the painter.
* In:
*	html:		XmHTMLWidget id
*	x,y:		upper-left corner of exposure region
*	width:		width of exposure region
*	height:		height of exposure region
* Returns:
*	nothing.
*****/
void
_XmHTMLRefresh(XmHTMLWidget html, int x, int y, int width, int height)
{
	int x1, x2, y1, y2, dy;
	XmHTMLObjectTable *start, *end;

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh Start\n"));

	x1 = x;
	x2 = x1 + width;

	/*
	* Update background with the given region. Must check if body image
	* hasn't been delayed or is being loaded progressively or we will get
	* some funny effects...
	*/
	if(HTML_ATTR(body_image) && !ImageDelayedCreation(HTML_ATTR(body_image)) &&
		BodyImageLoaded(HTML_ATTR(body_image)->html_image))
		PaintBackground(html, x, y, width, height);

	/*
	* We add the fontheight to the height of the exposure region. This will
	* ensure that the line right under the exposure region is also redrawn
	* properly. Same for topmost position, but subtraction instead of addition.
	*/
	dy = HTML_ATTR(default_font)->lineheight;
	y1 = (y - dy > 0 ? y - dy : y);
	y2 = y1 + height + 1.5*dy;

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, initial y1: %i, y2: %i\n",
		y1, y2));
	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, initial x1: %i, x2: %i\n",
		x1, x2));

	/* add vertical scroll and core offsets */
	y1 += HTML_ATTR(scroll_y) - CORE_ATTR(y);
	y2 += HTML_ATTR(scroll_y) + CORE_ATTR(y);

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, using y1: %i, y2: %i "
		"(scroll_y = %i, core.y = %i)\n", y1, y2, HTML_ATTR(scroll_y),
		CORE_ATTR(y)));

	/*
	* If the offset of the top of the exposed region is higher than
	* the max height of the text to display, the exposure region
	* is empty, so we just return here and leave everything untouched.
	*/
	if(y1 > HTML_ATTR(formatted_height))
	{
		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh End, y1 > maximum document "
			" height\n"));
		HTML_ATTR(top_line) = HTML_ATTR(nlines);
		return;
	}

	/*
	* Get paint stream start & end for the obtained exposure region
	* We have to take the height of the object into account as well.
	* We try to be a little bit smart here.
	* paint_start == NULL is a valid stream command, so check it.
	*/
	start = (HTML_ATTR(paint_start) ?
		HTML_ATTR(paint_start) : HTML_ATTR(formatted));

	/* sanity */
	if(start == NULL)
		return;

	/* below current paint engine start, scrolling down */
	if(y1 > start->y)
	{
		/* already in region, get first object in it */
		if(y1 < (start->y + start->height))
		{
			_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, walking bottom-up, "
				"y_start = %i\n", start->y));
			while(start && y1 > start->y && y1 < (start->y + start->height))
				start = start->prev;
		}
		/* below region, walk forward until we hit first object */
		else
		{
			_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, walking bottom-down, "
				"y_start = %i\n", start->y));
			while(start)
			{
				if(y1 > start->y && y1 < (start->y + start->height))
					break;
				start = start->next;
			}
		}
	}
	/* above current paint engine start, scrolling up */
	else
	{
		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, walking top-up, "
			"y_start = %i\n", start->y));

		while(start && y1 <= start->y)
			start = start->prev;

		start = (start ? start : HTML_ATTR(formatted));
		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, got y = %i (y1 = %i)\n",
			start->y, y1));

		/* get first object with same y position */
		while(start && start->prev && start->y == start->prev->y)
			start = start->prev;
	}

	/* sanity check */
	if(start == NULL)
		start = HTML_ATTR(formatted);
	end = start;

	/* get first point at bottom of exposed region */
	while(end && y2 > end->y)
		end = end->next;
	/* now walk to the last point still inside the region */
	while(end && y2 > end->y && y2 < (end->y + end->height))
		end = end->next;

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, initial start point, "
		"start->x = %i, start->y = %i\n", start->x, start->y));

	/*****
	* If start is part of a table, painting starts at the first cell in a
	* row
	*****/
	if(start->table)
	{
		XmHTMLTable *table;
		TableRow *row = NULL;
		TableCell *cell = NULL;
		int i, j;
		XmHTMLObjectTable *ts=NULL;

		table = start->table;
		if(table->childs)
			table = &(table->childs[0]);

		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, start located in table, "
			"looking, for current row.\n"));

		/* locate row where element is located */
		for(i = 0; i < table->nrows; i++)
		{
			row = &(table->rows[i]);
			if(row->owner == start)
			{
				_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, start found as "
					"row %i.\n\t(y = %i, height = %i)\n", i, start->y,
					start->height));
				break;
			}
			for(j = 0; j < row->ncells; j++)
			{
				cell = &(row->cells[j]);
				if(cell->owner == start)
				{
					ts = start;
					_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, start found "
						"as cell %i in row %i\n\t(y = %i, height = %i)\n",
						j, i, ts->y, ts->height));
					break;
				}
				for(ts = cell->owner; ts && ts != cell->end && ts != start;
					ts = ts->next);
				if(ts == start)
				{
					ts = start;
					_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, start found "
						"in cell %i, row %i\n\t(y = %i, height = %i)\n",
						j, i, ts->y, ts->height));
					break;
				}
			}
			if(ts == start)
			{
				start = row->owner;
				break;
			}
		}
		/* fallback */
		if(i == table->nrows)
		{
			start = start->table->start;
			_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, start nowhere to "
				"be found, setting to first element in table.\n\t(y = %i, "
				"height = %i)\n", start->y, start->height));
		}
	}

	/* set proper paint engine start & end */
	HTML_ATTR(paint_start) = start;
	HTML_ATTR(paint_end) = end;

	/* Set horizontal painting positions */
	HTML_ATTR(paint_x) = x1 + HTML_ATTR(scroll_x) - CORE_ATTR(x);
	HTML_ATTR(paint_width) = x2 + HTML_ATTR(scroll_x) + CORE_ATTR(x);

	/* Set vertical painting positions */
	HTML_ATTR(paint_y) = y1;
	HTML_ATTR(paint_height) = y2;

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, x1 = %i, x2 = %i\n", x1, x2));
	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, y1 = %i, y2 = %i\n", y1, y2));
	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, paint_start->x = %i, "
		"paint_start->y = %i\n", start->x, start->y));
#ifdef DEBUG
	if(end)
		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, paint_end->x = %i, "
			"paint_end->y = %i\n", end->x, end->y));
	else
		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, paint_end is NULL!\n"));
#endif

	if(HTML_ATTR(gc) == NULL)
		return;

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLRefresh, calling _XmHTMLPaint\n"));
	_XmHTMLPaint(html, HTML_ATTR(paint_start), HTML_ATTR(paint_end));

#if 0
	/* doesn't work yet */
	if(HTML_ATTR(is_frame && HTML_ATTR(frame_border)
		_XmHTMLDrawFrameBorder(html);
#endif

	/* display scrollbars */
	_XmHTMLSetScrollBars(html);
}

/*****
* Name: 		PaintBackground
* Return Type: 	void
* Description:	update background with the given region
* In:
*	html:		XmHTMLWidget for which to do background painting.
*	x,y:		origin of region to update
*	width,height: dimensions of region to update.
* Returns:
*	nothing.
* Note:
*	A simple and beautiful routine that does it's job perfectly!
*****/
void
PaintBackground(XmHTMLWidget html, int x, int y, int width, int height)
{
	int tile_width, tile_height, x_dist, y_dist, ntiles_x, ntiles_y;
	int x_offset, y_offset, tsx, tsy;
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	_XmHTMLDebug(1, ("XmHTML.c: PaintBackground start, x = %i, y = %i, "
		"width = %i, height = %i\n", x, y, width, height));

	/* fix 02/06/98-01, eb */
	/* not realized yet */
	if(!tka || !tka->win)
		return;

	/*****
	* We need to figure out a correct starting point for the first
	* tile to be drawn (ts_[x,y]_origin in the GC).
	* We know the region to update. First we need to get the number of tiles
	* drawn so far. Since we want the *total* number of tiles drawn, we must
	* add the scroll offsets to the region origin.
	*****/
	tile_width  = HTML_ATTR(body_image)->width;
	tile_height = HTML_ATTR(body_image)->height;

	_XmHTMLDebug(1, ("XmHTML.c: PaintBackground, tile width = %i, "
		"tile height = %i\n", tile_width, tile_height));

	x_dist = HTML_ATTR(scroll_x) + x;
	y_dist = HTML_ATTR(scroll_y) + y;

	ntiles_x = (int)(x_dist/tile_width);
	ntiles_y = (int)(y_dist/tile_height);

	_XmHTMLDebug(1, ("XmHTML.c: PaintBackground, no of full drawn "
		"horizontal tiles: %i (x_dist = %i)\n", ntiles_x, x_dist));
	_XmHTMLDebug(1, ("XmHTML.c: PaintBackground, no of full drawn "
		"vertical tiles  : %i (y_dist = %i)\n", ntiles_y, y_dist));
	/*
	* Now we know how many full tiles have been drawn, we can calculate
	* the horizontal and vertical shifts required to start tiling on a
	* tile boundary.
	*/
	x_offset = x_dist - ntiles_x * tile_width;
	y_offset = y_dist - ntiles_y * tile_height;

	_XmHTMLDebug(1, ("XmHTML.c: PaintBackground, computed horizontal "
		"offset: %i\n", x_offset));
	_XmHTMLDebug(1, ("XmHTML.c: PaintBackground, computed vertical "
		"offset  : %i\n", y_offset));
	/*
	* Now we can compute the x and y tile origins. Note that these can
	* be negative.
	*/
	tsx = x - x_offset;
	tsy = y - y_offset;

	_XmHTMLDebug(1, ("XmHTML.c: PaintBackground, computed horizontal "
		"tile origin: %i (x = %i)\n", tsx, x));
	_XmHTMLDebug(1, ("XmHTML.c: PaintBackground, computed vertical "
		"tile origin  : %i (y = %i)\n", tsy, y));

	tka->SetFillStyle(tka->dpy, HTML_ATTR(bg_gc),
		tka->fill_style[GC_FILL_TILED]);
	tka->SetTile(tka->dpy, HTML_ATTR(bg_gc), HTML_ATTR(body_image)->pixmap);
	tka->SetTSOrigin(tka->dpy, HTML_ATTR(bg_gc), tsx, tsy);

	/* a plain fillrect will redraw the background portion */
	tka->FillRectangle(tka->dpy, tka->win, HTML_ATTR(bg_gc),
		x, y, width, height);

	_XmHTMLDebug(1, ("XmHTML.c: PaintBackground end\n"));
}


/*****
* Name: 		_XmHTMLGetAnchor
* Return Type: 	XmHTMLWord*
* Description: 	determines if the given x and y positions are within the
*				bounding rectangle of an anchor.
* In:
*	w:			HTML widget to check
*	x,y:		position to validate
*	img:		image if anchor is part of an anchored image map.
* Returns:
*	A ptr. to the anchor data or NULL.
* Note:
*	anchor_words is an array that _only_ contains anchor data. Although
*	it requires a bit more memory, it's worth it since it will be a fast
*	lookup.
*****/
XmHTMLWord*
_XmHTMLGetAnchor(XmHTMLWidget html, int x, int y, XmHTMLImage *img)
{
	XmHTMLWord *anchor_word = NULL;
	int ys, xs;
	register int i;

	/* convert to absolute positions */
	xs = x + HTML_ATTR(scroll_x);
	ys = y + HTML_ATTR(scroll_y);

	for(i = 0 ; i < HTML_ATTR(anchor_words); i++)
	{
		anchor_word = &(HTML_ATTR(anchors[i]));
		if((xs >= anchor_word->x && xs<=(anchor_word->x+anchor_word->width)) &&
			(ys>=anchor_word->y && ys<=(anchor_word->y+anchor_word->height)))
		{
			_XmHTMLFullDebug(1, ("XmHTML.c: _XmHTMLGetAnchor, anchor is: %s\n",
				anchor_word->owner->anchor->href));

			/* store line number */
			anchor_word->owner->anchor->line = anchor_word->line;

			/*****
			* If we find an anchor, *and* it's an image *and* it references
			* an imagemap, we must tell the caller to fetch the corresponding
			* anchor from an imagemap using the found image.
			*****/
			if(anchor_word->type == OBJ_IMG &&
				anchor_word->image->map_type != XmMAP_NONE)
			{
				img = anchor_word->image;
				return(NULL);
			}
			img = NULL;
			return(anchor_word);
		}
	}
	_XmHTMLFullDebug(1, ("XmHTML.c: _XmHTMLGetAnchor, no match found\n"));

	return(NULL);
}

/*****
* Name: 		_XmHTMLGetImageAnchor
* Return Type: 	XmHTMLAnchor*
* Description: 	determines if the given x and y positions lie upon an image
*				that has an imagemap
* In:
*	html:		HTML widget to check
*	x,y:		position to validate
*	list:		list of images. If NULL the default list is used.
* Returns:
*	A ptr. to the anchor data or NULL.
*****/
XmHTMLAnchor*
_XmHTMLGetImageAnchor(XmHTMLWidget html, int x, int y, XmHTMLImage *list)
{
	XmHTMLImage *image = (list ? list : HTML_ATTR(images));
	XmHTMLAnchor *anchor = NULL;
	int ys, xs;
	XmHTMLImageMap *imagemap = NULL;

	/* convert to absolute position */
	xs = x + HTML_ATTR(scroll_x);
	ys = y + HTML_ATTR(scroll_y);

	/* don't do this if we haven't got any imagemaps */
	if(HTML_ATTR(image_maps) == NULL)
		return(NULL);

	_XmHTMLFullDebug(1, ("XmHTML.c: _XmHTMLGetImageAnchor, xs = %i, ys = %i\n",
		xs, ys));

	for(image = HTML_ATTR(images); image != NULL; image = image->next)
	{
#ifdef DEBUG
		if(image->owner)
		{
			_XmHTMLFullDebug(1, ("XmHTML.c: _XmHTMLGetImageAnchor, checking "
				"%s, x = %i, y = %i\n", image->url, image->owner->x,
				image->owner->y));
		}
		else
		{
			_XmHTMLFullDebug(1, ("XmHTML.c: _XmHTMLGetImageAnchor, checking %s "
				"(no owner).", image->url));
		}
#endif
		if(image->owner && (xs >= image->owner->x &&
				xs <= (image->owner->x + image->owner->width)) &&
			(ys >= image->owner->y &&
				ys <= (image->owner->y + image->owner->height)))
		{
			if(image->map_type != XmMAP_NONE)
			{
				if(image->map_type == XmMAP_SERVER)
				{
					_XmHTMLWarning(__WFUNC__(html, "_XmHTMLGetImageAnchor"),
						XMHTML_MSG_14);
					return(NULL);
				}
				if((imagemap = _XmHTMLGetImagemap(html,
						image->map_url)) != NULL)
				{
					if((anchor = _XmHTMLGetAnchorFromMap(html, x, y, image,
						imagemap)) != NULL)
					{
						_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLGetImageAnchor, "
							"anchor is: %s\n", anchor->href));
						return(anchor);
					}
				}

			}
		}
	}
	_XmHTMLFullDebug(1, ("XmHTML.c: _XmHTMLGetImageAnchor, no match found\n"));

	return(NULL);
}

/*****
* Name: 		_XmHTMLOnImage
* Return Type: 	XmHTMLImage*
* Description: 	checks whether the given positions fall within an image
* In:
*	html:		XmHTMLWidget id
*	x:			pointer x-position
*	y:			pointer y-position
* Returns:
*	The selected image if a match was found, NULL if not.
*****/
XmHTMLImage*
_XmHTMLOnImage(XmHTMLWidget html, int x, int y)
{
	XmHTMLImage *image;
	int xs, ys;

	/* convert to absolute position */
	xs = x + HTML_ATTR(scroll_x);
	ys = y + HTML_ATTR(scroll_y);

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLOnImage, xs = %i, ys = %i\n", xs, ys));

	for(image = HTML_ATTR(images); image != NULL; image = image->next)
	{
		if(image->owner && (xs >= image->owner->x &&
				xs <= (image->owner->x + image->owner->width)) &&
			(ys >= image->owner->y &&
				ys <= (image->owner->y + image->owner->height)))
		{
			_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLOnImage, image selected: %s\n",
				image->url));
			return(image);
		}
	}
	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLOnImage, no match found\n"));
	return(NULL);
}

/*****
* Name: 		_XmHTMLReset
* Return Type: 	void
* Description: 	resets all non-persistent resources to their defaults
* In:
*	html:		XmHTMLWidget id
*	free_img:	true when images should be freed. This will only be True
*				when the widget has received a new source.
* Returns:
*	nothing
*****/
void
_XmHTMLReset(XmHTMLWidget html, Boolean free_img)
{
	/* reset some important vars */
	HTML_ATTR(anchors)             = (XmHTMLWord*)NULL;
	HTML_ATTR(anchor_words)        = 0;
	HTML_ATTR(named_anchors)       = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(num_named_anchors)   = 0;
	HTML_ATTR(anchor_current_cursor_element) = (XmHTMLAnchor*)NULL;
	HTML_ATTR(armed_anchor)        = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(current_anchor)      = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(selected)            = (XmHTMLAnchor*)NULL;
	HTML_ATTR(selection)           = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(select_start)        = 0;
	HTML_ATTR(select_end)          = 0;
	HTML_ATTR(scroll_x)            = 0;
	HTML_ATTR(scroll_y)            = 0;
	HTML_ATTR(formatted_width)     = 1;
	HTML_ATTR(formatted_height)    = 1;
	HTML_ATTR(paint_start)         = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(paint_end)           = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(paint_x)             = 0;
	HTML_ATTR(paint_y)             = 0;
	HTML_ATTR(paint_width)         = 0;
	HTML_ATTR(paint_height)        = 0;
	HTML_ATTR(scroll_x)            = 0;
	HTML_ATTR(scroll_y)            = 0;
	HTML_ATTR(top_line)            = 0;
	/* fix 02/26/97-01, kdh */
	HTML_ATTR(paint_start)         = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(paint_end)           = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(nlines)              = 0;

	/* free the line table if it's present */
	if(HTML_ATTR(line_table) != NULL)
		free(HTML_ATTR(line_table));
	HTML_ATTR(line_table)          = (XmHTMLLineTable*)NULL;

	/* Reset all colors */
	HTML_ATTR(body_fg)             = HTML_ATTR(body_fg_save);
	HTML_ATTR(body_bg)             = HTML_ATTR(body_bg_save);
	HTML_ATTR(anchor_fg)           = HTML_ATTR(anchor_fg_save);
	HTML_ATTR(anchor_target_fg)    = HTML_ATTR(anchor_target_fg_save);
	HTML_ATTR(anchor_visited_fg)   = HTML_ATTR(anchor_visited_fg_save);
	HTML_ATTR(anchor_activated_fg) = HTML_ATTR(anchor_activated_fg_save);
	HTML_ATTR(anchor_activated_bg) = HTML_ATTR(anchor_activated_bg_save);
	HTML_ATTR(image_maps)          = (XmHTMLImageMap*)NULL;

	/* and reset image stuff if it was freed */
	if(free_img)
	{
		/* must reset body_image as well since it also has been freed */
		HTML_ATTR(body_image)          = (XmHTMLImage*)NULL;
		HTML_ATTR(images)              = (XmHTMLImage*)NULL;
		HTML_ATTR(body_image_url)      = (String)NULL;
		HTML_ATTR(alpha_buffer)        = (AlphaPtr)NULL;
		/* only reset when we aren't dithering */
		if(HTML_ATTR(map_to_palette) == XmDISABLED)
			HTML_ATTR(xcc) = (XCC)NULL;
	}
}

/*****
* Name: 		_XmHTMLFreeExpendableResources
* Return Type: 	void
* Description: 	frees all non-persistent resources of a Widget
* In:
*	html:		XmHTMLWidget id
*	free_img:	true when images should be freed. This will only be True
*				when the widget has received a new source.
* Returns:
*	nothing
*****/
void
_XmHTMLFreeExpendableResources(XmHTMLWidget html, Boolean free_img)
{
	/* Free anchor worddata */
	if(HTML_ATTR(anchor_words))
		free(HTML_ATTR(anchors));
	HTML_ATTR(anchors) = (XmHTMLWord*)NULL;

	/* Free named anchor data */
	if(HTML_ATTR(num_named_anchors))
		free(HTML_ATTR(named_anchors));
	HTML_ATTR(named_anchors) = (XmHTMLObjectTable*)NULL;

	/*****
	* Always free imagemaps, anchor data becomes invalid!!
	* (fix 09/17/97-02, kdh)
	*****/
	_XmHTMLFreeImageMaps(html);
	HTML_ATTR(image_maps) = (XmHTMLImageMap*)NULL;

	/* clear the images if we have to */
	if(free_img)
	{
		/* Free all images (also clears xcc & alpha channel stuff) */
		XmHTMLImageFreeAllImages((Widget)html);

		/* must reset body_image as well since it also has been freed */
		HTML_ATTR(body_image)          = (XmHTMLImage*)NULL;
		HTML_ATTR(images)              = (XmHTMLImage*)NULL;
		HTML_ATTR(delayed_creation)    = False; /* no delayed image creation */
		HTML_ATTR(alpha_buffer)        = (AlphaPtr)NULL;
		/* only reset when we aren't dithering */
		if(HTML_ATTR(map_to_palette) == XmDISABLED)
		{
			XCCFree(HTML_ATTR(xcc));
			HTML_ATTR(xcc) = (XCC)NULL;
		}
	}
	else
	{
		/*****
		* We need to orphan all images: the formatter will be called shortly
		* after this routine returns and as a result of that the owner
		* of each image will become invalid. Not orphanizing them would
		* lead to a lot of image copying.
		* Info structures with the XmIMAGE_DELAYED_CREATION bit need to
		* propagate this info to their parent, or chances are that alpha
		* channeling will *not* be redone when required.
		* Must not forget to set the global delayed_creation flag or nothing
		* will happen.
		*****/
		register XmHTMLImage *img;
		for(img = HTML_ATTR(images); img != NULL; img = img->next)
		{
			img->owner = NULL;	/* safety */
			img->options |= IMG_ORPHANED;
			if(!ImageInfoFreed(img) &&
				ImageInfoDelayedCreation(img->html_image))
			{
				img->options |= IMG_DELAYED_CREATION;
				HTML_ATTR(delayed_creation) = True;
			}
		}
	}
}

/*****
* Name: 		_XmHTMLGetLineObject
* Return Type: 	void
* Description: 	get the object located at the given y position.
* In:
*	html:		XmHTMLWidget
*	y_pos:		current text y position.
* Returns:
*	located element.
*****/
XmHTMLObjectTableElement
_XmHTMLGetLineObject(XmHTMLWidget html, int y_pos)
{
	register XmHTMLObjectTableElement tmp = NULL;
	int i;

	/*
	* y_pos given must fall in the bounding box of an element.
	* We try to be a little bit smart here:
	* If we have a paint engine end and it's y position is below the
	* requested position, walk forwards until we find a match.
	* If we have a paint engine start and it's y position is below the
	* requested position, walk forwards. If it's above the requested position,
	* walk backwards. We are always bound to find a matching element.
	*/
	if(HTML_ATTR(paint_end) || HTML_ATTR(paint_start))
	{
		/* located above paint engine end, walk forwards */
		if(HTML_ATTR(paint_end) && HTML_ATTR(paint_end->y) < y_pos)
		{
			for(tmp = HTML_ATTR(paint_end); tmp != NULL; tmp = tmp->next)
				if(y_pos >= tmp->y && y_pos < tmp->y + tmp->height)
					break;
		}
		/* not found or no paint engine end */
		else if(HTML_ATTR(paint_start))
		{
			/* located above paint engine start, walk forwards */
			if(HTML_ATTR(paint_start->y) < y_pos)
			{
				for(tmp = HTML_ATTR(paint_start); tmp != NULL; tmp = tmp->next)
					if(y_pos >= tmp->y && y_pos < tmp->y + tmp->height)
						break;
			}
			/* located under paint engine start, walk backwards */
			else
			{
				for(tmp = HTML_ATTR(paint_start); tmp != NULL; tmp = tmp->prev)
					if(y_pos >= tmp->y && y_pos < tmp->y + tmp->height)
						break;
			}
		}
	}
	/* neither paint engine start or end */
	else
	{
		/* sanity */
		if(HTML_ATTR(line_table) == NULL)
		{
			_XmHTMLDebug(1, ("private.c: _XmHTMLGetLineObject"
				" No line table present!\n"));
			return(NULL);
		}

		i = 0;
		while(i < HTML_ATTR(nlines))
		{
			if(HTML_ATTR(line_table)[i].y >= y_pos &&
				HTML_ATTR(line_table)[i].used)
				return(HTML_ATTR(line_table)[i].start);
			i++;
		}
	}

	/* top or bottom element */
	if(tmp == NULL || tmp->prev == NULL)
	{
		/* bottom element */
		if(tmp == NULL)
			return(HTML_ATTR(formatted));
		/* top element otherwise */
		return(NULL);
	}
	else if(tmp->y == y_pos)
		return(tmp);

	/*****
	* Finetuning: start searching the linetable using the line number
	* of the matched object as starting point.
	*****/

	/* sanity */
	if(HTML_ATTR(line_table) == NULL)
	{
		_XmHTMLDebug(1, ("private.c: _XmHTMLGetLineObject"
			" No line table present!\n"));
		return(tmp);
	}

	i = tmp->line;

	if(HTML_ATTR(line_table)[i].y < y_pos)
	{
		while(i < HTML_ATTR(nlines))
		{
			if(HTML_ATTR(line_table)[i].y >= y_pos &&
				HTML_ATTR(line_table)[i].used)
				return(HTML_ATTR(line_table)[i].start);
			i++;
		}
	}
	else
	{
		while(i)
		{
			if(HTML_ATTR(line_table)[i].y >= y_pos &&
				HTML_ATTR(line_table)[i].used)
				return(HTML_ATTR(line_table)[i].start);
			i--;
		}
	}
	return(NULL);
}

/*****
* Name: 		_XmHTMLSetCurrentLineNumber
* Return Type: 	void
* Description: 	get & set the linenumber of the line at the top of the
*				working area.
* In:
*	html:		XmHTMLWidget
*	y_pos:		current text y position.
* Returns:
*	nothing, but the top_line field of the htmlRec is updated.
*****/
void
_XmHTMLSetCurrentLineNumber(XmHTMLWidget html, int y_pos)
{
	XmHTMLObjectTableElement tmp;

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLSetCurrentLineNumber, y_pos = %i\n",
		y_pos));

	if((tmp = _XmHTMLGetLineObject(html, y_pos)) != NULL)
	{

		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLSetCurrentLineNumber, object found, "
			"y_pos = %i, linenumber = %i\n", tmp->y, tmp->line));

		/* set line number for the found object */
		HTML_ATTR(top_line) = tmp->line;

		/*****
		* If the current element has got more than one word in it, and these
		* words span accross a number of lines, adjust the linenumber.
		*****/
		if(tmp->n_words > 1 && tmp->words[0].y != tmp->words[tmp->n_words-1].y)
		{
			int i;
			for(i = 0 ; i < tmp->n_words && tmp->words[i].y < y_pos; i++);
			if(i != tmp->n_words)
				HTML_ATTR(top_line) = tmp->words[i].line;
		}
	}
	else
		HTML_ATTR(top_line) = 0;

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLSetCurrentLineNumber, top_line = %i\n",
		HTML_ATTR(top_line)));
}

/*****
* Name: 		_XmHTMLCheckMaxColorSetting
* Return Type: 	void
* Description: 	checks value of the XmNmaxImageColors resource against
*				maximum number of colors allowed for this display.
* In:
*	html:		XmHTMLWidget
* Returns:
*	nothing;
*****/
void
_XmHTMLCheckMaxColorSetting(XmHTMLWidget html)
{
	int max_colors;

	/* check for an XCC */
	if(HTML_ATTR(xcc) == NULL)
		_XmHTMLCheckXCC(html);

	/* get maximum allowable colors */
	max_colors = XCCGetNumColors(HTML_ATTR(xcc));

	/* limit to 256 colors */
	if(max_colors > XmHTML_MAX_IMAGE_COLORS)
		max_colors = XmHTML_MAX_IMAGE_COLORS;

	/* verify */
	if(HTML_ATTR(max_image_colors) > max_colors)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLCheckMaxColorSetting"),
			XMHTML_MSG_15, HTML_ATTR(max_image_colors), max_colors,
			max_colors);
		HTML_ATTR(max_image_colors) = max_colors;
	}
	/* plop maximum colors in */
	else if(HTML_ATTR(max_image_colors) == 0)
		HTML_ATTR(max_image_colors) = max_colors;
}

/*****
* Name: 		_XmHTMLGetAnchorByName
* Return Type: 	XmHTMLObjectTableElement
* Description: 	returns the named anchor data.
* In:
*	html:		XmHTMLWidget
*	anchor:		anchor to locate, with a leading hash sign.
* Returns:
*	anchor data upon success, NULL on failure.
*****/
XmHTMLObjectTableElement
_XmHTMLGetAnchorByName(XmHTMLWidget html, String anchor)
{
	XmHTMLObjectTableElement anchor_data;
	int i;
	String chPtr = NULL;

	/* see if it is indeed a named anchor */
	if(!anchor || !*anchor || anchor[0] != '#')
		return(NULL);	/* fix 02/03/97-04, kdh */

	/* we start right after the leading hash sign */
	chPtr = &anchor[1];

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLGetAnchorByName Start\n"));

	for(i = 0 ; i < HTML_ATTR(num_named_anchors); i++)
	{
		anchor_data = &(HTML_ATTR(named_anchors[i]));
		if(anchor_data->anchor && anchor_data->anchor->name &&
			!strcmp(anchor_data->anchor->name, chPtr))
		{
			_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLGetAnchorByName End, "
				"match found.\n"));
			return(anchor_data);
		}
	}
	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLGetAnchorByName End\n"));
	return(NULL);
}

/*****
* Name: 		_XmHTMLGetAnchorByValue
* Return Type: 	XmHTMLObjectTableElement
* Description: 	returns the named anchor data.
* In:
*	w:			XmHTMLWidget
*	anchor_id:	internal anchor id.
* Returns:
*	anchor data upon success, NULL on failure.
*****/
XmHTMLObjectTableElement
_XmHTMLGetAnchorByValue(XmHTMLWidget html, int anchor_id)
{
	XmHTMLObjectTableElement anchor_data;
	int i;
	String func = "_XmHTMLGetAnchorByValue";

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLGetAnchorByValue Start\n"));

	/* sanity */
	if(anchor_id < 0 || anchor_id >= HTML_ATTR(num_named_anchors))
	{
		_XmHTMLWarning(__WFUNC__(html, func), XMHTML_MSG_21,
			"Invalid id", func);
		return(NULL);
	}

	/* this should always match */
	anchor_data = &(HTML_ATTR(named_anchors[anchor_id]));
	if(anchor_data->id == anchor_id)
		return(anchor_data);

	/* hmm, something went wrong, search the whole list of named anchors */
	_XmHTMLWarning(__WFUNC__(html, func),
		XMHTML_MSG_18, anchor_id);

	for(i = 0 ; i < HTML_ATTR(num_named_anchors); i++)
	{
		anchor_data = &(HTML_ATTR(named_anchors[i]));
		if(anchor_data->id == anchor_id)
		{
			_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLGetAnchorByValue End, "
				"match found.\n"));
			return(anchor_data);
		}
	}
	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLGetAnchorByValue End\n"));
	return(NULL);
}

/*****
* Name: 		_XmHTMLVerticalPosToLine
* Return Type: 	int
* Description: 	translates a vertical position to the current line number
*				in the currently displayed document.
* In:
*	html:		XmHTMLWidget id;
*	y:			absolute document y position.
* Returns:
*	line number of the object found at the given position. nlines if not found.
*****/
int
_XmHTMLVerticalPosToLine(XmHTMLWidget html, int y)
{
	XmHTMLObjectTableElement tmp;

	/* sanity check */
	if(!HTML_ATTR(formatted))
		return(0);

	if((tmp = _XmHTMLGetLineObject(html, y)) != NULL)
	{

		_XmHTMLDebug(1, ("XmHTML.c: VerticalPosToLine, object found, "
			"y_pos = %i, linenumber = %i\n", tmp->y, tmp->line));

		/*
		* If the current element has got more than one word in it, and these
		* words span accross a number of lines, adjust the linenumber.
		*/
		if(tmp->n_words > 1 && tmp->words[0].y != tmp->words[tmp->n_words-1].y)
		{
			int i;
			for(i = 0 ; i < tmp->n_words && tmp->words[i].y < y; i++);
			if(i != tmp->n_words)
				return(tmp->words[i].line);
			else
				return(tmp->line);
		}
		else
			return(tmp->line);
	}
	return(0);
}

/*****
* Name: 		_XmHTMLScrollToLine
* Return Type: 	void
* Description: 	scrolls the widget to the given line number.
* In:
*	html:		XmHTMLWidget id
*	line:		line number to scroll to.
* Returns:
*	nothing.
*****/
void
_XmHTMLScrollToLine(XmHTMLWidget html, int line)
{
	XmHTMLObjectTableElement tmp = NULL;

	/* use the cast so negative line numbers are also allowed */
	if(line > (int)(HTML_ATTR(nlines)))
	{
		int value;

		_XmHTMLDebug(1, ("XmHTML.c: ScrollToLine, "
			"calling _XmHTMLMoveToPos\n"));

		HTML_ATTR(top_line) = HTML_ATTR(nlines);
		value = HTML_ATTR(formatted_height);

		/* fix 01/30/97-04, kdh */
		_XmHTMLAdjustVerticalScrollValue(HTML_ATTR(vsb), &value);

		_XmHTMLMoveToPos(HTML_ATTR(vsb), html, value);
		return;
	}
	if(line <= 0)
	{
		HTML_ATTR(top_line) = 0;
		_XmHTMLDebug(1, ("XmHTML.c: ScrollToLine, "
			"calling _XmHTMLMoveToPos\n"));
		_XmHTMLMoveToPos(HTML_ATTR(vsb), html, 0);
		return;
	}

	/* sanity */
	if(HTML_ATTR(line_table) == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLScrollToLine"),
			"No line table present!");
		return;
	}

	/* make sure we get a match! */
	while(HTML_ATTR(line_table)[line].used == False &&
		line < HTML_ATTR(nlines))
		line++;

	if(line == HTML_ATTR(nlines))
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLScrollToLine"),
			"Failed to detect requested line number (%i)", line);
		return;
	}

	tmp = HTML_ATTR(line_table)[line].start;

	/* get vertical position */
	if(tmp)
	{
		int i, value;	/* position to scroll to */

		/* we might have gone one object to far. Check and adjust */
		tmp = (line != tmp->line ? (tmp->prev ? tmp->prev : tmp) : tmp);

		value = tmp->y;
		HTML_ATTR(top_line) = tmp->line;

		/*
		* Not exactly the requested line. Now check the line numbers of
		* the text inside this object. We need to subtract the height of this
		* object if we want to have it displayed properly.
		*/
		if(tmp->line != line)
		{
			if(tmp->n_words)
			{
				/* fix 11/11/97-01, dbl */
				for(i = 0; i < tmp->n_words && line > tmp->words[i].line;
					i++);
				/* if found, we need to take y position of the previous word */
				if(i != tmp->n_words && i != 0)
				{
					HTML_ATTR(top_line) = tmp->words[i-1].line;
					value = tmp->words[i-1].y - tmp->words[i-1].height;
				}
			}
		}
		_XmHTMLDebug(1, ("XmHTML.c: ScrollToLine, "
			"requested line: %i, lineno found: %i, y_pos: %i\n",
			line, tmp->line, value));

		/* fix 01/30/97-04, kdh */
		_XmHTMLAdjustVerticalScrollValue(HTML_ATTR(vsb), &value);

		_XmHTMLMoveToPos(HTML_ATTR(vsb), html, value);
	}
	else
	{
		_XmHTMLDebug(1, ("XmHTML.c: ScrollToLine, "
			"failed to detect requested line number!\n"));
	}
}

/*****
* Name:			_XmHTMLDestroyPhaseZero
* Return Type: 	void
* Description: 	discard all toolkit independent resources
* In:
*	html:		XmHTMLWidget id being destroyed
* Returns:
*	nothing
*****/
void
_XmHTMLDestroyPhaseZero(XmHTMLWidget html)
{
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	/* First kill any outstanding PLC's */
	_XmHTMLKillPLCCycler(html);

	/* release event database */
	_XmHTMLEventFreeDatabase(html, html);

	/* Free list of parsed HTML elements */
	HTML_ATTR(elements) = _XmHTMLparseHTML(html, HTML_ATTR(elements), NULL,
							NULL);

	/* Free list of formatted HTML elements */
	_XmHTMLformatObjects(html, html);

	/* Free list of form data */
	_XmHTMLFreeForm(html, HTML_ATTR(form_data));
	HTML_ATTR(form_data) = (XmHTMLFormData*)NULL;

	/* free all non-persitent resources and destroy the images */
	_XmHTMLFreeExpendableResources(html, True);

	/*****
	* Free list of frames. It is important that the images are destroyed
	* *before* the frames themselves get destroyed: frames can also have
	* images, and destroying the frame before destroying the image data
	* causes havoc. _XmHTMLDestroyFrames invokes XtDestroyWidget to destroy
	* each of XmHTML's frame childs, which invokes this routine and so on.
	*****/
	if(HTML_ATTR(nframes))
		_XmHTMLDestroyFrames(html, HTML_ATTR(nframes));

	/* free all fonts for this widget's instance */
	_XmHTMLUnloadFonts(html);

	/* free cursors */
	if(HTML_ATTR(anchor_cursor) != None)
		tka->FreeCursor(tka->dpy, HTML_ATTR(anchor_cursor));

	/* Free GC's */
	if(HTML_ATTR(gc))
		tka->FreeGC(tka->dpy, HTML_ATTR(gc));
	if(HTML_ATTR(bg_gc))
		tka->FreeGC(tka->dpy, HTML_ATTR(bg_gc));
}

/*****
* Name: 		_XmHTMLCheckGC
* Return Type: 	void
* Description: 	creates a Graphics Context to be used for rendering
* In:
*	html:		XmHTMLWidget
* Returns:
*	nothing, but a GC is created and stored in the widget's internal data
*	structure. If background images are allowed, a seperate GC is created
*	which is used in PaintBackground to do tiling of the background with an
*	image.
*****/
void
_XmHTMLCheckGC(XmHTMLWidget html)
{
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLCheckGC Start\n"));

	/* sanity check, we *must* have a window if we want to have a gc!! */
	if(!tka->IsRealized((Widget)html) || tka->win == None)
		return;

	/* main gc */
	if(HTML_ATTR(gc) == NULL)
	{
		HTML_ATTR(gc) = tka->CreateGC(tka->dpy, tka->win, 0, NULL);
		tka->SetFunction(tka->dpy, HTML_ATTR(gc), tka->gc_func[GC_GXcopy]);
		tka->SetForeground(tka->dpy, HTML_ATTR(gc), MGR_ATTR(foreground));
		tka->SetBackground(tka->dpy, HTML_ATTR(gc),
			CORE_ATTR(background_pixel));

		XmHTMLTkaRecomputeColors(html, HTML_ATTR(body_bg));

		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLCheckGC, gc created\n"));
	}
	/* background image gc */
	if(HTML_ATTR(body_images_enabled) && HTML_ATTR(bg_gc) == NULL)
	{
		HTML_ATTR(bg_gc) = tka->CreateGC(tka->dpy, tka->win, 0, NULL);
		tka->CopyGC(tka->dpy, HTML_ATTR(gc), 0xFFFF, HTML_ATTR(bg_gc));
	}

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLCheckGC End\n"));
}

/*****
* Name: 		_XmHTMLLayout
* Return Type: 	void
* Description: 	main layout algorithm.
*				computes text layout and configures the scrollbars.
*				Also handles image recreation.
* In:
*	html:		widget to layout
* Returns:
*	nothing
*****/
void
_XmHTMLLayout(XmHTMLWidget html)
{
	XmHTMLObjectTableElement curr_ele = NULL;

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLLayout Start\n"));

	/* set blocking flag */
	HTML_ATTR(in_layout) = True;

	/* remember current vertical position if we have been scrolled */
	if(HTML_ATTR(scroll_y))
		curr_ele = _XmHTMLGetLineObject(html, HTML_ATTR(scroll_y));

	/* make a resize request if we have to do auto-sizing in either direction */
	if(HTML_ATTR(resize_width) || HTML_ATTR(resize_height))
		_XmHTMLAutoSizeWidget(html);
	else
		_XmHTMLComputeLayout(html);

	/* set new vertical scrollbar positions */
	if(curr_ele != NULL)
		HTML_ATTR(scroll_y) = curr_ele->y;
	else
		HTML_ATTR(scroll_y) = 0;

	/* configure the scrollbars, will also resize work_area */
	_XmHTMLCheckScrollBars(html);

	HTML_ATTR(in_layout) = False;
	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLLayout End\n"));
	return;
}

/*****
* Name: 		_XmHTMLResize
* Return Type: 	void
* Description: 	xmHTMLWidgetClass resize method.
* In:
*	w:			resized widget.
* Returns:
*	nothing
*****/
void
_XmHTMLResize(Widget w)
{
	Boolean do_expose;
	XmHTMLWidget html = (XmHTMLWidget)w;
	int foo, vsb_width;
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLResize Start\n"));

	/* No needless resizing */
	if(!tka->IsRealized(w))
	{
		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLResize end, widget not "
			"realized.\n"));
		return;
	}

	if(HTML_ATTR(in_layout))
	{
		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLResize end, layout flag is set.\n"));
		return;
	}

	_XmHTMLGetScrollDim(html, &foo, &vsb_width);

	/* No change in size, return */
	if((CORE_ATTR(height) == HTML_ATTR(work_height)) &&
		(CORE_ATTR(width) == (HTML_ATTR(work_width) + HTML_ATTR(margin_width) +
			vsb_width)))
	{
		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLResize End, no change in size\n"));
		return;
	}

	/*
	* Check if we have to do layout and generate an expose event.
	* When the widget shrinks, X does not generate an expose event.
	* We want to recompute layout and generate an expose event when the
	* width changes.
	* When the height increases, we only want to generate a partial
	* exposure (this gets handled in Redisplay).
	*/
	do_expose = (CORE_ATTR(width) != (HTML_ATTR(work_width) +
		HTML_ATTR(margin_width) + vsb_width));

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLResize, new window dimensions: %ix%i.\n",
		CORE_ATTR(width) - HTML_ATTR(margin_width), HTML_ATTR(work_height)));
	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLResize, generating expose event : %s.\n",
		(do_expose ? "yes" : "no")));

	/* Clear current visible text */
	if(do_expose)
	{
		/*
		* save new height & width of visible area.
		* subtract margin_width once to minimize number of calcs in
		* the paint routines: every thing rendered starts at an x position
		* of margin_width.
		*/
		HTML_ATTR(work_width) = CORE_ATTR(width) - HTML_ATTR(margin_width) -
			vsb_width;
		HTML_ATTR(work_height)= CORE_ATTR(height);

		/* Recompute layout */
		_XmHTMLLayout(html);

		/* Clear current text area and generate an expose event */
		_XmHTMLClearArea(html, 0, 0, CORE_ATTR(width), CORE_ATTR(height));
	}
	/* change in height */
	else
	{
		/*
		* Get new start & end points for the paint engine
		* We have two cases: shrink or stretch.
		* When stretched, we generate an exposure event for the added
		* area and let DrawRedisplay figure it out. If shrunk, adjust
		* end point for the paint engine.
		*/

		/* Window has been stretched */
		if(HTML_ATTR(work_height) < CORE_ATTR(height))
		{
			/*
			* formatted_height has some formatting offsets in it. Need
			* to subtract them first.
			*/
			int max = HTML_ATTR(formatted_height) - HTML_ATTR(margin_height) -
				HTML_ATTR(default_font)->descent;
			/*
			* If the stretch is so large that the entire text will fit
			* in the new window height, remove the scrollbars by resetting
			* the vertical scrollbar position.
			*/
			if(CORE_ATTR(height) > max)
				HTML_ATTR(scroll_y) = 0;

			/* save new height */
			HTML_ATTR(work_height) = CORE_ATTR(height);

			/* reset scrollbars (this will also resize the work_area) */
			_XmHTMLCheckScrollBars(html);

			/*
			* just clear the entire area. Will generate a double exposure
			* but everything will be painted as it should.
			*/
			_XmHTMLClearArea(html, 0, 0, CORE_ATTR(width), CORE_ATTR(height));
		}
		/* window has been shrunk */
		else
		{
			XmHTMLObjectTable *start, *end;
			int y;

			/* get new y maximum */
			y = HTML_ATTR(scroll_y) + CORE_ATTR(height);

			/* Starting point is end of previous stream */
			start = (HTML_ATTR(paint_end) == NULL ? HTML_ATTR(formatted):
				HTML_ATTR(paint_end));

			/* Walk backwards until we reach the desired height */
			for(end = start; end != NULL && y >= end->y; end = end->prev);

			/* save end point */
			HTML_ATTR(paint_end) = end;

			/* save new height */
			HTML_ATTR(work_height) = CORE_ATTR(height);

			/* reset scrollbars (this will also resize the work_area) */
			_XmHTMLCheckScrollBars(html);

			/* no need to paint */
		}
	}
	/* resize XmHTML's frame childs */
	if(HTML_ATTR(nframes))
	{
		_XmHTMLDebug(1, ("XmHTML.c: Resize, calling ReconfigureFrames\n"));
		_XmHTMLReconfigureFrames(html);
	}

	_XmHTMLSetScrollBars(html);

	_XmHTMLDebug(1, ("XmHTML.c: Resize End\n"));

	return;
}

/*****
* Name: 		_XmHTMLClearArea
* Return Type: 	void
* Description: 	XClearArea wrapper. Does form component updating as well.
* In:
*	html:		XmHTMLWidget id;
*	x,y:		upper left corner of region to be updated;
*	width:		width of region;
*	height:		height of region;
* Returns:
*
*****/
void
_XmHTMLClearArea(XmHTMLWidget html, int x, int y, int width, int height)
{
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	/*****
	* Sanity check, we must have a window before we can clear anything.
	* Could happen when XmNmappedWhenManaged has been set to False.
	*****/
	if(!tka->IsRealized((Widget)html) || tka->win == None)
		return;

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLClearArea Start, x: %i, y: %i, width: "
		"%i height: %i.\n", x, y, width, height));

	/* first scroll form widgets if we have them */
	if(HTML_ATTR(form_data))
	{
		_XmHTMLScrollForm(html);
		tka->ClearArea(tka->dpy, tka->win, x, y, width, height, False);
		_XmHTMLRefresh(html, x, y, width, height);

		/*****
		* If we've got a form but no clipmask, fallback to less-smooth
		* scrolling
		*****/
#if 0
		if(HTML_ATTR(form_data) && HTML_ATTR(form_data)->can_clip == False)
			_XmHTMLRaiseFormWidgets(html);
#endif
	}
	else
		tka->ClearArea(tka->dpy, tka->win, x, y, width, height, True);

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLClearArea End.\n"));
}

/*****
* Name: 		_XmHTMLCheckXCC
* Return Type: 	void
* Description: 	creates an XCC for the given XmHTMLWidget if one hasn't been
*				allocated yet.
* In:
*	html:		XmHTMLWidget id;
* Returns:
*	nothing
*****/
void
_XmHTMLCheckXCC(XmHTMLWidget html)
{
	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLCheckXCC Start\n"));

	/*
	* CheckXCC is called each time an image is loaded, so it's quite
	* usefull if we have a GC around by the time the widget is being
	* mapped to the display.
	* Our SubstructureNotify event handler can fail in some cases leading to
	* a situation where we don't have a GC when images are about to be
	* rendered (especially background images can cause a problem, they
	* are at the top of the text).
	*/
	_XmHTMLCheckGC(html);

	/*
	* Create an XCC.
	* XmHTML never decides whether or not to use a private or standard
	* colormap. A private colormap can be supplied by setting it on the
	* widget's parent, we know how to deal with that.
	*/
	if(!HTML_ATTR(xcc))
	{
		VISUAL *visual = NULL;
		COLORMAP cmap  = TkaGetColormap(html);

		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLCheckXCC: creating an XCC\n"));

		/* get a visual, always returns a good one so no check required */
		visual = XCCGetParentVisual((Widget)html);

		/* create an xcc for this widget */
		HTML_ATTR(xcc) = XCCCreate((Widget)html, visual, cmap);
	}
	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLCheckXCC End\n"));
}

/*****
* Name: 		_XmHTMLMoveToPos
* Return Type: 	void
* Description: 	scroll the working area with the given value
* In:
*	w:			originator
*	html:		XmHTMLWidget
*	value:		position to scroll to
* Returns:
*	nothing
*****/
void
_XmHTMLMoveToPos(Widget w, XmHTMLWidget html, int value)
{
	int inc, x, y, width, height;
	ToolkitAbstraction *tka = HTML_ATTR(tka);
	int vsb_width = 0, hsb_height = 0;
	XmHTMLFormData *form = HTML_ATTR(form_data);

	/* sanity check */
	if(value < 0)
		return;

	/* default exposure region */
	x = y = 0;
	width = CORE_ATTR(width);
	height = CORE_ATTR(height);

	/*
	* need to adjust slider position since we may not be called from
	* the scrollbar callback handler.
	*/
	TkaScrollbarSliderSetPosition(w, value);

	/*****
	* If we've got a form but no clipmask, fallback to less-smooth
	* scrolling
	*****/
#if 0
	if(form && form->can_clip == False)
		LowerFormWidgets(html);
#endif

	/* vertical scrolling */
	if(w == HTML_ATTR(vsb))
	{
		/*
		* clicking on the slider causes activation of the scrollbar
		* callbacks. Since there is no real movement, just return.
		* Not doing this will cause an entire redraw of the window.
		*/
		if(value == HTML_ATTR(scroll_y))
			return;		/* fix 01/20/97-01 kdh */

		/* save line number */
		_XmHTMLSetCurrentLineNumber(html, value);

		/* moving down (text moving up) */
		if(value > HTML_ATTR(scroll_y))
		{
			inc = value - HTML_ATTR(scroll_y);

			/* save new value */
			HTML_ATTR(scroll_y) = value;

			/* save new paint engine start */
			HTML_ATTR(paint_start) = HTML_ATTR(paint_end);

			/* small increment */
			if(inc < HTML_ATTR(work_height))
			{
				/*****
				* See if we have a hsb. If we have one, we need to add
				* the height of the hsb to the region requiring updating.
				*****/
				if(HTML_ATTR(needs_hsb))
#ifdef NO_XLIB_ILLEGAL_ACCESS
					_XmHTMLGetScrollDim(html, &hsb_height, &vsb_width);
#else
					hsb_height = ATTR_CORE(HTML_ATTR(hsb), height);
#endif
				/****
				* Set clipmask for proper form scrolling only if we're
				* in range
				*****/
				if(form && form->can_clip)
				{
					int xs = form->x - HTML_ATTR(scroll_x);
					int ys = form->y - value;

					if(ys + form->height < 0 || ys > HTML_ATTR(work_height) ||
						xs + form->width < 0 || xs > HTML_ATTR(work_width))
						;
					else
					{
						tka->SetClipMask(tka->dpy, HTML_ATTR(gc), form->clip);
						tka->SetClipOrigin(tka->dpy, HTML_ATTR(gc), xs, ys);
					}
				}

				/* copy visible part upward */
				tka->CopyArea(tka->dpy, tka->win, tka->win, HTML_ATTR(gc), 0,
					inc, HTML_ATTR(work_width) + HTML_ATTR(margin_width),
					HTML_ATTR(work_height) - inc - hsb_height, 0, 0);

				if(form)
					tka->SetClipMask(tka->dpy, HTML_ATTR(gc), None);

				/* clear area below */
				x = 0;
				y = HTML_ATTR(work_height) - inc - hsb_height;
				width = CORE_ATTR(width);
				height = inc + hsb_height;
			}
			/* large increment, use default area */
		}
		/* moving up (text moving down) */
		else
		{
			inc = HTML_ATTR(scroll_y) - value;

			/* save new value */
			HTML_ATTR(scroll_y) = value;

			/* small increment */
			if(inc < HTML_ATTR(work_height))
			{
				/****
				* Set clipmask for proper form scrolling only if we're
				* in range
				*****/
				if(form && form->can_clip)
				{
					int xs = form->x - HTML_ATTR(scroll_x);
					int ys = form->y - value;

					if(ys + form->height < 0 || ys > HTML_ATTR(work_height) ||
						xs + form->width < 0 || xs > HTML_ATTR(work_width))
						;
					else
					{
						tka->SetClipMask(tka->dpy, HTML_ATTR(gc), form->clip);
						tka->SetClipOrigin(tka->dpy, HTML_ATTR(gc), xs, ys);
					}
				}

				/* copy area down */
				tka->CopyArea(tka->dpy, tka->win, tka->win, HTML_ATTR(gc), 0, 0,
					HTML_ATTR(work_width) + HTML_ATTR(margin_width),
					HTML_ATTR(work_height) - inc, 0, inc);

				/* save paint engine end */
				HTML_ATTR(paint_end) = HTML_ATTR(paint_start);

				if(form)
					tka->SetClipMask(tka->dpy, HTML_ATTR(gc), None);

				/* clear area above */
				x = y = 0;
				width = CORE_ATTR(width);
				height = inc;
			}
			/* large increment, use default area */
		}
	}
	/* horizontal scrolling */
	else if(w == HTML_ATTR(hsb))
	{
		/*
		* clicking on the slider causes activation of the scrollbar
		* callbacks. Since there is no real movement, just return.
		* Not doing this will cause an entire redraw of the window.
		*/
		if(value == HTML_ATTR(scroll_x))
			return;		/* fix 01/20/97-01 kdh */

		/* moving right (text moving left) */
		if (value > HTML_ATTR(scroll_x))
		{
			inc = value - HTML_ATTR(scroll_x);

			/* save new value */
			HTML_ATTR(scroll_x) = value;

			/* small increment */
			if(inc < HTML_ATTR(work_width))
			{
				/*
				* See if we have a vsb. If we have, no additional offset
				* required, otherwise we also have to clear the space that
				* has been reserved for it.
				*/
				if(!HTML_ATTR(needs_vsb))
#ifdef NO_XLIB_ILLEGAL_ACCESS
					_XmHTMLGetScrollDim(html, &hsb_height, &vsb_width);
#else
					vsb_width = ATTR_CORE(HTML_ATTR(vsb), width);
#endif
				/****
				* Set clipmask for proper form scrolling only if we're
				* in range
				*****/
				if(form && form->can_clip)
				{
					int xs = form->x - value;
					int ys = form->y - HTML_ATTR(scroll_y);

					if(xs + form->width < 0 || xs > HTML_ATTR(work_width) ||
						ys + form->height < 0 || ys > HTML_ATTR(work_height))
						;
					else
					{
						tka->SetClipMask(tka->dpy, HTML_ATTR(gc), form->clip);
						tka->SetClipOrigin(tka->dpy, HTML_ATTR(gc), xs, ys);
					}
				}

				/* copy area to the left */
				tka->CopyArea(tka->dpy, tka->win, tka->win, HTML_ATTR(gc),
					inc, 0, HTML_ATTR(work_width) - inc,
					HTML_ATTR(work_height), 0, 0);

				if(form)
					tka->SetClipMask(tka->dpy, HTML_ATTR(gc), None);

				/* clear area on right */
				x = HTML_ATTR(work_width) - inc;
				y = 0;
				width = inc + HTML_ATTR(margin_width) + vsb_width;
				height = HTML_ATTR(work_height);
			}
			/* large increment, use default area */
		}
		/* moving left (text moving right) */
		else
		{
			inc = HTML_ATTR(scroll_x) - value;

			/* save new value */
			HTML_ATTR(scroll_x) = value;

			/* small increment */
			if(inc < HTML_ATTR(work_width))
			{
				if(!HTML_ATTR(needs_vsb))
#ifdef NO_XLIB_ILLEGAL_ACCESS
					_XmHTMLGetScrollDim(html, &hsb_height, &vsb_width);
#else
					vsb_width = ATTR_CORE(HTML_ATTR(vsb), width);
#endif
				/****
				* Set clipmask for proper form scrolling only if we're
				* in range
				*****/
				if(form && form->can_clip)
				{
					int xs = form->x - value;
					int ys = form->y - HTML_ATTR(scroll_y);

					if(xs + form->width < 0 || xs > HTML_ATTR(work_width) ||
						ys + form->height < 0 || ys > HTML_ATTR(work_height))
						;
					else
					{
						tka->SetClipMask(tka->dpy, HTML_ATTR(gc), form->clip);
						tka->SetClipOrigin(tka->dpy, HTML_ATTR(gc), xs, ys);
					}
				}

				/* copy area to the right */
				/* fix 01/24/97-01, kdh */
				tka->CopyArea(tka->dpy, tka->win, tka->win, HTML_ATTR(gc), 0, 0,
					HTML_ATTR(work_width) - inc + HTML_ATTR(margin_width) +
						vsb_width,
					HTML_ATTR(work_height), inc, 0);

				if(form)
					tka->SetClipMask(tka->dpy, HTML_ATTR(gc), None);

				/* clear area on left */
				x = y = 0;
				width = inc;
				height = HTML_ATTR(work_height);
			}
			/* large increment, use default area */
		}
	}
	else
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLMoveToPos"), XMHTML_MSG_10);
		return;
	}

	/* update display */
	if (0) { /* Not radical enough it seems.
              Causes refresh problems when scroll bar is moved
              ZSS: March 2012 */
      _XmHTMLClearArea(html, x, y, width, height);
   } else { /* overkill, but seems to get rid of refresh problem.
               It does add some unpleasant flicker though ...*/
      XmHTMLRefresh((Widget)html);
   }
}
