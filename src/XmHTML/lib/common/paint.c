#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* paint.c : XmHTML rendering routines
*
* This file Version	$Revision$
*
* Creation date:		Fri Dec  6 19:50:20 GMT+0100 1996
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
* (C)Copyright 1995-1996 Ripley Software Development
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
* Revision 1.19  1998/04/27 07:02:05  newt
* A few bugfixes in anchor painting + tka stuff
*
* Revision 1.18  1998/04/04 06:28:17  newt
* XmHTML Beta 1.1.3
*
* Revision 1.17  1997/10/23 00:25:06  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.16  1997/08/31 17:37:20  newt
* log edit
*
* Revision 1.15  1997/08/30 01:17:29  newt
* Text layout for <PRE></PRE> is now properly handled.
* Added anchor highlighting support.
* Generalized anchor button rendering.
* Added support for colored <HR>.
*
* Revision 1.14  1997/08/01 13:05:28  newt
* Lots of bugfixes: baseline adjustment of images, forms components or large
* fonts, animated gif disposal handling. Reduced data storage.
*
* Revision 1.13  1997/05/28 01:52:43  newt
* Bugfix 05/26/97-01. Form layout changes.
*
* Revision 1.12  1997/04/29 14:30:19  newt
* Bugfix 04/26/97-01. Fixed SetText to use dimensions of baseline word when
* updating the object data.
*
* Revision 1.11  1997/04/03 05:40:21  newt
* Anchor and word rendering of mixed fonts finally works. Fixed anchor
* transparency for documents with a body images. Fixed image anchor scrolling
* problem.
*
* Revision 1.10  1997/03/28 07:18:37  newt
* Alignment bugfix: now only done with a positive offset.
* Bugfix in frame disposal: index was one frame too early.
* Bugfix in image anchor rendering: now done before checking exposure region.
*
* Revision 1.9  1997/03/20 08:12:38  newt
* SetText: images now use their vertical alignment specs for baseline
* adjustment.
*
* Revision 1.8  1997/03/11 19:57:04  newt
* SetText now does both text and image layout. 
* DrawImage now does animated gifs and transparent images
*
* Revision 1.7  1997/03/04 18:48:52  newt
* Animation stuff. Fixed a spacing bug in SetText
*
* Revision 1.6  1997/03/04 01:00:55  newt
* ?
*
* Revision 1.5  1997/03/02 23:20:31  newt
* Added setting and rendering of images. image-related changes to SetText. 
* SetText now also ``glues'' words together properly
*
* Revision 1.4  1997/02/11 02:09:28  newt
* Fair amount of changes: text layout has been completely changed
*
* Revision 1.3  1997/01/09 06:55:47  newt
* expanded copyright marker
*
* Revision 1.2  1997/01/09 06:46:39  newt
* lots of changes: painting now divided in a layout en rendering part.
*
* Revision 1.1  1996/12/19 02:17:12  newt
* Initial Revision
*
*****/ 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>

#include "toolkit.h"
#include XmHTMLPrivateHeader

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/
/* main rendering routines */
static void DrawText(XmHTMLWidget html, XmHTMLObjectTableElement data);
static void DrawAnchor(XmHTMLWidget html, XmHTMLObjectTableElement data);
static void DrawImageAnchor(XmHTMLWidget html, XmHTMLObjectTableElement data);
static void DrawRule(XmHTMLWidget html, XmHTMLObjectTableElement data);
static void DrawBullet(XmHTMLWidget html, XmHTMLObjectTableElement data);
static XmHTMLObjectTableElement DrawTable(XmHTMLWidget html,
	XmHTMLObjectTableElement data, XmHTMLObjectTableElement data_end);

/* various helper routines */
static void DrawAnchorButton(ToolkitAbstraction *tka, int x, int y,
	Dimension width, Dimension height, GC top, GC bottom);
static void DrawFrame(XmHTMLWidget html, XmHTMLImage *image, int xs, int ys);
static void DrawTableBorder(XmHTMLWidget html, XmHTMLTable *table);
static void DrawCellFrame(XmHTMLWidget html, TableCell *cell);
static void DrawCellContent(XmHTMLWidget html, XmHTMLObjectTableElement start,
	XmHTMLObjectTableElement end);
static void TimerCB(XtPointer data, XtIntervalId *id);

/*** Private Variable Declarations ***/
#define DRAW_TOP		CELL_TOP
#define DRAW_BOTTOM		CELL_BOTTOM
#define DRAW_LEFT		CELL_LEFT
#define DRAW_RIGHT		CELL_RIGHT
#define DRAW_BOX		CELL_BOX
#define DRAW_NONE		CELL_NONE

/*****
* Same note as for anchor width adjustment applies here to, except that we
* _must_ do this locally instead of adjusting the width field of a word
* directly. The width field is used by the line breaking algorithm in SetText,
* so modifying it will make the words wider every time the widget is resized.
* Luckily this is only required for underlined/striked words.
*****/
#define AdjustWordWidth { \
	width = words[i].width; \
	if(i < nwords-1 && words[i].line == words[i+1].line) { \
			width = words[i+1].x - words[i].x; \
	} \
}

/********
**** Private paint functions (display independent)
********/

/*****
* Name:			DrawText
* Return Type: 	void
* Description: 	main text rendering engine.
* In: 
*	html:		XmHTMLWidget id;
*	data:		element to be painted;
* Returns:
*	nothing.
* Note:
*	Used for both regular and preformatted text.
*****/
static void
DrawText(XmHTMLWidget html, XmHTMLObjectTableElement data)
{
	int width, ys, xs, nwords = data->n_words;
	XmHTMLWord *words = data->words;
	GC gc = HTML_ATTR(gc);
	ToolkitAbstraction *tka = HTML_ATTR(tka);
	register int i;
	register XmHTMLWord *tmp;

	if(!nwords)
		return;

	/* only need to set this once */
	tka->SetForeground(tka->dpy, gc, data->fg);

	for(i = 0 ; i < nwords; i++)
	{
		tmp = &words[i];

		/* 
		* When any of the two cases below is true, the text at the current
		* position is outside the exposed area. Not doing this check would 
		* cause a visible flicker of the screen when scrolling: the entire 
		* line would be repainted, even the invisible text.
		* And we sure don't want to render any linebreaks, looks pretty ugly.
		* (this test is a lot cheaper than doing ``invisible'' rendering)
		*/
		if((HTML_ATTR(paint_y) > tmp->y + tmp->height ||
			HTML_ATTR(paint_height) < tmp->y) ||
			(HTML_ATTR(paint_x) > tmp->x + tmp->width ||
			HTML_ATTR(paint_width) < tmp->x) || tmp->type == OBJ_BLOCK)
		{
#ifdef DEBUG
			if(HTML_ATTR(paint_y) > tmp->y + tmp->height ||
				HTML_ATTR(paint_height) < tmp->y)
				_XmHTMLDebug(16, ("paint.c: DrawText, skipping %s, outside "
					"vertical range.\n", tmp->word));
			else if(tmp->type != OBJ_BLOCK)
				_XmHTMLDebug(16, ("paint.c: DrawText, skipping %s, outside "
					"horizontal range.\n", tmp->word));
			else
				_XmHTMLDebug(16, ("paint.c: DrawText, skipping break.\n"));
#endif
			continue;
		}

		xs = tmp->x - HTML_ATTR(scroll_x);
		ys = tmp->y - HTML_ATTR(scroll_y);

		tka->DrawString(tka->dpy, tka->win, words[0].font, gc, xs, ys,
			tmp->word, tmp->len);

		if(tmp->line_data & LINE_UNDER)
		{
			int dy;
			/* 
			* vertical position for underline, barely connects with the 
			* underside of the ``deepest'' character.
			*/
			dy = ys + tmp->base->font->ul_offset;
			AdjustWordWidth;

			tka->SetLineAttributes(tka->dpy, gc,
				tmp->base->font->ul_thickness, 
				(tmp->line_data & LINE_SOLID ? 
					tka->line_style[GC_LINE_SOLID] :
					tka->line_style[GC_LINE_DOUBLE_DASH]),
				tka->cap_style[GC_CAP_BUTT],
				tka->join_style[GC_JOIN_BEVEL]);

			tka->DrawLine(tka->dpy, tka->win, gc, xs, dy, xs + width, dy);
			if(tmp->line_data & LINE_DOUBLE)
				tka->DrawLine(tka->dpy, tka->win, gc, xs, dy+2, xs + width, 
					dy+2);
		}
		if(tmp->line_data & LINE_STRIKE)
		{
			int dy;
			/* strikeout line is somewhere near the middle of a line */
			dy = ys - tmp->base->font->st_offset;
			AdjustWordWidth;

			tka->SetLineAttributes(tka->dpy, gc,
				tmp->base->font->st_thickness,
				tka->line_style[GC_LINE_SOLID],
				tka->cap_style[GC_CAP_BUTT],
				tka->join_style[GC_JOIN_BEVEL]);
			tka->DrawLine(tka->dpy, tka->win, gc, xs, dy, xs + width, dy);
		}
	}
}

/*****
* Name:			DrawAnchor
* Return Type: 	void
* Description: 	main text anchor renderer.
* In: 
*	html:		XmHTMLWidget id;
*	data:		anchor to be painted;
* Returns:
*	nothing.
* Note:
*	This routine handles all textual anchor stuff. It paints anchors according
*	to the selected anchor style and (optionally) performs anchor highlighting.
*	Image anchors are rendered by DrawImageAnchor.
*****/
static void
DrawAnchor(XmHTMLWidget html, XmHTMLObjectTableElement data)
{
	int x, xs, y, ys, width, start, nwords = 0;
	GC gc = HTML_ATTR(gc);
	GC hiGC, topGC, bottomGC;
	XmHTMLfont *font;
	register int i, j;
	XmHTMLWord **all_words = NULL, *words = NULL, *tmp;
	XmHTMLObjectTableElement a_start, a_end, temp;
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	/* pick up the real start of this anchor */
	for(a_start = data; a_start && a_start->anchor == data->anchor;
		a_start = a_start->prev);

	/* sanity, should never happen */
	if(a_start == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "DrawAnchor"), XMHTML_MSG_90);
		return;
	}

	/* previous loop always walks back one to many */
	a_start = a_start->next;

	/* pick up the real end of this anchor and count the words */
	for(a_end = a_start, nwords = 0;
		a_end != NULL && a_end->anchor == a_start->anchor; a_end = a_end->next)
	{
		/* ignore image words, they get handled by DrawImageAnchor. */
		if(!(a_end->text_data & TEXT_IMAGE) &&
			!(a_end->text_data & TEXT_BREAK))
			nwords += a_end->n_words;
	}

	_XmHTMLDebug(16, ("paint.c: DrawAnchor, anchor contains %i words\n",
		nwords));

	/* sanity check */
	if(!nwords)
		return;		/* fix 01/30/97-01, kdh */

	/* 
	* put all anchor words into an array if this anchor spans multiple
	* objects (as can be the case with font changes within an anchor)
	* If this isn't the case, just use the words of the current data
	* object.
	*/
	if(a_start->next != a_end)
	{
		_XmHTMLDebug(16, ("paint.c: DrawAnchor, allocating a word array for "
			"%i words\n", nwords));

		all_words = (XmHTMLWord**)calloc(nwords, sizeof(XmHTMLWord*));

		i = 0;
		for(temp = a_start; temp != a_end; temp = temp->next)
		{
			/* ignore image words, they get handled by DrawImageAnchor. */
			if(!(temp->text_data & TEXT_IMAGE) &&
				!(temp->text_data & TEXT_BREAK))
			{
				for(j = 0 ; j < temp->n_words; j++)
					all_words[i++] = &(temp->words[j]);
			}
		}
		words = NULL;
	}
	else
	{
		_XmHTMLDebug(16, ("paint.c: DrawAnchor, not allocating a word array, "
			"all words belong to the same object.\n"));
		words = data->words;
	}

	/*
	* this is used for drawing the bounding rectangle of an anchor.
	* When an anchor is encountered, width is used to compute the total
	* width of a rectangle surrounding all anchor words on the same line.
	* The bounding rectangle drawn extends a little bit to the left and
	* right of the anchor.
	*/
	width = (words ? words[0].width : all_words[0]->width);
	/* extend to the left */
	x = (words ? words[0].x : all_words[0]->x) - HTML_ATTR(scroll_x) - 2;
	y = (words ? words[0].y : all_words[0]->y) - HTML_ATTR(scroll_y);
	i = start = 0;

	do
	{
		tmp = (words ? &words[i] : all_words[i]);

		/* anchors are always painted */
		ys = tmp->y - HTML_ATTR(scroll_y);
		xs = tmp->x - HTML_ATTR(scroll_x);

		/* baseline font */
		font = tmp->base->font;

		_XmHTMLFullDebug(16, ("paint.c: painting anchor, x = %i, y = %i\n",
			tmp->x, tmp->y));

		/* compute total width of all words on this line */
		if(ys == y)
			width = xs + tmp->width - x;
		/* extend to the right if this word has a trailing space */
		width += (tmp->posbits & TEXT_SPACE_TRAIL ? 2 : 1);

		if(ys != y || i == nwords-1)
		{
			/*****
			* Anchor painting. We first make the distinction between documents
			* with and without a body image.
			* Then we check if we need to paint the anchors as pushbuttons
			* and then we check if we are to perform anchor highlighting.
			*****/
			if(HTML_ATTR(body_image) == NULL)
			{
				/*****
				* No body image present.
				* Check if we are to paint the anchors as pushbuttons.
				*****/
				if(HTML_ATTR(anchor_buttons))
				{
					if(data->anchor_state == ANCHOR_INSELECT &&
						HTML_ATTR(highlight_on_enter))
					{
						/* can only happen if XmNhighlightOnEnter is set */
						hiGC = MGR_ATTR(highlight_GC);
						topGC = MGR_ATTR(top_shadow_GC);
						bottomGC = MGR_ATTR(bottom_shadow_GC);
					}
					else if(data->anchor_state == ANCHOR_SELECTED)
					{
						hiGC = MGR_ATTR(highlight_GC);
						topGC = MGR_ATTR(bottom_shadow_GC);
						bottomGC = MGR_ATTR(top_shadow_GC);
					}
					else	/* button is unselected */
					{
						/* use object's background */
						tka->SetForeground(tka->dpy, gc, data->bg);
						hiGC = gc;
						topGC = MGR_ATTR(top_shadow_GC);
						bottomGC = MGR_ATTR(bottom_shadow_GC);
					}
					if(HTML_ATTR(highlight_on_enter))
					{
						/* paint button highlighting */
						tka->FillRectangle(tka->dpy, tka->win, hiGC, x,
							y - font->ascent, width, font->lineheight);
					}
					/* and draw as requested */
					DrawAnchorButton(tka, x, y - font->ascent,
						width, font->lineheight, topGC, bottomGC);
				}
				else	
				{
					/*****
					* No anchor buttons. Determine which color to use to paint
					* the bounding box around the anchor.
					* Note: without a body image, anchor highlighting is
					* achieved by painting the bounding box in the highlight
					* color.
					*****/
					if(data->anchor_state == ANCHOR_INSELECT)
						tka->SetForeground(tka->dpy, gc,
								MGR_ATTR(highlight_color));
					else if(data->anchor_state == ANCHOR_SELECTED)
						tka->SetForeground(tka->dpy, gc,
								HTML_ATTR(anchor_activated_bg));
					else
						tka->SetForeground(tka->dpy, gc, data->bg);

					tka->FillRectangle(tka->dpy, tka->win, gc, x,
						y - font->ascent, width, font->lineheight);
				}
				/* set appropriate foreground color */
				tka->SetForeground(tka->dpy, gc,
					data->anchor_state == ANCHOR_SELECTED ? 
					HTML_ATTR(anchor_activated_fg) : data->fg);
			}
			else
			{
				/*****
				* We have a body image. Painting the buttons as above would
				* obliterate the part of the image under the anchor, so
				* if we are to perform button highlighting, we paint the
				* anchor's *text* in the highlight color.
				*****/
				if(HTML_ATTR(anchor_buttons))
				{
					if(data->anchor_state == ANCHOR_SELECTED)
					{
						topGC = MGR_ATTR(bottom_shadow_GC),
						bottomGC = MGR_ATTR(top_shadow_GC);
					}
					else	/* button is unselected or being selected */
					{
						topGC = MGR_ATTR(top_shadow_GC),
						bottomGC = MGR_ATTR(bottom_shadow_GC);
					}
					/* draw as requested */
					DrawAnchorButton(tka, x, y - font->ascent,
						width, font->lineheight, topGC, bottomGC);
				}
				/*****
				* no special stuff for anchor bounding box if we aren't
				* painting the anchors as buttons.
				*****/
				/* set appropriate foreground color */
				if(data->anchor_state == ANCHOR_INSELECT)
					tka->SetForeground(tka->dpy, gc,
						MGR_ATTR(highlight_color));
				else if(data->anchor_state == ANCHOR_SELECTED)
					tka->SetForeground(tka->dpy, gc,
							HTML_ATTR(anchor_activated_fg));
				else
					tka->SetForeground(tka->dpy, gc, data->fg);
			}
			/* 
			* paint all text. Need to do it here since the XFillRect call
			* would obliterate any text painted before it.
			*/

			if(words)
			{
				for(j = start; j < i+1; j++)
				{
					tka->DrawString(tka->dpy, tka->win, words[j].font, gc,
						words[j].x - HTML_ATTR(scroll_x),
						words[j].y - HTML_ATTR(scroll_y),
						words[j].word, words[j].len);
				}
			}
			else
			{
				for(j = start; j < i+1; j++)
				{
					tka->DrawString(tka->dpy, tka->win, all_words[j]->font, gc,
						all_words[j]->x - HTML_ATTR(scroll_x),
						all_words[j]->y - HTML_ATTR(scroll_y),
						all_words[j]->word, all_words[j]->len);
				}
			}

			/* Anchor buttons are never underlined, it looks ugly */
			if(!HTML_ATTR(anchor_buttons) && 
				(tmp->line_data & LINE_SOLID || tmp->line_data & LINE_DASHED))
			{
				int dy = y + font->ul_offset;
				tka->SetLineAttributes(tka->dpy, gc, 1, 
					(tmp->line_data & LINE_SOLID ?
						tka->line_style[GC_LINE_SOLID] :
						tka->line_style[GC_LINE_DOUBLE_DASH]),
					tka->cap_style[GC_CAP_BUTT],
					tka->join_style[GC_JOIN_BEVEL]);
				tka->DrawLine(tka->dpy, tka->win, gc, x+2, dy, x+width, dy);
				/* draw another line if requested */
				if(tmp->line_data & LINE_DOUBLE)
					tka->DrawLine(tka->dpy, tka->win, gc, x + 2, dy + 2,
						x + width, dy + 2);
			}
			if(tmp->line_data & LINE_STRIKE)
			{
				int dy = y - font->st_offset;
				tka->SetLineAttributes(tka->dpy, gc, 1,
						tka->line_style[GC_LINE_SOLID],
						tka->cap_style[GC_CAP_BUTT],
						tka->join_style[GC_JOIN_BEVEL]);
				tka->DrawLine(tka->dpy, tka->win, gc, x+2, dy,
						x + width - 2, dy);
			}
			/* stupid hack to get the last word of a broken anchor right */
			if(ys != y && i == nwords-1)
				i--;
			/* next word starts on another line */
			width = tmp->width;
			start = i;
			x = xs-2;
			y = ys;
		}
		i++;
	}
	while(i != nwords);

	if(words == NULL)
	{
		_XmHTMLDebug(16, ("paint.c: DrawAnchor, freeing allocated word "
			"array.\n"));
		free(all_words);	/* fix 05/26/97-01, kdh */
		/*****
		* adjust current object data as we have now updated a number of
		* objects in one go. We must use prev as _XmHTMLPaint will advance
		* to a_end itself.
		*****/
		data = (a_end ? a_end->prev : data);
	}

	/* flush out referrering hyperlink if we have a DrawAnchorData function */
	if(tka->DrawAnchorData)
	{
		nwords = data->n_words - 1;

		/*****
		* compute position for footnote marker: at the end of the last word
		* in this hyperlink. Currently only used by postscript output.
		*****/
		x = data->words[nwords].x + data->words[nwords].width;
		y = data->words[nwords].y;

		/* store */
		tka->DrawAnchorData(tka->dpy, tka->win, HTML_ATTR(gc), x, y, data);
	
	}
}

static void
DrawImageAnchor(XmHTMLWidget html, XmHTMLObjectTableElement data)
{
	int x, y, width, height;
	GC gc = HTML_ATTR(gc);
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	/*
	* this is used for drawing the bounding rectangle of an anchor.
	* When an anchor is encountered, width is used to compute the total
	* width of a rectangle surrounding all anchor words on the same line.
	* The bounding rectangle drawn extends a little bit to the left and
	* right of the anchor.
	*/
	width  = data->words->width + 2;
	height = data->words->height + 2;

	/* extend to the left */
	x = data->words->x - HTML_ATTR(scroll_x) - 1;
	/* and to the top */
	y = data->words->y - HTML_ATTR(scroll_y) - 1;

	if(data->words->image->border)
	{
		/* add border offsets as well */
		x -= data->words->image->border;
		y -= data->words->image->border;

		_XmHTMLFullDebug(16, ("paint.c: painting image anchor, x = %i, "
			"y = %i\n", data->x, data->y));

		if(HTML_ATTR(anchor_buttons))
		{
			GC top, bottom;
			if(HTML_ATTR(highlight_on_enter))
			{
				/* can only happen if XmNhighlightOnEnter is set */
				if(data->anchor_state == ANCHOR_INSELECT)
				{
					/* paint button highlighting */
					if(HTML_ATTR(body_image) == NULL && 
						data->words->image->clip != None)
						tka->FillRectangle(tka->dpy, tka->win,
							MGR_ATTR(highlight_GC), x, y , width, height);
					/* and draw as unselected */
					top    = MGR_ATTR(top_shadow_GC);
					bottom = MGR_ATTR(bottom_shadow_GC);
				}
				else if(data->anchor_state == ANCHOR_SELECTED)
				{
					/* paint button highlighting */
					if(HTML_ATTR(body_image) == NULL && 
						data->words->image->clip != None)
						tka->FillRectangle(tka->dpy, tka->win,
							MGR_ATTR(highlight_GC), x, y, width, height);

					/* and draw as selected */
					top    = MGR_ATTR(bottom_shadow_GC);
					bottom = MGR_ATTR(top_shadow_GC);
				}
				else	/* button is unselected */
				{
					/* restore correct background */
					if(HTML_ATTR(body_image) == NULL && 
						data->words->image->clip != None)
					{
						tka->SetForeground(tka->dpy, gc, HTML_ATTR(body_bg));
						tka->FillRectangle(tka->dpy, tka->win, gc, x, y,
								width, height);
						tka->SetForeground(tka->dpy, gc, HTML_ATTR(body_fg));
					}
					/* and draw as unselected */
					top    = MGR_ATTR(top_shadow_GC);
					bottom = MGR_ATTR(bottom_shadow_GC);
				}
			}
			else 	/* either selected or unselected */
			{
				if(data->anchor_state == ANCHOR_SELECTED)
				{
					top = MGR_ATTR(bottom_shadow_GC);
					bottom = MGR_ATTR(top_shadow_GC);
				}
				else
				{
					bottom = MGR_ATTR(bottom_shadow_GC);
					top = MGR_ATTR(top_shadow_GC);
				}
			}
			DrawAnchorButton(tka, x, y, width, height, top, bottom);
		}
		else
		{
			/* set line attribs */
			tka->SetLineAttributes(tka->dpy, gc, data->words->image->border,
					tka->line_style[GC_LINE_SOLID],
					tka->cap_style[GC_CAP_BUTT],
					tka->join_style[GC_JOIN_BEVEL]);

			/* draw background */
			/* fix 04/03/97-01, kdh */
			if(HTML_ATTR(body_image) == NULL)
			{
				if(data->anchor_state == ANCHOR_INSELECT)
					tka->SetForeground(tka->dpy, gc,
							MGR_ATTR(highlight_color));
				else if(data->anchor_state == ANCHOR_SELECTED)
					tka->SetForeground(tka->dpy, gc,
							HTML_ATTR(anchor_activated_bg));
				else
					tka->SetForeground(tka->dpy, gc, HTML_ATTR(body_bg));
				tka->FillRectangle(tka->dpy, tka->win, gc, x, y,
						width, height);
			}

			/* draw lines */
			tka->SetForeground(tka->dpy, gc,
				data->anchor_state == ANCHOR_SELECTED ?
					HTML_ATTR(anchor_activated_fg) : HTML_ATTR(anchor_fg));
			tka->DrawRectangle(tka->dpy, tka->win, gc, x, y, width, height);
		}
	}
	/* paint the alt text if images are disabled */
	if(!HTML_ATTR(images_enabled))
	{
		tka->SetForeground(tka->dpy, gc,
				data->anchor_state == ANCHOR_SELECTED ?
					HTML_ATTR(anchor_activated_fg) : HTML_ATTR(anchor_fg));

		/* put text inside bounding rectangle */
		x += data->words->image->width + 4;
		y += data->words->image->height/2 + 4;

		tka->DrawString(tka->dpy, tka->win, data->words->font, gc, x, y,
			data->words->word, data->words->len);
	}
}

/*
* To prevent racing conditions, we must first remove an
* existing timeout proc before we add a new one.
*/
#define REMOVE_TIMEOUTPROC(IMG) { \
	if(IMG->proc_id) \
	{ \
		_XmHTMLDebug(16, ("paint.c: DrawImage, removing animation %s " \
			"timeout\n", IMG->url)); \
		tka->RemoveTimeOut(IMG->proc_id); \
		IMG->proc_id = None; \
	} \
}

/*****
* Name:			TimerCB
* Return Type: 	void
* Description: 	Xt timer callback procedure. Used by animations
* In: 
*	data:		animation for which this timeout has been called;
*	id:			id of timer used to activate this function.
* Returns:
*	nothing.
*****/
static void
TimerCB(XtPointer data, XtIntervalId *id)
{
	XmHTMLImage *image = (XmHTMLImage*)data;

	/* freeze animation at current frame */
	if(image->html->html.freeze_animations)
	{
		ToolkitAbstraction *tka = image->html->html.tka;
		REMOVE_TIMEOUTPROC(image);
		return;
	}

	image->options |= IMG_FRAMEREFRESH;
	_XmHTMLDrawImage(image->html, image->owner, 0, True);
}

#define RESET_GC(MYGC) { \
	tka->SetClipOrigin(tka->dpy, MYGC, 0, 0); \
	tka->SetClipMask(tka->dpy, MYGC, None); \
}

#define GET_TILE_OFFSETS(TSX,TSY) { \
	int tile_width, tile_height, x_dist, y_dist; \
	int ntiles_x, ntiles_y; \
	int x_offset, y_offset, xd, yd; \
	/* adjust for logical screen offsets */ \
	xd = xs + fx; /* x-pos relative to upper-left screen corner */ \
	yd = ys + fy; /* y-pos relative to upper-left screen corner */ \
	tile_width  = HTML_ATTR(body_image)->width; \
	tile_height = HTML_ATTR(body_image)->height; \
	x_dist = HTML_ATTR(scroll_x) + xd; /* total distance covered so far */ \
	y_dist = HTML_ATTR(scroll_y) + yd; /* total distance covered so far */ \
	ntiles_x = (int)(x_dist/tile_width); /* no of horizontal tiles */ \
	ntiles_y = (int)(y_dist/tile_height); /* no of vertical tiles */ \
	x_offset = x_dist - (ntiles_x * tile_width); \
	y_offset = y_dist - (ntiles_y * tile_height); \
	TSX = xd - x_offset; \
	TSY = yd - y_offset; \
}

/*****
* Name: 		DrawFrame
* Return Type: 	void
* Description: 	animation driver, does frame disposal and renders a new
*				frame
* In: 
*	w:			XmHTMLWidget id
*	image:		image data
*	xs:			absolute screen x-coordinate
*	ys:			absolute screen y-coordinate
* Returns:
*	nothing
* Note:
*	Complex animations
*	------------------
*	Instead of drawing into the window directly, we draw into an internal
*	pixmap and blit this pixmap to the screen when all required processing
*	has been done, which is a lot faster than drawing on the screen directly.
*	Another advantage of this approach is that we always have a current state
*	available which can be used when an animation is scrolled on and off
*	screen (frame dimensions differ from logical screen dimensions or a
*	disposal method other than XmIMAGE_DISPOSE_NONE is to be used).
*
*	Easy animations
*   ---------------
*	Each frame is blitted to screen directly, only processing done is using a
*	possible clipmask (frame dimensions equal to logical screen dimensions and
*	a disposal method of XmIMAGE_DISPOSE_NONE).
*****/
static void
DrawFrame(XmHTMLWidget html, XmHTMLImage *image, int xs, int ys)
{
	int idx, width = 0, height = 0, fx, fy;
	GC gc = HTML_ATTR(gc);
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	_XmHTMLDebug(16, ("paint.c: DrawFrame, animation %s, frame %i, "
		"(x = %i, y = %i)\n", image->url, image->current_frame, xs, ys));

	/* first reset the gc */
	RESET_GC(gc);

	/*
	* First check if we are running this animation internally. If we aren't
	* we have a simple animation of which each frame has the same size and
	* no disposal method has been specified. This type of animations are blit
	* to screen directly.
	*/
	if(!ImageHasState(image))
	{
		/* index of current frame */
		idx = image->current_frame;
		width  = image->frames[idx].w;
		height = image->frames[idx].h;

		/* can happen when a frame falls outside the logical screen area */
		if(image->frames[idx].pixmap != None)
		{
			/* plug in the clipmask */
			if(image->frames[idx].clip)
			{
				tka->SetClipOrigin(tka->dpy, gc, xs, ys);
				tka->SetClipMask(tka->dpy, gc, image->frames[idx].clip);
			}
			/* blit frame to screen */
			tka->CopyArea(tka->dpy, image->frames[idx].pixmap, tka->win,
					gc, 0, 0, width, height, xs, ys);
		}
		/*
		* Jump to frame updating when we are not triggered
		* by an exposure, otherwise just return.
		*/
		if(ImageFrameRefresh(image))
			goto nextframe;
		return;
	}
	/*
	* If DrawFrame was triggered by an exposure, just blit current animation
	* state to screen and return immediatly.
	*/
	if(!ImageFrameRefresh(image))
	{
		tka->DrawImage(html, image, gc, 0, 0, image->width, image->height,
			xs, ys);
		return;
	}

	/*****
	* If we get here we are running the animation internally. First check the
	* disposal method and update the current state accordingly *before*
	* putting the next frame on the display. 
	* Pixmap can be None if a frame falls outside the logical screen area.
	* idx is the index of the previous frame (the frame that is currently
	* being displayed).
	*****/
	idx = image->current_frame ? image->current_frame - 1 : image->nframes - 1;

	if(image->frames[idx].pixmap != None)
	{
		fx     = image->frames[idx].x;
		fy     = image->frames[idx].y;
		width  = image->frames[idx].w;
		height = image->frames[idx].h;

		if(image->frames[idx].dispose == XmIMAGE_DISPOSE_BY_BACKGROUND)
		{
			_XmHTMLDebug(16, ("paint.c: DrawFrame, %s, disposing frame %i "
				"by background, x = %i, y = %i, %ix%i\n", image->url, idx,
				xs + fx, ys + fy, width, height));

			/* we have a body image; get proper background tile offsets. */
			if(HTML_ATTR(body_image))
			{
				/* we have a body image, compute correct tile offsets */
				int tsx, tsy;

				GET_TILE_OFFSETS(tsx,tsy);

				_XmHTMLDebug(16, ("paint.c: DrawFrame: background disposal "
					"uses a tile with origin at (%i,%i)\n", tsx, tsy));

				tka->SetFillStyle(tka->dpy, HTML_ATTR(bg_gc),
						tka->fill_style[GC_FILL_TILED]);
				tka->SetTile(tka->dpy, HTML_ATTR(bg_gc),
						HTML_ATTR(body_image)->pixmap);
				tka->SetTSOrigin(tka->dpy, HTML_ATTR(bg_gc),
						tsx - xs, tsy - ys);

				/* paint it. */
				tka->FillRectangle(tka->dpy, image->pixmap, HTML_ATTR(bg_gc),
					fx, fy, width, height);
			}
			/*
			* No body image, do a fillrect in background color. clipmasks
			* are ignored since we are already restoring to background!
			*/
			else
			{
				tka->SetForeground(tka->dpy, gc, HTML_ATTR(body_bg));
				tka->FillRectangle(tka->dpy, image->pixmap, gc, fx, fy, width,
						height);
				tka->SetForeground(tka->dpy, gc, HTML_ATTR(body_fg));
			}
		}
		/*
		* no disposal method but we have a clipmask. Need to plug in
		* the background image or color. As we are completely overlaying
		* the current image with the new image, we can safely erase
		* the entire contents of the current state with the wanted
		* background, after which we use the clipmask to copy the requested
		* parts of the image on screen.
		*
		* Please note that this is *only* done for the very first frame
		* of such an animation. All other animations, whether they have a
		* clipmask or not, are put on top of this frame. Doing it for
		* other frames as well would lead to unwanted results as the
		* underlying portions of the animation would be replaced with the
		* current background, and thereby violating the none disposal
		* method logic.
		*/
		else if(image->frames[idx].dispose == XmIMAGE_DISPOSE_NONE &&
			idx == 0 && image->frames[idx].clip != None)
		{
			/* we have a body image, compute correct tile offsets */
			if(HTML_ATTR(body_image))
			{
				int tsx, tsy;

				GET_TILE_OFFSETS(tsx,tsy);

				_XmHTMLDebug(16, ("paint.c: DrawFrame: background disposal "
					"uses a tile with origin at (%i,%i)\n", tsx, tsy));

				/* update gc values */
				tka->SetFillStyle(tka->dpy, HTML_ATTR(bg_gc),
						tka->fill_style[GC_FILL_TILED]);
				tka->SetTile(tka->dpy, HTML_ATTR(bg_gc),
						HTML_ATTR(body_image)->pixmap);
				tka->SetTSOrigin(tka->dpy, HTML_ATTR(bg_gc),
						tsx - xs, tsy - ys);

				/* do a fillrect to render the background image */ 
				tka->FillRectangle(tka->dpy, image->pixmap, HTML_ATTR(bg_gc),
					fx, fy, width, height);
				/*
				* no need to reset the background gc, its only used for
				* overall background rendering.
				*/
			}
			else
			{
				/* do a plain fillrect in current background color */
				tka->SetForeground(tka->dpy, gc, HTML_ATTR(body_bg));
				tka->FillRectangle(tka->dpy, image->pixmap, gc, fx, fy, width,
						height);
				tka->SetForeground(tka->dpy, gc, HTML_ATTR(body_fg));
			}

			/* now plug in the clipmask */
			tka->SetClipOrigin(tka->dpy, gc, fx, fy);
			tka->SetClipMask(tka->dpy, gc, image->frames[idx].clip);

			/* paint it. Use full image dimensions */
			tka->CopyArea(tka->dpy, image->frames[idx].pixmap, image->pixmap,
				gc, 0, 0, width, height, fx, fy);
		}
		/* dispose by previous (the only one to have a prev_state) */
		else if(image->frames[idx].prev_state != None)
		{
			/* plug in the clipmask */
			if(image->frames[idx].clip)
			{
				/* set gc values */
				tka->SetClipOrigin(tka->dpy, gc, fx, fy);
				tka->SetClipMask(tka->dpy, gc, image->frames[idx].clip);
			}
			/* put previous screen state on current state */
			tka->CopyArea(tka->dpy, image->frames[idx].prev_state,
					image->pixmap, gc, 0, 0, width, height, fx, fy);
		}
	}
	/* reset gc */
	RESET_GC(gc);

	/* index of current frame */
	idx = image->current_frame;

	/* can happen when a frame falls outside the logical screen area */
	if(image->frames[idx].pixmap != None)
	{
		fx      = image->frames[idx].x;
		fy      = image->frames[idx].y;
		width   = image->frames[idx].w;
		height  = image->frames[idx].h;

		/*
		* get current screen state if we are to dispose of this frame by the 
		* previous state. The previous state is given by the current pixmap,
		* so we just create a new pixmap and copy the current one into it.
		* This is about the fastest method I can think of.
		*/
		if(image->frames[idx].dispose == XmIMAGE_DISPOSE_BY_PREVIOUS &&
			image->frames[idx].prev_state == None)
		{
			Pixmap prev_state;
			GC tmpGC;

			/* create pixmap that is to receive the image */
			prev_state = tka->CreatePixmap(tka->dpy, tka->win, width, height,
				XCCGetDepth(HTML_ATTR(xcc)));

			/* copy it */
			tmpGC = tka->CreateGC(tka->dpy, prev_state, 0, 0);
			tka->SetFunction(tka->dpy, tmpGC, tka->gc_func[GC_GXcopy]);
			tka->CopyArea(tka->dpy, image->pixmap, prev_state, tmpGC,
					fx, fy, width, height, 0, 0);

			/* and save it */
			image->frames[idx].prev_state = prev_state;

			/* free and destroy */
			tka->FreeGC(tka->dpy, tmpGC);
		}
		if(image->frames[idx].clip)
		{
			tka->SetClipOrigin(tka->dpy, gc, fx, fy);
			tka->SetClipMask(tka->dpy, gc, image->frames[idx].clip);
		}
		tka->CopyArea(tka->dpy, image->frames[idx].pixmap, image->pixmap,
				gc, 0, 0, width, height, fx, fy);

		/* reset gc */
		RESET_GC(gc);

		/* blit current state to screen */
		tka->CopyArea(tka->dpy, image->pixmap, tka->win, gc, 0, 0,
			image->width, image->height, xs, ys);
	}
nextframe:
	image->current_frame++;

	/* will get set again by TimerCB */
	image->options &= ~(IMG_FRAMEREFRESH);

	if(image->current_frame == image->nframes)
	{
		image->current_frame = 0;
		/*
		* Sigh, when an animation is running forever (loop_count == 0) and
		* some sucker is keeping XmHTML up forever, chances are that we *can*
		* exceed INT_MAX. Since some systems don't wrap their integers properly
		* when their value exceeds INT_MAX, we can't keep increasing the
		* current loop count forever since this *can* lead to a crash (which
		* is a potential security hole). To prevent this from happening, we
		* *only* increase current_loop when run this animation a limited
		* number of times.
		*/
		if(image->loop_count)
		{
			image->current_loop++;
			/*
			* If the current loop count matches the total loop count, depromote
			* the animation to a regular image so the next time the timer
			* callback is activated we will enter normal image processing. 
			*/
			if(image->current_loop == image->loop_count)
				image->options &= ~(IMG_ISANIM);
		}
	}
	/*
	* To prevent racing conditions, we must first remove an existing 
	* timeout proc before adding a new one.
	*/
	REMOVE_TIMEOUTPROC(image);

	image->proc_id = tka->AddTimeOut(image->context, 
		image->frames[idx].timeout, TimerCB, image);

	_XmHTMLDebug(16, ("paint.c: DrawFrame end\n"));
}

/*****
* Name: 		DrawRule
* Return Type: 	void
* Description: 	paints a horizontal rule.
* In: 
*	html:		XmHTMLWidget id;
*	data:		element data;
* Returns:
*	nothing.
* Note:
*	Rules that had their noshade attribute set are identiefied by having
*	a non-zero y_offset field in the data. We support a color attribute
*	in this case as well, so colored rules are possible. They are also
*	possible if a hr is in the proper context: one of the extensions
*	supported by XmHTML is a color attribute on the DIV tag.
*****/
static void
DrawRule(XmHTMLWidget html, XmHTMLObjectTableElement data)
{
	int dy;
	GC gc;
	int xs, ys;
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	/* 
	* recompute rule layout if we are auto-sizing.
	* Needs to be done since the formatted_width is only known once
	* the entire document has been laid out.
	*/
	if(HTML_ATTR(resize_width))
	{
		int x;
		int width = HTML_ATTR(work_width) - HTML_ATTR(margin_width);

		/* horizontal offset */
		x = HTML_ATTR(margin_width) + data->ident;

		/* See if we have an width specification */
		if(data->len != 0)
		{
			if(data->len < 0)	/* % spec */
				width *= (float)(-1*data->len/100.);
			else	/* pixel spec, cut if wider than available */
				width = (data->len > width ? width : data->len);
			/* alignment is only honored if there is a width spec */
			switch(data->halign)
			{
				case XmHALIGN_RIGHT:
					x = HTML_ATTR(margin_width) + HTML_ATTR(work_width) - width;
					break;
				case XmHALIGN_CENTER:
					x = HTML_ATTR(margin_width) + 
					(HTML_ATTR(work_width) - width - HTML_ATTR(margin_width))/2;
				default:	/* shutup compiler */
					break;
			}
		}
		/* Save updated position and width */
		data->x = x;
		data->width = width;
	}

	xs = data->x - HTML_ATTR(scroll_x);

	/* vertical offset */
	dy = 0;
	ys = data->y - HTML_ATTR(scroll_y);

	if(data->height)
	{
		if(data->y_offset)	/* noshade */
		{
			gc = HTML_ATTR(gc);
			tka->SetLineAttributes(tka->dpy, gc, 1,
				tka->line_style[GC_LINE_SOLID],
				tka->cap_style[GC_CAP_BUTT],
				tka->join_style[GC_JOIN_BEVEL]);
			tka->SetForeground(tka->dpy, gc, data->fg);
			tka->FillRectangle(tka->dpy, tka->win, gc, xs, ys + dy,
					data->width, data->height);
		}
		else
		{
			if(data->fg != HTML_ATTR(body_fg))
				XmHTMLTkaRecomputeShadowColors(html, data->fg);

			tka->DrawShadows(tka->dpy, tka->win, MGR_ATTR(top_shadow_GC),
				MGR_ATTR(bottom_shadow_GC), xs, ys + dy, data->width,
				data->height, 1, XmSHADOW_IN);

			if(data->fg != HTML_ATTR(body_fg))
				XmHTMLTkaRecomputeShadowColors(html, HTML_ATTR(body_bg));
		}
	}
	else
	{
		if(data->y_offset) /* noshade */
		{
			gc = HTML_ATTR(gc);
			tka->SetLineAttributes(tka->dpy, gc, 1,
				tka->line_style[GC_LINE_SOLID],
				tka->cap_style[GC_CAP_BUTT],
				tka->join_style[GC_JOIN_BEVEL]);
			tka->SetForeground(tka->dpy, gc, data->fg);
			tka->DrawLine(tka->dpy, tka->win, gc, xs, ys + dy,
					xs + data->width, ys + dy);
			tka->DrawLine(tka->dpy, tka->win, gc, xs, ys + dy + 1,
					xs + data->width, ys + dy + 1);
		}
		else
		{
			if(data->fg != HTML_ATTR(body_fg))
				XmHTMLTkaRecomputeShadowColors(html, data->fg);

			tka->DrawShadows(tka->dpy, tka->win, MGR_ATTR(top_shadow_GC),
				MGR_ATTR(bottom_shadow_GC), xs, ys + dy, data->width, 2,
				1, XmSHADOW_IN);

			if(data->fg != HTML_ATTR(body_fg))
				XmHTMLTkaRecomputeShadowColors(html, HTML_ATTR(body_bg));
		}
	}
}

static void
DrawBullet(XmHTMLWidget html, XmHTMLObjectTableElement data)
{
	GC gc = HTML_ATTR(gc);
	int ys, xs;
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	/* reset colors, an anchor might have been drawn before */
	tka->SetForeground(tka->dpy, gc, data->fg);
	tka->SetLineAttributes(tka->dpy, gc, 1,
			tka->line_style[GC_LINE_SOLID],
			tka->cap_style[GC_CAP_BUTT],
			tka->join_style[GC_JOIN_BEVEL]);

	xs = data->x - HTML_ATTR(scroll_x);
	ys = data->y - HTML_ATTR(scroll_y);

	switch(data->marker)
	{
		case XmMARKER_DISC:
			tka->FillArc(tka->dpy, tka->win, gc,
				xs - 2*data->width, ys - data->height, 
				data->width, data->width, 0, 23040);
			break;
		case XmMARKER_SQUARE:
			tka->DrawRectangle(tka->dpy, tka->win, gc,
				xs - 2*data->width, ys - data->height, 
				data->width, data->width);
			break;
		case XmMARKER_CIRCLE:
			tka->DrawArc(tka->dpy, tka->win, gc,
				xs - 2*data->width, ys - data->height, 
				data->width, data->width, 0, 23040);
			break;
		default: 
			/* Textual markers are right-aligned. */
			tka->DrawString(tka->dpy, tka->win, HTML_ATTR(default_font), gc,
				xs - data->width, ys, data->text, data->len);
			break;
	}
}

/*****
* Name:			DrawAnchorButton
* Return Type: 	void
* Description: 	render an anchor as a button.
* In: 
*	tka:		paint functions
*	x,y:		position of top-left origin;
*	width:		width of button;
*	height:		height of buttom;
*	top_sha..:  top shadow color to use;
*	bottom_..:	bottom shadow color to use;
* Returns:
*
*****/
static void
DrawAnchorButton(ToolkitAbstraction *tka, int x, int y, Dimension width,
	Dimension height, GC top_shadow_GC, GC bottom_shadow_GC)
{
	tka->DrawShadows(tka->dpy, tka->win, top_shadow_GC, bottom_shadow_GC,
		x, y, width, height, 1, XmSHADOW_OUT);
}

/*****
* Name:			DrawTable
* Return Type: 	XmHTMLObjectTableElement
* Description: 	render a table.
* In: 
*	html:		XmHTMLWidget id;
*	start:		starting object;
*	data_end:	ending object
* Returns:
*	last element of the table or NULL.
*****/
static XmHTMLObjectTableElement
DrawTable(XmHTMLWidget html, XmHTMLObjectTableElement start, 
	XmHTMLObjectTableElement data_end)
{
	XmHTMLTable *table;
	TableRow *row = NULL;
	TableCell *cell = NULL;
	int nrows, ncols, i, j;

	/* pick up table data */
	table = start->table;

	if(table == NULL)
		return(start);

	/*****
	* The first table in a stack of tables contains all data for all
	* table childs it contains. The first table child is the master
	* table itself. So when a table doesn't have a child table it *is*
	* a child table itself and thus we should add the left offset
	* to the initial horizontal position.
	*****/
	if(table->childs)
		table = &(table->childs[0]);

#if 0
	/* only render table border if we have to */
	if(table->props->framing != TFRAME_VOID)
		DrawTableBorder(html, table);
#endif
	nrows = table->nrows;
	ncols = table->ncols;

	for(i = 0; i < nrows; i++)
	{
		row = &(table->rows[i]);

		for(j = 0; j < row->ncells; j++)
		{
			cell = &(row->cells[j]);

			/* only draw something if it falls in the exposure area */
			if((HTML_ATTR(paint_y) > cell->owner->y + cell->owner->height ||
				HTML_ATTR(paint_height) < cell->owner->y) ||
				(HTML_ATTR(paint_x) > cell->owner->x + cell->owner->width ||
				HTML_ATTR(paint_width) < cell->owner->x))
				continue;

			/*****
			* Render the cell if it has a background color/image or when
			* we have to draw borders.
			*****/
			/* check if we have to draw a border */
			if(HTML_ATTR(body_image) ||
				cell->props->bg != HTML_ATTR(body_bg) ||
				cell->props->bg_image != NULL ||
				cell->props->ruling != TRULE_NONE)
				DrawCellFrame(html, cell);

			DrawCellContent(html, cell->start, cell->end);
		}
	}

	/* only render table border if we have to */
	if(table->props->framing != TFRAME_VOID)
		DrawTableBorder(html, table);

	/* sanity */
	if(table->end && data_end)
		return(table->end->y < data_end->y ? table->end->prev : data_end->prev);
	return(table->end ? table->end->prev : data_end);
}

/*****
* Name:			DrawCellContent
* Return Type: 	void
* Description: 	render the contents of a single cell
* In: 
*	html:		XmHTMLWidget id;
*	start:		starting object;
*	end:		ending object
* Returns:
*	nothing.
*****/
static void
DrawCellContent(XmHTMLWidget html, XmHTMLObjectTableElement start,
	XmHTMLObjectTableElement end)
{
	XmHTMLObjectTableElement temp;

	temp = start;

	_XmHTMLDebug(16, ("paint.c: DrawCellContent, table_start: x = %i, "
		"y = %i\n", start->x, start->y));

	while(temp && temp != end)
	{
		_XmHTMLDebug(16, ("paint.c: DrawCellContent, checking object %s\n",
			temp->object ? temp->object->element : "<dummy element>"));

		switch(temp->object_type)
		{
			case OBJ_TEXT:
			case OBJ_PRE_TEXT:
				/*
				* First check if this is an image. DrawImage will render
				* an image as an anchor if required.
				*/
				if(temp->text_data & TEXT_IMAGE)
					_XmHTMLDrawImage(html, temp, 0, False);
				else
				{
					/* form scrolling gets handled by formScroll in XmHTML.c */
					if(temp->text_data & TEXT_FORM)
						break;
					else
					{
						if(temp->text_data & TEXT_ANCHOR)
							DrawAnchor(html, temp);
						else
							DrawText(html, temp);
					}
				}
				break;
			case OBJ_BULLET:
				DrawBullet(html, temp);
				break;
			case OBJ_HRULE:
				DrawRule(html, temp);
				break;
			case OBJ_TABLE:
				/* nested tables, hehehe */
				(void)DrawTable(html, temp, end);
				break;
			case OBJ_TABLE_FRAME:
			case OBJ_IMG:
			case OBJ_APPLET:
			case OBJ_BLOCK:
			case OBJ_NONE:
				_XmHTMLDebug(16, ("paint.c: DrawTable, skipping undrawable "
					"object %s\n",
					temp->object ? temp->object->element : "<dummy element>"));
				break;
			default:
				_XmHTMLWarning(__WFUNC__(html, "DrawTable"), XMHTML_MSG_78);
		}
		temp = temp->next;
	}
	_XmHTMLDebug(16, ("paint.c: DrawCellContent End\n"));
}

/*****
* Name:			DrawCellFrame
* Return Type: 	void
* Description: 	renders a frame around a table cell. Optionally fills it
*				with a background color or image.
* In: 
*	html:		XmHTMLWidget id;
*	cell:		cell to be framed;
* Returns:
*	nothing.
*****/
static void
DrawCellFrame(XmHTMLWidget html, TableCell *cell)
{
	XmHTMLObjectTableElement data = cell->owner;
	int xs, ys, width, height;
	int rx, ry, rw, rh;		/* cell background region coordinates */
	Byte rule = DRAW_BOX;
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	/* which sides do we have to render? */
	switch(cell->props->ruling)
	{
		case TRULE_NONE:	/* no rules, only bg color/image will be done */
			rule = DRAW_NONE;
			break;
		case TRULE_GROUPS:	/* only horizontal rules */
		case TRULE_ROWS:	/* only horizontal rules */
			rule = DRAW_LEFT | DRAW_RIGHT;
			break;
		case TRULE_COLS:	/* only vertical rules */
			rule = DRAW_TOP | DRAW_BOTTOM;
			break;
		case TRULE_ALL:		/* all rules */
			break;
	}

	/* width & height of the box to be drawn */
	rw   = width  = data->width;
	rh   = height = data->height;

	/*****
	* Initial, absolute, positions.
	* Painting uses upper-left coordinates.
	******/
	rx = xs = data->x;
	ry = ys = data->y;

	/*****
	* Correct absolute positions for updating only the exposed region of
	* a cell's *background*
	* Not doing this would repaint the background of an entire cell while
	* it's contents are only redrawn for the exposed region.
	*****/
	if(rx < HTML_ATTR(paint_x))
	{
		/* origin too far left */
		rw -= (HTML_ATTR(paint_x) - rx);
		rx = HTML_ATTR(paint_x);
	}

	if(rx + rw > HTML_ATTR(paint_width))
	{
		/* width too far right */
		rw = HTML_ATTR(paint_width) - rx;
	}

	if(ry < HTML_ATTR(paint_y))
	{
		/* origin too high */
		rh -= (HTML_ATTR(paint_y) - ry);
		ry = HTML_ATTR(paint_y);
	}

	if(ry + rh > HTML_ATTR(paint_height))
	{
		/* height too low */
		rh = HTML_ATTR(paint_height) - ry;
	}

	if(rh <= 0 || rw <= 0)
		return;

	/*****
	* Translate absolute coordinates to relative ones by substracting
	* the region that has been scrolled.
	*****/
	xs -= HTML_ATTR(scroll_x);
	ys -= HTML_ATTR(scroll_y);
	rx -= HTML_ATTR(scroll_x);
	ry -= HTML_ATTR(scroll_y);

	/* Do we have a unique background color? */
	if(HTML_ATTR(body_image) || cell->owner->bg != HTML_ATTR(body_bg))
	{
		tka->SetForeground(tka->dpy, HTML_ATTR(gc), data->bg);
		tka->FillRectangle(tka->dpy, tka->win, HTML_ATTR(gc), rx, ry, rw, rh);
	}

	/* Do we have a background image? */
	if(cell->props->bg_image != NULL)
	{
		int tile_width, tile_height, x_dist, y_dist;
		int ntiles_x, ntiles_y, tsx, tsy;
		int x_offset, y_offset;
		XmHTMLImage *bg_image = cell->props->bg_image;

		tile_width  = bg_image->width;
		tile_height = bg_image->height;

		/* total distance covered so far */
		x_dist = HTML_ATTR(scroll_x) + rx;
		y_dist = HTML_ATTR(scroll_y) + ry;

		/* no of horizontal tiles */
		ntiles_x = (int)(x_dist/tile_width);
		ntiles_y = (int)(y_dist/tile_height);
		x_offset = x_dist - (ntiles_x * tile_width);
		y_offset = y_dist - (ntiles_y * tile_height);
		tsx = rx - x_offset;
		tsy = ry - y_offset;

		/* set gc values */
		tka->SetFillStyle(tka->dpy, HTML_ATTR(bg_gc),
				tka->fill_style[GC_FILL_TILED]);
		tka->SetTile(tka->dpy, HTML_ATTR(bg_gc), bg_image->pixmap);
		tka->SetTSOrigin(tka->dpy, HTML_ATTR(bg_gc),
				tsx - rx, tsy - ry);

		/* paint it. */
		tka->FillRectangle(tka->dpy, tka->win, HTML_ATTR(bg_gc), rx, ry,
				rw, rh);
	}

	/* no border drawing if we don't have to */
	if(cell->props->border == 0)
		return;

	/* draw an entire box and return */
	if(DRAW_BOX == (rule & DRAW_BOX))
	{
		tka->DrawShadows(tka->dpy, tka->win, MGR_ATTR(top_shadow_GC),
			MGR_ATTR(bottom_shadow_GC), xs, ys - 1, width, height,
			1, XmSHADOW_IN);
		return;
	}

	/* top border */
	if(rule & DRAW_TOP)
		tka->DrawShadows(tka->dpy, tka->win, MGR_ATTR(top_shadow_GC),
			MGR_ATTR(bottom_shadow_GC), xs, ys - 1, width, 2,
			1, XmSHADOW_OUT);

	/* left border */
	if(rule & DRAW_LEFT)
		tka->DrawShadows(tka->dpy, tka->win, MGR_ATTR(top_shadow_GC),
			MGR_ATTR(bottom_shadow_GC), xs, ys, 2, height,
			1, XmSHADOW_OUT);

	/* bottom border */
	if(rule & DRAW_BOTTOM)
		tka->DrawShadows(tka->dpy, tka->win, MGR_ATTR(top_shadow_GC),
			MGR_ATTR(bottom_shadow_GC), xs, ys + height - 1, width, 2,
			1, XmSHADOW_OUT);

	/* right border */
	if(rule & DRAW_RIGHT)
		tka->DrawShadows(tka->dpy, tka->win, MGR_ATTR(top_shadow_GC),
			MGR_ATTR(bottom_shadow_GC), xs + width, ys, 2, height,
			1, XmSHADOW_OUT);
}

/*****
* Name:			DrawTableBorder
* Return Type: 	void
* Description: 	renders a frame around a table.
* In: 
*	html:		XmHTMLWidget id;
*	table:		table for which a frame must be rendered;
* Returns:
*	nothing.
*****/
static void
DrawTableBorder(XmHTMLWidget html, XmHTMLTable *table)
{
	int xs, ys;
	XmHTMLObjectTableElement data = table->owner;
	int bwidth, width, height;
	Byte rule = DRAW_BOX;
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	bwidth = table->props->border;
	width  = data->width;
	height = data->height;

	/* horizontal offset, taking scrolling into account */
	xs = data->x - HTML_ATTR(scroll_x);

	/* vertical offset, taking scrolling into account */
	ys = data->y - HTML_ATTR(scroll_y);

	_XmHTMLDebug(16, ("Drawing table border: x = %i, y = %i, width = %i, "
		"height = %i\n", xs, ys, width, height));

	/* which sides do we have to render? */
	switch(table->props->framing)
	{
		case TFRAME_VOID:
			return;
			break;
		case TFRAME_ABOVE:
			rule = DRAW_TOP;
			break;
		case TFRAME_BELOW:
			rule = DRAW_BOTTOM;
			break;
		case TFRAME_LEFT:
			rule = DRAW_LEFT;
			break;
		case TFRAME_RIGHT:
			rule = DRAW_RIGHT;
			break;
		case TFRAME_HSIDES:
			rule = DRAW_LEFT|DRAW_RIGHT;
			break;
		case TFRAME_VSIDES:
			rule = DRAW_TOP|DRAW_BOTTOM;
			break;
		case TFRAME_BOX:
		case TFRAME_BORDER:
			break;
	}

	/* draw an entire box and return */
	if(DRAW_BOX == (rule & DRAW_BOX))
	{
		tka->DrawShadows(tka->dpy, tka->win, MGR_ATTR(top_shadow_GC),
			MGR_ATTR(bottom_shadow_GC), xs, ys - 1, width + 1, height + 1,
			bwidth, XmSHADOW_OUT);
		return;
	}

	/* top border */
	if(rule & DRAW_TOP)
		tka->DrawShadows(tka->dpy, tka->win, MGR_ATTR(top_shadow_GC),
			MGR_ATTR(bottom_shadow_GC), xs, ys, width, table->vmargin,
			bwidth, XmSHADOW_OUT);

	/* left border */
	if(rule & DRAW_LEFT)
		tka->DrawShadows(tka->dpy, tka->win, MGR_ATTR(top_shadow_GC),
			MGR_ATTR(bottom_shadow_GC), xs, ys, table->hmargin, height,
			bwidth, XmSHADOW_OUT);

	/* bottom border */
	if(rule & DRAW_BOTTOM)
		tka->DrawShadows(tka->dpy, tka->win, MGR_ATTR(top_shadow_GC),
			MGR_ATTR(bottom_shadow_GC), xs, ys + height, width, table->vmargin,
			bwidth, XmSHADOW_IN);

	/* right border */
	if(rule & DRAW_RIGHT)
		tka->DrawShadows(tka->dpy, tka->win, MGR_ATTR(top_shadow_GC),
			MGR_ATTR(bottom_shadow_GC), xs + width, ys, table->hmargin, height,
			bwidth, XmSHADOW_IN);
}

/********
**** Private exported paint functions
********/

/*****
* Name: 		_XmHTMLPaint
* Return Type: 	void
* Description: 	re-paints the given amount of data.
* In: 
*	w:			widget to paint text to
*	start:		start item
*	end:		end item
* Returns:
*	nothing.
*****/
void
_XmHTMLPaint(XmHTMLWidget html, XmHTMLObjectTable *start, 
	XmHTMLObjectTable *end)
{
	XmHTMLObjectTableElement temp;

	_XmHTMLDebug(16, ("paint.c: _XmHTMLPaint Start, paint_start: x = %i, "
		"y = %i, paint_end: x = %i, y = %i\n", start->x, start->y,
		 end ? end->x : -1, end ? end->y : -1));

	temp = start;

	while(temp && temp != end)
	{
		_XmHTMLDebug(16, ("paint.c: _XmHTMLPaint, painting object %s\n",
			temp->object ? temp->object->element : "<dummy element>"));

		switch(temp->object_type)
		{
			case OBJ_TEXT:
			case OBJ_PRE_TEXT:
				/*
				* First check if this is an image. DrawImage will render
				* an image as an anchor if required.
				*/
				if(temp->text_data & TEXT_IMAGE)
					_XmHTMLDrawImage(html, temp, 0, False);
				else
				{
					/* form scrolling gets handled by formScroll in XmHTML.c */
					if(temp->text_data & TEXT_FORM)
						break;
					else
					{
						if(temp->text_data & TEXT_ANCHOR)
							DrawAnchor(html, temp);
						else
							DrawText(html, temp);
					}
				}
				break;
			case OBJ_BULLET:
				DrawBullet(html, temp);
				break;
			case OBJ_HRULE:
				DrawRule(html, temp);
				break;
			case OBJ_IMG:
				_XmHTMLWarning(__WFUNC__(html, "_XmHTMLPaint"), XMHTML_MSG_89);
				break;
			case OBJ_TABLE:
			case OBJ_TABLE_FRAME:
				temp = DrawTable(html, temp, end);
				break;
			case OBJ_APPLET:
			case OBJ_BLOCK:
			case OBJ_NONE:
				break;
			default:
				_XmHTMLWarning(__WFUNC__(html, "_XmHTMLPaint"), XMHTML_MSG_78);
		}
		temp = temp->next;
	}
	_XmHTMLDebug(16, ("paint.c: _XmHTMLPaint End\n"));
	return;
}

/*****
* Name: 		_XmHTMLRestartAnimations
* Return Type: 	void
* Description: 	restarts all animations. Called by SetValues when the
*				value of the XmNfreezeAnimations resource switches from 
*				True to False.
* In: 
*	html:		XmHTMLWidget id
* Returns:
*	nothing.
*****/
void
_XmHTMLRestartAnimations(XmHTMLWidget html)
{
	XmHTMLImage *tmp;

	for(tmp = HTML_ATTR(images); tmp != NULL; tmp = tmp->next)
	{
		if(ImageIsAnim(tmp))
		{
			tmp->options |= IMG_FRAMEREFRESH;
			_XmHTMLDrawImage(html, tmp->owner, 0, False);
		}
	}
}

/*****
* Name: 		_XmHTMLDrawImage
* Return Type: 	void
* Description: 	image refresher.
* In: 
*	w:			XmHTMLWidget id
*	data:		Object data.
*	y_offset:	vertical offset for screen copying;
*	from_timerCB: true when called from the timeout proc.
* Returns:
*	nothing
* Note:
*	this is a funny routine: it does plain images as well as
*	animations. Animations with a loop count of zero will loop
*	forever. Other animations will loop their counts and when that
*	has been reached they are depromoted to regular images.
*	The only way to restore animations with a loop count to animations
*	again is to reload them (XmHTMLImageUpdate).
*****/
void
_XmHTMLDrawImage(XmHTMLWidget html, XmHTMLObjectTableElement data, int y_offset,
	Boolean from_timerCB)
{
	int xs, ys;
	XmHTMLImage *image;
	GC gc;
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	/* sanity check */
	if((image = data->words->image) == NULL)
		return;

	gc = (ImageIsProgressive(image) ? HTML_ATTR(plc_gc) : HTML_ATTR(gc));

	/* compute correct image offsets */
	xs = data->words->x - HTML_ATTR(scroll_x);
	ys = data->words->y - HTML_ATTR(scroll_y);

	_XmHTMLDebug(16, ("paint.c: DrawImage start, x = %i, y = %i\n",
		data->x, data->y));

	/*
	* animation frames should be repainted if the animation is somewhere
	* in the visible area.
	*/
	if(ImageFrameRefresh(image))
	{
		if(xs + image->width < 0 || xs > HTML_ATTR(work_width) ||
			ys + image->height < 0 || ys > HTML_ATTR(work_height))
		{
			_XmHTMLDebug(16, ("paint.c: DrawImage end, animation %s not in "
				"visible area.\n", image->url));
			REMOVE_TIMEOUTPROC(image);
			return;
		}
	}
	/*
	* Only do this when we are repainting this image as a result of an
	* exposure.
	*/
	if(!from_timerCB)
	{
		/* anchors are always repainted if they are visible */
		/* fix 03/25/97-01, kdh */
		if(data->text_data & TEXT_ANCHOR)
		{
			if(xs + image->width > 0 && xs < HTML_ATTR(work_width) &&
				ys + image->height > 0 && ys < HTML_ATTR(work_height))
				DrawImageAnchor(html, data);
		}
		else
		{
			/* 
			* When any of the two cases below is true, the image is not in the 
			* exposed screen area. Not doing this check would cause a visible 
			* flicker of the screen when scrolling: the entire image would be 
			* repainted even if it is not visible.
			*/
			if(HTML_ATTR(paint_y) > data->words->y + image->height ||
				HTML_ATTR(paint_height) < data->words->y)
			{
				_XmHTMLDebug(16, ("paint.c: DrawImage end, out of vertical "
					"range.\n"));
				return;
			}
			if(HTML_ATTR(paint_x) > data->words->x + image->width ||
				HTML_ATTR(paint_width) < data->words->x)
			{
				_XmHTMLDebug(16, ("paint.c: DrawImage end, out of horizontal "
					"range.\n"));
				return;
			}
		}
	}

	/*
	* If this is an animation, paint next frame or restore current
	* state when we are scrolling this animation on and off screen.
	*/
	if(ImageIsAnim(image))
		DrawFrame(html, image, xs, ys);
	else if(image->pixmap != None)
	{
		/* put in clipmask */
		if(image->clip)
		{
			/* set gc values */
			tka->SetClipOrigin(tka->dpy, gc, xs, ys);
			tka->SetClipMask(tka->dpy, gc, image->clip);
		}
		/* copy to screen */
		tka->DrawImage(html, image, gc, 0, y_offset, image->width,
			image->height, xs, ys + y_offset);
	}
	/* reset gc */
	RESET_GC(gc);

	/*****
	* Paint the alt text if images are disabled or when this image is
	* delayed.
	*****/
	if((!HTML_ATTR(images_enabled) ||
		(image->html_image && ImageInfoDelayed(image->html_image))) &&
		!(data->text_data & TEXT_ANCHOR))
	{
		tka->SetForeground(tka->dpy, gc, HTML_ATTR(body_fg));

		/* put text inside bounding rectangle */
		xs += image->width + 4;
		ys += image->height/2 + 4;
		tka->DrawString(tka->dpy, tka->win, data->words->font, gc, xs, ys,
			data->words->word, data->words->len);
	}

	/* check if we have to draw the imagemap bounding boxes */
	if(image->map_type == XmMAP_CLIENT && HTML_ATTR(imagemap_draw))
		_XmHTMLDrawImagemapSelection(html, image);
	
	_XmHTMLDebug(16, ("paint.c: DrawImage end\n"));
}

/*****
* Name: 		_XmHTMLPaintAnchorUnselected
* Return Type: 	void
* Description:  paints the currently active anchor in an unactivated state.
*				_XmHTMLPaint will do the proper rendering.
* In: 
*	w:			HTML widget of which to unset the current anchor
* Returns:
*	nothing.
*****/
void
_XmHTMLPaintAnchorUnSelected(XmHTMLWidget html)
{
	XmHTMLObjectTable *start, *end;

	start = HTML_ATTR(current_anchor);

	/* pick up the anchor end. An anchor ends when the raw worddata changes. */
	for(end = start; end != NULL && end->object == start->object; 
		end = end->next)
		end->anchor_state = ANCHOR_UNSELECTED;

	_XmHTMLFullDebug(1, ("XmHTML.c: _XmHTMLPaintAnchorUnselected, unselecting "
			"anchor: %s\n", start->anchor->href));

	/* paint it... */
	_XmHTMLPaint(html, start, end);

	/* ...and invalidate current selection */
	HTML_ATTR(current_anchor) = NULL;
}

/*****
* Name: 		_XmHTMLPaintAnchorSelected
* Return Type: 	void
* Description:  paints the current active in an activated state.
*				_XmHTMLPaint will do the proper rendering.
* In: 
*	html:		HTML widget of which to set the current anchor
* Returns:
*	nothing.
*****/
void
_XmHTMLPaintAnchorSelected(XmHTMLWidget html, XmHTMLWord *anchor)
{
	XmHTMLObjectTable *start, *end;

	/* save as the current active anchor */
	start = HTML_ATTR(current_anchor) = anchor->owner;

	start = anchor->owner;

	/* pick up anchor end. */
	for(end = start; end != NULL && end->object == start->object; 
		end = end->next)
		end->anchor_state = ANCHOR_SELECTED;

	_XmHTMLFullDebug(1, ("XmHTML.c: _XmHTMLPaintAnchorSelected, selecting "
		"anchor: %s\n", start->anchor->href));

	/* paint it */
	_XmHTMLPaint(html, start, end);
}

/*****
* Name: 		_XmHTMLPaintAnchorLeave
* Return Type: 	void
* Description:  remove anchor highlighting.
* In: 
*	html:		HTML widget of which to set the current anchor
* Returns:
*	nothing.
*****/
void
_XmHTMLPaintAnchorLeave(XmHTMLWidget html)
{
	XmHTMLObjectTable *start, *end;

	/* save as the current active anchor */
	start = HTML_ATTR(armed_anchor);

	/* pick up the anchor end. */
	for(end = start; end != NULL && end->object == start->object; 
		end = end->next)
		end->anchor_state = ANCHOR_UNSELECTED;

	_XmHTMLFullDebug(1, ("XmHTML.c: _XmHTMLPaintAnchorLeave, leaving "
		"anchor: %s\n", start->anchor->href));

	/* paint it */
	_XmHTMLPaint(html, start, end);

	HTML_ATTR(armed_anchor) = NULL;
}

/*****
* Name: 		_XmHTMLPaintAnchorEntry
* Return Type: 	void
* Description: 	paints a highlight on the given anchor.
* In: 
*	html:		XmHTMLWidget id;
*	anchor:		anchor to receive highlighting.
* Returns:
*	nothing.
*****/
void
_XmHTMLPaintAnchorEntry(XmHTMLWidget html, XmHTMLObjectTable *anchor)
{
	XmHTMLObjectTable *start, *end;

	/* save as the current active anchor */
	start = HTML_ATTR(armed_anchor) = anchor;

	/* pick up anchor end */
	for(end = start; end != NULL && end->object == start->object; 
		end = end->next)
		end->anchor_state = ANCHOR_INSELECT;

	_XmHTMLFullDebug(1, ("XmHTML.c: _XmHTMLPaintAnchorEntry, entering "
		"anchor: %s\n", start->anchor->href));

	/* paint it */
	_XmHTMLPaint(html, start, end);
}

