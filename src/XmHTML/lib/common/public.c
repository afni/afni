#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* public.c : XmHTML public routines that do not depend on X Intrinsics
*			 or Motif.
*
* This file Version	$Revision$
*
* Creation date:		Tue Apr 14 15:36:37 GMT+0100 1998
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
*
* Copyright (C) 1994-1998 by Ripley Software Development
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
* Revision 1.2  2012/03/01 17:56:31  ziad
* Cput
*
* Revision 1.1  2011/06/30 16:10:38  rwcox
* Cadd
*
* Revision 1.1  1998/04/27 06:54:17  newt
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

#include <X11/IntrinsicP.h>	/* Fast macros */
#include <Xm/XmP.h>			/* Private motif funcs. */
#include <Xm/DrawP.h>
#include <Xm/XmStrDefs.h>	/* For motif XmN macros */

#ifdef HAVE_LIBJPEG
#include <jpeglib.h>
#endif

#ifdef HAVE_LIBZ
#include <zlib.h>
#endif

#ifdef HAVE_LIBPNG
#include <png.h>
#endif

/* Our private header files */
#include "toolkit.h"
#include XmHTMLPrivateHeader

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/

/*** Private Variable Declarations ***/

/**********
***** Public Anhor functions
**********/

/*****
* Name: 		XmHTMLAnchorGetId
* Return Type: 	int
* Description: 	returns the internal id of an anchor
* In:
*	w:			XmHTMLWidget
*	anchor:		anchor to locate
* Returns:
*	the id upon success, -1 if not found.
*****/
int
XmHTMLAnchorGetId(Widget w, String anchor)
{
	XmHTMLObjectTableElement anchor_data = NULL;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "AnchorGetId");
		return(-1);
	}

	if((anchor_data = _XmHTMLGetAnchorByName((XmHTMLWidget)w, anchor)) != NULL)
		return(anchor_data->id);
	else /* not found */
		return(-1);
}

/*****
* Name: 		XmHTMLAnchorScrollToId
* Return Type: 	void
* Description: 	moves the text with the current anchor on top.
* In:
*	w:			XmHTMLWidget
*	anchor_id:	internal anchor id to scroll to.
* Returns:
*	nothing.
*****/
void
XmHTMLAnchorScrollToId(Widget w, int anchor_id)
{
	XmHTMLWidget html;
	XmHTMLObjectTableElement anchor_data = NULL;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "XmHTMLAnchorScrollToId");
		return;
	}

	html = (XmHTMLWidget)w;

	/* only scroll when we have a vertical scrollbar */
	/* fix 10/22/97-01, kdh */
	if((anchor_data = _XmHTMLGetAnchorByValue(html, anchor_id)) != NULL &&
		HTML_ATTR(needs_vsb))
	{
		int value;

		_XmHTMLDebug(1, ("XmHTML.c: XmHTMLAnchorScrollToId, "
			"calling _XmHTMLMoveToPos\n"));

		value = anchor_data->y - anchor_data->height;

		/* fix 01/30/97-04, kdh */
		_XmHTMLAdjustVerticalScrollValue(HTML_ATTR(vsb), &value);

		_XmHTMLMoveToPos(HTML_ATTR(vsb), html, value);
	}
}

/*****
* Name: 		XmHTMLAnchorScrollToName
* Return Type: 	void
* Description: 	moves the text with the current anchor on top.
* In:
*	w:			XmHTMLWidget
*	anchor:		anchor to scroll to.
* Returns:
*	nothing.
*****/
void
XmHTMLAnchorScrollToName(Widget w, String anchor)
{
	XmHTMLWidget html;
	XmHTMLObjectTableElement anchor_data = NULL;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "AnchorScrollToName");
		return;
	}

	html = (XmHTMLWidget)w;

	/* only scroll when we have a vertical scrollbar */
	/* fix 10/22/97-01, kdh */
	if((anchor_data = _XmHTMLGetAnchorByName(html, anchor)) != NULL &&
		HTML_ATTR(needs_vsb))
	{
		int value;

		_XmHTMLDebug(1, ("XmHTML.c: XmHTMLAnchorScrollToName, "
			"calling _XmHTMLMoveToPos\n"));

		value = anchor_data->y - anchor_data->height;

		/* fix 01/30/97-04, kdh */
		_XmHTMLAdjustVerticalScrollValue(HTML_ATTR(vsb), &value);

		_XmHTMLMoveToPos(HTML_ATTR(vsb), html, value);
	}
	return;
}

/*****
* Name:			XmHTMLAnchorReEvalAnchor
* Return Type: 	void
* Description: 	checks an HTML instance against a current href name
*               alters any matching anchors to 'visited' and causes refesh
* In:
*	w:			html widget
*	href:		current anchors reference
*	visited:	if True, href will be rendered as visited, if False, anchor
*				will be rendered as not visited.
* Returns:
*	nothing.
* Note:
*	Kindly provided by Randy Wilson (wilson@allsparc.ct.tsc.com)
*****/
void
XmHTMLAnchorReEval(Widget w, String href, Boolean visited)
{
	int i;
	Byte line_style;
	XmHTMLWidget html;
	XmHTMLAnchor *tmp;
	int refresh_html;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "XmHTMLAnchorReEval");
		return;
	}
	html = (XmHTMLWidget)w;

	if(!href || *href == '\0')
		return;

	refresh_html = 0;

	/* check all anchors pointing to the same name */
	for(i = 0 ; i < html->html.anchor_words ; i++)
	{
		/* check if this anchor matches */
		if(html->html.anchors[i].owner)
		{
			/* need to verify that the html is fully created */
			tmp = html->html.anchors[i].owner->anchor;

			/* set new state */
			if(!(strcasecmp(tmp->href, href)) && tmp->visited != visited)
			{
				int j;
				refresh_html = 1;

				/* update anchor state */
				tmp->visited = visited;

				/* a match, set the foreground & underlining of master block */
				if(visited)
				{
					/* foreground */
					html->html.anchors[i].owner->fg =
						HTML_ATTR(anchor_visited_fg);
					/* change underline style as well! */
					line_style = HTML_ATTR(anchor_visited_line);
				}
				else
				{
					/* foreground */
					if(tmp->target)
					{
						html->html.anchors[i].owner->fg =
							HTML_ATTR(anchor_target_fg);
						line_style = HTML_ATTR(anchor_target_line);
					}
					else
					{
						html->html.anchors[i].owner->fg =
							HTML_ATTR(anchor_fg);
						line_style = HTML_ATTR(anchor_line);
					}
				}
				/* check strike through flag */
				if(html->html.anchors[i].self->line_data & LINE_STRIKE)
					line_style |= LINE_STRIKE;

				/* update all words for this anchor */
				for(j = 0; j < html->html.anchors[i].owner->n_words; j++)
				{
					html->html.anchors[i].owner->words[j].line_data=line_style;
				}
			}
		}
		/* skip remaining anchor words of the master block */
		while(i < html->html.anchor_words-1 &&
			html->html.anchors[i].owner == html->html.anchors[i+1].owner)
			i++;
	}
	if(refresh_html)
	{
		/* refresh the display so the changes take immediate effect */
		ToolkitAbstraction *tka = HTML_ATTR(tka);
		tka->ClearArea(tka->dpy, tka->win, 0, 0,
				CORE_ATTR(width), CORE_ATTR(height), False);
	}
}

/*****
* Name:			XmHTMLAnchorVisibleById
* Return Type: 	Boolean
* Description: 	figure out if the given anchor is in the visible area.
* In:
*	w:			XmHTMLWidget id;
*	anchor_id:	ID of anchor for which to test visibility
* Returns:
*	True when anchor is visible, false if not.
*****/
Boolean
XmHTMLAnchorVisibleById(Widget w, int anchor_id)
{
	XmHTMLWidget html;
	XmHTMLObjectTableElement anchor_data = NULL;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "XmHTMLAnchorVisibleById");
		return(False);
	}

	html = (XmHTMLWidget)w;

	/* always visible if we don't have a vertical scrollbar */
	if(!HTML_ATTR(needs_vsb))
		return(True);

	if((anchor_data = _XmHTMLGetAnchorByValue(html, anchor_id)) != NULL)
	{
		if(anchor_data->y > HTML_ATTR(scroll_y) &&
			anchor_data->y < HTML_ATTR(scroll_y) + HTML_ATTR(work_height))
			return(True);
	}
	return(False);
}

/*****
* Name:			XmHTMLAnchorVisibleByName
* Return Type: 	Boolean
* Description: 	figure out if the given anchor is in the visible area.
* In:
*	w:			XmHTMLWidget id;
*	anchor:		NAME of anchor for which to test visibility
* Returns:
*	True when anchor is visible, false if not.
*****/
Boolean
XmHTMLAnchorVisibleByName(Widget w, String anchor)
{
	XmHTMLWidget html;
	XmHTMLObjectTableElement anchor_data = NULL;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "XmHTMLAnchorVisibleByName");
		return(False);
	}

	html = (XmHTMLWidget)w;

	/* always visible if we don't have a vertical scrollbar */
	if(!HTML_ATTR(needs_vsb))
		return(True);

	if((anchor_data = _XmHTMLGetAnchorByName(html, anchor)) != NULL)
	{
		if(anchor_data->y > HTML_ATTR(scroll_y) &&
			anchor_data->y < HTML_ATTR(scroll_y) + HTML_ATTR(work_height))
			return(True);
	}
	return(False);
}

/**********
***** Public Text Functions
**********/

/*****
* Name: 		XmHTMLTextScrollToLine
* Return Type: 	void
* Description: 	scrolls the widget to the given line number.
* In:
*	w:			widget to scroll
*	line:		line number to scroll to.
* Returns:
*	nothing.
*****/
void
XmHTMLTextScrollToLine(Widget w, int line)
{
	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "AnchorScrollToLine");
		return;
	}

	if(line == ((XmHTMLWidget)w)->html.top_line)
		return;

	/* scroll to the requested line */
	_XmHTMLScrollToLine((XmHTMLWidget)w, line);
}

/*****
* Name: 		XmHTMLTextGetSource
* Return Type: 	String
* Description: 	return a POINTER to the original, unmodified document.
* In:
*	w:			XmHTMLWidget in question
* Returns:
*	a *pointer* to the original text, or NULL when w isn't a subclass of XmHTML
*	or there wasn't a current document.
*****/
String
XmHTMLTextGetSource(Widget w)
{
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "TextGetSource");
		return(NULL);
	}

	return(((XmHTMLWidget)w)->html.source);
}

/*****
* Name: 		XmHTMLTextGetString
* Return Type: 	String
* Description: 	composes a text buffer consisting of the parser output.
*				This return buffer is not necessarely equal to the original
*				document as the document verification and repair routines
*				are capable of modifying the original rather heavily.
* In:
*	w:			XmHTMLWidget in question
* Returns:
*	An allocated buffer containing the composed text upon success, NULL on
*	failure.
* Note:
*	The return value from this function must be freed by the caller.
*	Typical use of this function is to set this buffer into the widget when
*	the parser failed to verify the document as it might well be that a next
*	parser pass on the original document does produce a HTML3.2 conforming
*	and verified document.
*****/
String
XmHTMLTextGetString(Widget w)
{
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "TextGetString");
		return(NULL);
	}

	return(_XmHTMLTextGetString(((XmHTMLWidget)w)->html.elements));
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
documentLoadNormal(XmHTMLWidget html, String text, size_t len)
{
	Boolean had_hsb, had_vsb;
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	_XmHTMLDebug(1, ("XmHTML.c: documentLoadNormal, start\n"));

	/* almost impossible */
	if(HTML_ATTR(value) == text)
		return;

	/* check if the new value is different from the current source */
	if(text && HTML_ATTR(source) && len &&
		!(strncmp(HTML_ATTR(source), text, len)))
		return;

	/* Check for an onUnLoad event. Process it if present */
	if(HTML_ATTR(event_mask) & EVENT_UNLOAD)
		_XmHTMLEventProcess(html, NULL, HTML_ATTR(body_events)->onUnload);

	/* unset cursor if we've got one */
	if(HTML_ATTR(current_anchor) != NULL)
		tka->UndefineCursor(tka->dpy, tka->win);

	/* check the current state of the scrollbars */
	had_hsb = tka->IsManaged(HTML_ATTR(hsb));
	had_vsb = tka->IsManaged(HTML_ATTR(vsb));

	/* First kill any outstanding PLC's */
	_XmHTMLKillPLCCycler(html);

	/* release event database */
	_XmHTMLEventFreeDatabase(html, html);

	/* release body events */
	if(ATTR_HTML(html,body_events) != NULL)
	{
		free(ATTR_HTML(html, body_events));
	}
	ATTR_HTML(html, body_events) = (AllEvents*)NULL;
	ATTR_HTML(html, event_mask) = 0L;

	/* now destroy any forms */
	_XmHTMLFreeForm(html, HTML_ATTR(form_data));
	HTML_ATTR(form_data) = (XmHTMLFormData*)NULL;

	/*****
	* Destroy any external object data inside frames before new
	* ones are created.
	*****/
#ifdef HAVE_EXTERNAL_OBJECTS
	_XmHTMLFrameDestroyExternalObjects(html);
	_XmHTMLDestroyExternalObjects(html);
#endif

	/* clear the current display area. Prevents color flashing etc. */
	if(HTML_ATTR(gc) != NULL)
	{
		tka->ClearArea(tka->dpy, tka->win, 0, 0,
			CORE_ATTR(width), CORE_ATTR(height), False);
	}

	/* clear current source */
	if(HTML_ATTR(source))
	{
		free(HTML_ATTR(source));
		HTML_ATTR(source) = NULL;
		HTML_ATTR(value)  = NULL;
	}

	/* set new source text */
	if(text && len)
	{
		/* strndup returns a NULL terminated string */
		HTML_ATTR(source) = my_strndup(text, len);
		HTML_ATTR(value) = HTML_ATTR(source);
	}

	/* destroy any existing frames */
	if(HTML_ATTR(nframes))
		_XmHTMLDestroyFrames(html, HTML_ATTR(nframes));

	/* free all non-persistent resources and images */
	_XmHTMLFreeExpendableResources(html, True);

	/* reset some important vars */
	_XmHTMLReset(html, True);

	/*****
	* Create XCC, it's probably been destroyed by now (unless a fixed
	* palette is being used.
	*****/
	_XmHTMLCheckXCC(html);

	/* input is always complete */
	HTML_ATTR(input_complete) = True;

	/* Parse the raw HTML text */
	HTML_ATTR(elements) = _XmHTMLparseHTML(html, HTML_ATTR(elements),
		HTML_ATTR(source), html);

	/* Trigger link callback */
	if(HTML_ATTR(link_callback))
		_XmHTMLLinkCallback(html);

	/* reset topline */
	HTML_ATTR(top_line) = 0;

	/* check for frames */
	HTML_ATTR(nframes) = _XmHTMLCheckForFrames(html, HTML_ATTR(elements));

	/* set appropriate bgcolor and get new values for top, bottom & highlight */
	XmHTMLTkaRecomputeColors(html, HTML_ATTR(body_bg));

	/* create frames */
	if(!_XmHTMLCreateFrames(NULL, html))
	{
		HTML_ATTR(frames) = NULL;
		HTML_ATTR(nframes) = 0;
		/* keep current frame setting */
	}

	/* do initial markup */
	_XmHTMLformatObjects(html, html);

	/* check for delayed external imagemaps */
	_XmHTMLCheckImagemaps(html);

	/* compute new screen layout */
	_XmHTMLLayout(html);

	/* and clear the display, causing an Expose event */
	if(HTML_ATTR(gc) != NULL)
		_XmHTMLClearArea(html, 0, 0, CORE_ATTR(width), CORE_ATTR(height));

	/*****
	* Check for onLoad events: this should take place after the area
	* has been cleared so that the widget's own callbacks are activated
	* first. Otherwise expose events may be generated and cause redisplay
	* with an inconsistent widget context...
	* No need to check the return value: modifying the document content
	* during onLoad processing is forbidden (EventProcess exits if it
	* does happen).
	*****/
	if(ATTR_HTML(html, event_mask) & EVENT_LOAD)
		_XmHTMLEventProcess(html, NULL, HTML_ATTR(body_events)->onLoad);

	/* and start up the PLC cycler */
	HTML_ATTR(plc_suspended) = False;
	_XmHTMLPLCCycler((XtPointer)html, None);
}

#ifdef CANDO_PROGRESSIVE_LOAD
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
documentLoadProgressive(XmHTMLWidget w, String text, size_t len)
{
	return;
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
documentLoadIncremental(XmHTMLWidget w, String text, size_t len)
{
	return;
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
documentFlush(XmHTMLWidget w, String text, size_t len)
{
	return;
}
#endif

/*****
* Name: 		XmHTMLTextSetString
* Return Type: 	void
* Description: 	sets the given text into the given HTML widget
* In:
*	w:			XmHTMLWidget in question
*	value:		text to set
* Returns:
*	clears any previous text and sets the new text.
*****/
void
XmHTMLTextSetString(Widget w, String text)
{
	XmHTMLWidget html;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "TextSetString");
		return;
	}

	/* widget ptr */
	html = (XmHTMLWidget)w;

#ifdef CANDO_PROGRESSIVE_LOAD
	if(HTML_ATTR(load_type) == XmLOAD_NORMAL)
	{
		/* test required since strlen(NULL) doesn't exactly work... */
		documentLoadNormal(html, text, text ? strlen(text) : 0);
	}
	else
		_XmHTMLWarning(__WFUNC__(w, "XmHTMLTextSetString"),
			"XmHTMLTextSetString can not be used for progressive document "
			"loading.\n     Use XmHTMLTextSetStringWithLength instead.");
#else
	/* test required since strlen(NULL) doesn't exactly work... */
	documentLoadNormal(html, text, text ? strlen(text) : 0);
#endif
}

/*****
* Name: 		XmHTMLTextSetStringWithLength
* Return Type: 	void
* Description: 	sets the given text into the given HTML widget
* In:
*	w:			XmHTMLWidget in question
*	value:		text to set. Doesn't have to be NULL terminated
*	len:		size of input string.
* Returns:
*	depending on current loading type, loads an entire document,
*	continue where we left of, append new text and then continue,
*	suspend or abort document load.
*****/
void
XmHTMLTextSetStringWithLength(Widget w, String text, size_t len)
{
	XmHTMLWidget html;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "TextSetStringWithLength");
		return;
	}

	/* widget ptr */
	html = (XmHTMLWidget)w;

#ifdef CANDO_PROGRESSIVE_LOAD
	switch(HTML_ATTR(load_type))
	{
		case XmLOAD_NORMAL:
			documentLoadNormal((XmHTMLWidget)w, text, len);
			break;
		case XmLOAD_PROGRESSIVE:
			documentLoadProgressive((XmHTMLWidget)w, text, len);
		case XmLOAD_INCREMENTAL:
			documentLoadIncremental((XmHTMLWidget)w, text, len);
		case XmLOAD_ABORT:
			documentFlush((XmHTMLWidget)w, text, len);
		case XmLOAD_SUSPEND:
			break;
	}
#else
	documentLoadNormal((XmHTMLWidget)w, text, len);
#endif
}

#ifdef CANDO_PROGRESSIVE_LOAD
/*****
* Name:
* Return Type:
* Description:
* In:
*
* Returns:
*
*****/
Boolean
XmHTMLTextUpdate(Widget w, String text, size_t len)
{
	XmHTMLWidget html;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "XmHTMLTextUpdate");
		return(False);
	}

	/* widget ptr */
	html = (XmHTMLWidget)w;

	if(HTML_ATTR(load_type) != XmLOAD_PROGRESSIVE)
		return(False);

	documentLoadProgressive((XmHTMLWidget)w, text, len);
	return(True);
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
Boolean
XmHTMLTextAppend(Widget w, String text, size_t len)
{
	XmHTMLWidget html;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "XmHTMLTextAppend");
		return(False);
	}

	/* widget ptr */
	html = (XmHTMLWidget)w;

	if(HTML_ATTR(load_type) != XmLOAD_INCREMENTAL)
		return(False);

	documentLoadIncremental((XmHTMLWidget)w, text, len);
	return(True);
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
Boolean
XmHTMLTextFlush(Widget w, String text, size_t len)
{
	XmHTMLWidget html;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "XmHTMLTextFlush");
		return(False);
	}

	/* widget ptr */
	html = (XmHTMLWidget)w;

	switch(HTML_ATTR(load_type))
	{
		case XmLOAD_PROGRESSIVE:
			documentLoadProgressive((XmHTMLWidget)w, text, len);
			documentFlush((XmHTMLWidget)w, NULL, 0);
			return(True);
		case XmLOAD_INCREMENTAL:
			documentLoadIncremental((XmHTMLWidget)w, text, len);
			documentFlush((XmHTMLWidget)w, NULL, 0);
			return(True);
		case XmLOAD_NORMAL:
		case XmLOAD_ABORT:
		case XmLOAD_SUSPEND:
			return(False);
	}
	return(False);	/* not reached but keeps compiler happy */
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
void
XmHTMLTextSetLoadType(Widget w, XmHTMLLoadType load_type)
{
	XmHTMLWidget html;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "XmHTMLTextFlush");
		return;
	}
	/* widget ptr */
	html = (XmHTMLWidget)w;

	HTML_ATTR(load_type) = load_type;
}
#endif /* CANDO_PROGRESSIVE_LOAD */

/*****
* Name:			XmHTMLTextGetFormatted
* Return Type: 	String
* Description: 	returns a formatted copy of the current document.
* In:
*	w:			XmHTMLWidget id;
*	papertype:	type of paper to use (any of the XmHTMLTEXT_PAPERSIZE enums);
*	papersize:	size of paper for custom stuff, or default overrides;
*	type:		type of output wanted, plain, formatted or PS;
*	PSoptions:	options to use when creating postscript output.
* Returns:
*	a string which needs to be freed by the caller.
*****/
String
XmHTMLTextGetFormatted(Widget w, unsigned char papertype,
	XmHTMLPaperSize *paperdef, unsigned char type, unsigned long PSoptions)
{
	XmHTMLWidget html;
	XmHTMLPaperSize *pdef, pbase;
	String ret_val = NULL;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "TextGetFormatted");
		return(NULL);
	}

	/* custom papersize requires a paper definition. */
	if(papertype == XmHTMLTEXT_PAPERSIZE_CUSTOM && paperdef == NULL)
	{
		_XmHTMLWarning(__WFUNC__(w, "XmHTMLTextGetFormatted"), XMHTML_MSG_23);
		return(NULL);
	}

	/* widget ptr */
	html = (XmHTMLWidget)w;

	/*****
	* get appropriate papersize definitions if not given.
	*****/
	if(papertype != XmHTMLTEXT_PAPERSIZE_CUSTOM && paperdef == NULL)
	{
		/* formatting routines use point size */
		if(papertype == XmHTMLTEXT_PAPERSIZE_A4)
		{
			pbase.unit_type     = XmHTML_POINT;
			pbase.paper_type    = XmHTMLTEXT_PAPERSIZE_A4;
			pbase.width         = 597;	/* 210mm */
			pbase.height        = 845;	/* 297mm */
			pbase.left_margin   = 23;	/* 10mm  */
			pbase.right_margin  = 23;
			pbase.top_margin    = 23;
			pbase.bottom_margin = 23;
		}
		else 	/* XmHTMLTEXT_PAPERSIZE_LETTER */
		{
			pbase.unit_type     = XmHTML_POINT;
			pbase.paper_type    = XmHTMLTEXT_PAPERSIZE_LETTER;
			pbase.width         = 614;	/* 8.5in */
			pbase.height        = 795;	/* 11in  */
			pbase.left_margin   = 65;	/* 0.9in */
			pbase.right_margin  = 65;
			pbase.top_margin    = 65;
			pbase.bottom_margin = 51;	/* 0.7in */
		}
		/* convert to correct output type */
		pdef = _XmHTMLTextCheckAndConvertPaperDef(html, &pbase, type);
	}
	else	/* check validity of paper definition and convert to correct type */
		pdef = _XmHTMLTextCheckAndConvertPaperDef(html, paperdef, type);

	if(pdef == NULL)
		return(NULL);

	switch(type)
	{
		case XmHTMLTEXT_PLAIN:
			ret_val = _XmHTMLTextGetPlain(html, pdef, HTML_ATTR(formatted),
				NULL, 0);
			break;
		case XmHTMLTEXT_FORMATTED:
			ret_val = _XmHTMLTextGetFormatted(html, pdef, HTML_ATTR(formatted),
				NULL, 0);
			break;
		case XmHTMLTEXT_POSTSCRIPT:
			ret_val = _XmHTMLTextGetPS(html, pdef, HTML_ATTR(formatted),
				NULL, PSoptions);
			break;
		default:
			_XmHTMLWarning(__WFUNC__(w, "XmHTMLTextGetFormatted"),
				XMHTML_MSG_24);
	}
	/* no longer needed */
	free(pdef);

	return(ret_val);
}

/**********
***** Public Image Functions
**********/

/*****
* Name: 		XmHTMLImageGetInfoSize
* Return Type: 	int
* Description: 	returns the size of the given XmImageInfo structure.
* In:
*	info:		ptr to a XmImageInfo structure;
* Returns:
*	size of the given XmImageInfo structure.
* Note:
*	This function is used both by us and the caching routines.
*****/
int
XmHTMLImageGetImageInfoSize(XmImageInfo *info)
{
	int size = 0;
	XmImageInfo *frame = info;

	while(frame != NULL)
	{
		size += sizeof(XmImageInfo);
		size += frame->width*frame->height;		/* raw image data */

		/* clipmask size. The clipmask is a bitmap of depth 1 */
		if(frame->clip)
		{
			int clipsize;
			clipsize = frame->width;
			/* make it byte-aligned */
			while((clipsize % 8))
				clipsize++;
			/* this many bytes on a row */
			clipsize /= 8;
			/* and this many rows */
			clipsize *= frame->height;
			size += clipsize;
		}
		/* reds, greens and blues */
		size += 3*frame->ncolors*sizeof(Dimension);
		frame = frame->frame;	/* next frame of this image (if any) */
	}
	return(size);
}

/*****
* Name: 		XmHTMLImageFreeAllImages
* Return Type: 	void
* Description: 	releases all allocated images and associated structures
* In:
*	html:		XmHTMLWidget for which to free all images
* Returns:
*	nothing
*****/
void
XmHTMLImageFreeAllImages(Widget w)
{
	XmHTMLImage *image, *image_list;
	XmHTMLWidget html;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "ImageFreeAllImages");
		return;
	}

	html = (XmHTMLWidget)w;
 	image_list = html->html.images;

	while(image_list != NULL)
	{
		image = image_list->next;
		_XmHTMLFreeImage(html, image_list);
		image_list = NULL;
		image_list = image;
	}
	html->html.images = NULL;

	/* alpha channel stuff */
	if(html->html.alpha_buffer)
	{
		if(html->html.alpha_buffer->ncolors)
			free(html->html.alpha_buffer->bg_cmap);
		free(html->html.alpha_buffer);
	}
	html->html.alpha_buffer = (AlphaPtr)NULL;

	/* only release XCC when we aren't using a fixed palette */
	if(html->html.map_to_palette == XmDISABLED)
	{
		XCCFree(html->html.xcc);
		html->html.xcc = (XCC)NULL;
	}
}

/*****
* Name: 		XmHTMLImageFreeImageInfo
* Return Type: 	void
* Description: 	free the given XmImageInfo structure
* In:
*	info:		image to free
* Returns:
*	nothing
*****/
void
XmHTMLImageFreeImageInfo(Widget w, XmImageInfo *info)
{
	static String func = "XmHTMLImageFreeImageInfo";

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, func);
		return;
	}

	/* sanity check */
	if(info == NULL)
	{
		_XmHTMLWarning(__WFUNC__(NULL, func), XMHTML_MSG_21, "NULL", func);
		return;
	}

	_XmHTMLFreeImageInfo((XmHTMLWidget)w, info, True);
}

/*****
* Name: 		XmHTMLImageAddImageMap
* Return Type: 	void
* Description: 	add the given imagemap to a HTML widget
* In:
*	w:			widget
*	image_map:	raw html data containing the imagemap to parse.
* Returns:
*	nothing
*****/
void
XmHTMLImageAddImageMap(Widget w, String image_map)
{
	XmHTMLWidget html;
	XmHTMLObject *parsed_map, *temp;
	XmHTMLImageMap *imageMap = NULL;
	static String func = "XmHTMLImageAddImageMap";

	/* sanity check */
	if(!w || !XmIsHTML(w) || image_map == NULL)
	{
		if(image_map == NULL)
			_XmHTMLWarning(__WFUNC__(w, func),
				XMHTML_MSG_21, "NULL", func);
		else
			_XmHTMLBadParent(w, func);
		return;
	}

	html = (XmHTMLWidget)w;

	/* parse the imagemap */
	if((parsed_map = _XmHTMLparseHTML(html, NULL, image_map, NULL)) == NULL)
		return;

	for(temp = parsed_map; temp != NULL; temp = temp->next)
	{
		switch(temp->id)
		{
			case HT_MAP:
				if(temp->is_end)
				{
					_XmHTMLStoreImagemap(html, imageMap);
					imageMap = NULL;
				}
				else
				{
					String chPtr;

					chPtr = _XmHTMLTagGetValue(temp->attributes, "name");
					if(chPtr != NULL)
					{
						imageMap = _XmHTMLCreateImagemap(chPtr);
						free(chPtr);
					}
					else
						_XmHTMLWarning(__WFUNC__(w, func),
							XMHTML_MSG_76, temp->line);
				}
				break;

			case HT_AREA:
				if(imageMap)
					_XmHTMLAddAreaToMap(html, imageMap, temp);
				else
					_XmHTMLWarning(__WFUNC__(w, func),
						XMHTML_MSG_48, html_tokens[HT_AREA],
						html_tokens[HT_MAP], temp->line);
				break;
			default:
				break;
		}
	}
	/* free the parsed imagemap data */
	(void)_XmHTMLparseHTML(html, parsed_map, NULL, NULL);
}

/*****
* Name: 		XmHTMLImageGetType
* Return Type: 	int
* Description: 	determines the type of a given image
* In:
*	file:		name of image file to check
* Returns:
*	the image type if supported, IMAGE_UNKNOWN otherwise.
*****/
unsigned char
XmHTMLImageGetType(String file, unsigned char *buf, int size)
{
	ImageBuffer data, *dp = NULL;
	Byte ret_val = IMAGE_UNKNOWN;

	if(!file)
		return(IMAGE_ERROR);

	if(size == 0 || buf == NULL)
	{
		if((dp = _XmHTMLImageFileToBuffer(file)) == NULL)
			return(IMAGE_ERROR);
	}
	else
	{
		if(buf != NULL && size != 0)
		{
			data.file = file;
			data.buffer = (Byte*)buf;
			data.size = (size_t)size;
			data.next = 0;
			data.may_free = False;
			dp = &data;
		}
		else
			return(IMAGE_ERROR);
	}

	ret_val = _XmHTMLGetImageType(dp);

	FreeImageBuffer(dp);

	return(ret_val);
}

/*****
* Name:			XmHTMLImageJPEGSupported
* Return Type:	Boolean
* Description:	check if JPEG support is available (this is a compile time
*				option).
* In:
*	nothing.
* Returns:
*	True if support is available, False if not.
*****/
Boolean
XmHTMLImageJPEGSupported(void)
{
#ifdef HAVE_LIBJPEG
	return(True);
#else
	return(False);
#endif
}

/*****
* Name:			XmHTMLImagePNGSupported
* Return Type:	Boolean
* Description:	check if PNG support is available (this is a compile time
*				option).
* In:
*	nothing.
* Returns:
*	True if support is available, False if not.
*****/
Boolean
XmHTMLImagePNGSupported(void)
{
#ifdef HAVE_LIBPNG
	return(True);
#else
	return(False);
#endif
}

/*****
* Name:			XmHTMLImageGZFSupported
* Return Type:	Boolean
* Description:	check if GZF support is available (this is a compile time
*				option).
* In:
*	nothing.
* Returns:
*	True if support is available, False if not.
*****/
Boolean
XmHTMLImageGZFSupported(void)
{
#if defined(HAVE_LIBPNG) || defined(HAVE_LIBZ)
	return(True);
#else
	return(False);
#endif
}

/*****
* Name: 		XmHTMLImageUpdate
* Return Type: 	XmImageStatus
* Description: 	updates an image
* In:
*	w:			XmHTMLWidget
*	image:		image info representing the image to be updated.
*				This must be an XmImageInfo structure *known* to XmHTML.
* Returns:
*	XmIMAGE_ALMOST if updating this image requires a recomputation of the
*	document layout, XmIMAGE_OK if not and some other value upon error.
*****/
XmImageStatus
XmHTMLImageUpdate(Widget w, XmImageInfo *image)
{
	XmHTMLWidget html;
	XmHTMLObjectTableElement temp;
	static String func = "XmHTMLImageUpdate";
	Boolean is_body_image;
	XmImageStatus status;
	ToolkitAbstraction *tka = NULL;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, func);
		return(XmIMAGE_ERROR);
	}

	if(image == NULL)
	{
		_XmHTMLWarning(__WFUNC__(w, func), XMHTML_MSG_21, "NULL", func);
		return(XmIMAGE_BAD);
	}

	html = (XmHTMLWidget)w;
	tka = HTML_ATTR(tka);

	/* do we already have the body image? */
	is_body_image = html->html.body_image != NULL;

	/* return error code if failed */
	if((status = _XmHTMLReplaceOrUpdateImage(html, image, NULL, &temp))
		!= XmIMAGE_OK)
		return(status);

	if(temp != NULL)
	{
		int xs, ys;
		xs = temp->x - html->html.scroll_x;
		ys = temp->y - html->html.scroll_y;
		/* We may paint the image, but we only do it when it's visible */
		if(!(xs + temp->width < 0 || xs > html->html.work_width ||
			ys + temp->height < 0 || ys > html->html.work_height))
		{
			_XmHTMLDebug(6, ("images.c: XmHTMLImageUpdate, painting image "
				"%s\n", image->url));
			/* clear the current image, don't generate an exposure */
			tka->ClearArea(tka->dpy, tka->win, xs, ys,
				temp->width, temp->height, False);
			/* put up the new image */
			_XmHTMLPaint(html, temp, temp->next);
			tka->Sync(tka->dpy, True);
		}
	}
	else
	{
		/* if we've updated the body image, plug it in */
		if(!is_body_image && html->html.body_image != NULL)
		{
			_XmHTMLClearArea(html, 0, 0, ATTR_CORE(html, width),
				ATTR_CORE(html, height));
			tka->Sync(tka->dpy, True);
		}
	}
	return(XmIMAGE_OK);
}

/*****
* Name: 		XmHTMLImageReplace
* Return Type: 	XmImageStatus
* Description: 	replaces the XmImageInfo structure with a new one
* In:
*	w:			XmHTMLWidget
*	image:		XmImageInfo structure to be replaced, must be known by XmHTML
*	new_image:	new XmImageInfo structure
* Returns:
*	XmIMAGE_ALMOST if replacing this image requires a recomputation of the
*	document layout, XmIMAGE_OK if not and some other value upon error.
*****/
XmImageStatus
XmHTMLImageReplace(Widget w, XmImageInfo *image, XmImageInfo *new_image)
{
	XmHTMLWidget html;
	XmHTMLObjectTableElement temp;
	XmImageStatus status;
	Boolean is_body_image;
	static String func = "XmHTMLImageReplace";
	ToolkitAbstraction *tka = NULL;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, func);
		return(XmIMAGE_ERROR);
	}

	/* sanity */
	if(image == NULL || new_image == NULL)
	{
		_XmHTMLWarning(__WFUNC__(w, func), XMHTML_MSG_21,
			(image == NULL ? "NULL image arg" : "NULL new_image arg"), func);
		return(XmIMAGE_BAD);
	}
	html = (XmHTMLWidget)w;
	tka = HTML_ATTR(tka);

	/* do we already have the body image? */
	is_body_image = html->html.body_image != NULL;

	if((status = _XmHTMLReplaceOrUpdateImage(html, image, new_image, &temp))
		!= XmIMAGE_OK)
		return(status);

	if(temp != NULL)
	{
		int xs, ys;
		xs = temp->x - html->html.scroll_x;
		ys = temp->y - html->html.scroll_y;
		/* We may paint the image, but we only do it when it's visible */
		if(!(xs + temp->width < 0 || xs > html->html.work_width ||
			ys + temp->height < 0 || ys > html->html.work_height))
		{
			_XmHTMLDebug(6, ("images.c: XmHTMLImageReplace, painting image "
				"%s\n", image->url));
			/* clear the current image, don't generate an exposure */
			tka->ClearArea(tka->dpy, tka->win, xs, ys,
				temp->width, temp->height, False);
			/* put up the new image */
			_XmHTMLPaint(html, temp, temp->next);
			tka->Sync(tka->dpy, True);
		}
	}
	else
	{
		/* if we've replaced the body image, plug it in */
		if(!is_body_image && html->html.body_image != NULL)
		{
			_XmHTMLClearArea(html, 0, 0, ATTR_CORE(html,width),
				ATTR_CORE(html,height));
			tka->Sync(tka->dpy, True);
		}
	}
	return(XmIMAGE_OK);
}

/**********
***** Public Tag Analyzer Functions
**********/

/*****
* Name: 		XmHTMLTagCheck
* Return Type: 	Boolean
* Description: 	checks whether the given tag exists in the attributes of a
*				HTML element
* In:
*	attributes:	attributes from an HTML element
*	tag:		tag to look for.
* Returns:
*	True if tag is found, False otherwise.
*****/
Boolean
XmHTMLTagCheck(String attributes, String tag)
{
	return _XmHTMLTagCheck(attributes, tag);
}

/*****
* Name: 		XmHTMLTagGetValue
* Return Type: 	String
* Description: 	looks for the specified tag in the given list of attributes.
* In:
*	attributes:	attributes from an HTML element
*	tag:		tag to look for.
* Returns:
*	if tag exists, the value of this tag, NULL otherwise.
*	return value is always malloc'd; caller must free it.
*****/
String
XmHTMLTagGetValue(String attributes, String tag)
{
	return _XmHTMLTagGetValue(attributes, tag);
}

/*****
* Name: 		XmHTMLTagGetNumber
* Return Type: 	int
* Description: 	retrieves the numerical value of the given tag.
* In:
*	attributes:	attributes from an HTML element
*	tag:		tag to look for.
*	def:		default value if tag is not found.
* Returns:
*	if tag exists, the value of this tag, def otherwise
*****/
int
XmHTMLTagGetNumber(String attributes, String tag, int default_value)
{
	return _XmHTMLTagGetNumber(attributes, tag, default_value);
}

/*****
* Name:			XmHTMLTagCheckNumber
* Return Type: 	int
* Description: 	retrieves the numerical value of the given tag.
*				If the returned no is negative, the specified value was
*				a relative number. Otherwise it's an absolute number.
* In:
*	attributes:	attributes from an HTML element
*	tag:		tag to look for.
*	def:		default value if tag is not found.
* Returns:
*	if tag exists, the value of this tag, def otherwise
*****/
int
XmHTMLTagCheckNumber(String attributes, String tag,
	int default_value)
{
	return _XmHTMLTagCheckNumber(attributes, tag, default_value);
}

/*****
* Name: 		XmHTMLTagCheckValue
* Return Type: 	Boolean
* Description: 	checks whether the specified tag in the given list of attributes
*				has a certain value.
* In:
*	attributes:	attributes from an HTML element
*	tag:		tag to look for.
*	check:		value to check.
* Returns:
*	returns True if tag exists and has the correct value, False otherwise.
*****/
Boolean
XmHTMLTagCheckValue(String attributes, String tag, String check)
{
	return _XmHTMLTagCheckValue(attributes, tag, check);
}

/**********
***** Misc. Public Functions
**********/

/*****
* Name: 		XmHTMLGetVersion
* Return Type: 	int
* Description: 	returns the version number of XmHTML
* In:
*	nothing
* Returns:
*	version number of this library.
*****/
int
XmHTMLGetVersion(void)
{
	return(XmHTMLVersion);
}

#ifndef STRINGIFY
#define STRINGIFY(x)	#x
#endif
#define STR(x)	STRINGIFY(x)

char*
XmHTMLGetVersionString(void)
{
#ifndef SHORT_VERSION_STRING
	static String version = {XmHTMLVERSION_STRING "\n"
# ifdef I18N
		"Multilingual and Internationalization support available.\n"
# endif
# ifdef HAVE_LIBJPEG
#  ifdef JPEG_LIB_VERSION
		"libJPEG version " STR(JPEG_LIB_VERSION) " Image support available.\n"
#  else
		"libJPEG version (unknown version) Image support available.\n"
#  endif	/* JPEG_LIB_VERSION */
# endif	/* HAVE_LIBJPEG */
# ifdef HAVE_LIBPNG
#  ifdef PNG_LIBPNG_VER_STRING
		"libpng  version " PNG_LIBPNG_VER_STRING " Image support available.\n"
#  else
		"libpng  version (unknown version) Image support available.\n"
#  endif	/* PNG_LIBPNG_VER_STRING */
# endif	/* HAVE_LIBPNG */
# ifdef ZLIB_VERSION		/* included by libpng */
		"libz    version " ZLIB_VERSION " compression library support "
		"available.\n"
#  else
#   ifdef HAVE_LIBZ
		"libz    version (unknown version) compression library support "
		"available.\n"
#   endif /* HAVE_LIBZ */
#  endif /* ZLIB_VERSION */
		"This library is distributed under the terms of the\n"
		"GNU Library General Public License."};
	return(version);
#else
	return(XmHTMLVERSION_STRING);
#endif	/* SHORT_VERSION_STRING */
}

/*****
* Name: 		XmHTMLGetTitle
* Return Type: 	String
* Description: 	returns the value of the <title></title> element
* In:
*	w:			XmHTMLWidget in question
* Returns:
*	value of the title upon success, NULL on failure.
*****/
String
XmHTMLGetTitle(Widget w)
{
	XmHTMLWidget html;
	XmHTMLObject *tmp;
	static String ret_val;
	String start, end;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "GetTitle");
		return(NULL);
	}

	html = (XmHTMLWidget)w;

	for(tmp = HTML_ATTR(elements);
		tmp != NULL && tmp->id != HT_TITLE && tmp->id != HT_BODY;
		tmp = tmp->next);

	/* sanity check */
	if(!tmp || !tmp->next || tmp->id == HT_BODY)
		return(NULL);

	/* ok, we have reached the title element, pick up the text */
	tmp = tmp->next;

	/* another sanity check */
	if(!tmp->element)
		return(NULL);

	/* skip leading... */
	for(start = tmp->element; *start != '\0' && isspace(*start); start++);

	/* ...and trailing whitespace */
	for(end = &start[strlen(start)-1]; *end != '\0' && isspace(*end);
		end--);

	/* always backs up one to many */
	end++;

	/* sanity */
	if(*start == '\0' || (end - start) <= 0)
		return(NULL);

  	/* duplicate the title */
	ret_val = my_strndup(start, end - start);

	/* expand escape sequences */
	_XmHTMLExpandEscapes(ret_val, HTML_ATTR(bad_html_warnings));

	/* and return to caller */
	return(ret_val);
}

/*****
* Name: 		XmHTMLXYToInfo
* Return Type: 	XmHTMLInfoStructure*
* Description: 	Retrieves the contents of an image and/or anchor at the
*				given cursor position.
* In:
*	w:			XmHTMLWidget id;
*	x:			x-location of pointer, relative to left side of the workArea
*	y:			y-location of pointer, relative to top side of the workArea
* Returns:
*	A filled XmHTMLInfoStructure when the pointer was pressed on an image
*	and/or anchor. NULL if not.
* Note:
*	The return value, nor one of its members may be freed by the caller.
*****/
XmHTMLInfoPtr
XmHTMLXYToInfo(Widget w, int x, int y)
{
	static XmHTMLInfoStructure cbs;
	static XmHTMLImage *image;
	static XmHTMLAnchorCallbackStruct anchor_cbs;
	static XmImageInfo info;
	long line = -1;
	XmHTMLAnchor *anchor;
	XmHTMLWord *anchor_word;
	XmHTMLImage anchor_img;
	XmHTMLWidget html;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "XYToInfo");
		return(NULL);
	}

	html = (XmHTMLWidget)w;

	/* default fields */
	cbs.x      = x - CORE_ATTR(x);
	cbs.y      = y - CORE_ATTR(y);
	cbs.is_map = XmMAP_NONE;
	cbs.image  = NULL;
	cbs.anchor = NULL;
	line = -1;

	/* pick up a possible anchor or imagemap location */
	anchor = NULL;

	if((anchor_word = _XmHTMLGetAnchor(html, x, y, &anchor_img)) == NULL)
		anchor = _XmHTMLGetImageAnchor(html, x, y, &anchor_img);

	/* no regular anchor, see if it's an imagemap */
	if(anchor == NULL && anchor_word)
		anchor = anchor_word->owner->anchor;

	/*
	* Final check: if this anchor is a form component it can't be followed
	* as this is an internal-only anchor.
	*/
	if(anchor && anchor->url_type == ANCHOR_FORM_IMAGE)
		anchor = NULL;

	/* check if we have an anchor */
	if(anchor != NULL)
	{
		/* set to zero */
		(void)memset(&anchor_cbs, 0, sizeof(XmHTMLAnchorCallbackStruct));

		/* initialize callback fields */
		anchor_cbs.reason   = XmCR_ACTIVATE;
		anchor_cbs.event    = NULL;
		anchor_cbs.url_type = anchor->url_type;
		anchor_cbs.line     = anchor->line;
		anchor_cbs.href     = anchor->href;
		anchor_cbs.target   = anchor->target;
		anchor_cbs.rel      = anchor->rel;
		anchor_cbs.rev      = anchor->rev;
		anchor_cbs.title    = anchor->title;
		anchor_cbs.doit     = False;
		anchor_cbs.visited  = anchor->visited;
		cbs.anchor = &anchor_cbs;
		line       = anchor->line;
	}

	/* check if we have an image.*/
	if((image = _XmHTMLOnImage(html, x, y)) != NULL)
	{
		/* set map type */
		cbs.is_map = (image->map_type != XmMAP_NONE);

		if(image->html_image != NULL)
		{
			/* no image info if this image is being loaded progressively */
			if(!ImageInfoProgressive(image->html_image))
			{
				/* use real url but link all other members */
				info.url          = image->url;
				info.data         = image->html_image->data;
				info.clip         = image->html_image->clip;
				info.width        = image->html_image->width;
				info.height       = image->html_image->height;
				info.reds         = image->html_image->reds;
				info.greens       = image->html_image->greens;
				info.blues        = image->html_image->blues;
				info.bg           = image->html_image->bg;
				info.ncolors      = image->html_image->ncolors;
				info.options      = image->html_image->options;
				info.type         = image->html_image->type;
				info.depth        = image->html_image->depth;
				info.colorspace   = image->html_image->colorspace;
				info.transparency = image->html_image->transparency;
				info.swidth       = image->html_image->swidth;
				info.sheight      = image->html_image->sheight;
				info.scolors      = image->html_image->scolors;
				info.alpha        = image->html_image->alpha;
				info.fg_gamma     = image->html_image->fg_gamma;
				info.x            = image->html_image->x;
				info.y            = image->html_image->y;
				info.loop_count   = image->html_image->loop_count;
				info.dispose      = image->html_image->dispose;
				info.timeout      = image->html_image->timeout;
				info.nframes      = image->html_image->nframes;
				info.frame        = image->html_image->frame;
				info.user_data    = image->html_image->user_data;
				/* set it */
				cbs.image = &info;
			}
		}
		else
		{
			/* XmImageInfo has been freed, construct one */
			/* set to zero */
			memset(&info, 0, sizeof(XmImageInfo));
			/* fill in the fields we know */
			info.url     = image->url;
			info.type    = IMAGE_UNKNOWN;
			info.width   = image->swidth;
			info.height  = image->sheight;
			info.swidth  = image->width;
			info.sheight = image->height;
			info.ncolors = image->npixels;
			info.nframes = image->nframes;
			/* set it */
			cbs.image     = &info;
		}
		if(line == -1)
			line = (image->owner ? image->owner->line : -1);
	}
	/* no line number yet, get one */
	if(line == -1)
		cbs.line = _XmHTMLVerticalPosToLine(html, y + HTML_ATTR(scroll_y));
	else
		cbs.line = line;
	return(&cbs);
}

/*****
* Name:			XmHTMLGetDocumentInfo
* Return Type: 	XmHTMLDocumentInfo
* Description: 	returns a list of all images and anchors found in the
*				currently displayed page.
* In:
*	w:			XmHTMLWidget id.
* Returns:
*	a new XmHTMLDocumentInfo structure on success or NULL on failure.
*****/
XmHTMLDocumentInfo*
XmHTMLGetDocumentInfo(Widget w)
{
	XmHTMLWidget html;
	XmHTMLImage *image;
	XmHTMLAnchor *anchor;
	int ssize = 0;
	Byte *chPtr;
	static XmHTMLDocumentInfo *doc_info = NULL;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "GetDocumentInfo");
		return(NULL);
	}
	html = (XmHTMLWidget)w;

	doc_info = (XmHTMLDocumentInfo*)calloc(1, sizeof(XmHTMLDocumentInfo));

	/* background image ? */
	if((image = HTML_ATTR(images)) != NULL && ImageIsBackground(image))
	{
		doc_info->bg_image = strdup(image->url);
		image = image->next;
	}
	if(image)
	{
		/* see how many images we have */
		ssize = 0;
		for(; image != NULL; image = image->next)
			ssize += (strlen(image->url) + 1);
		ssize++;	/* terminating \0 */

		/* allocate */
		doc_info->images = (String)calloc(ssize, sizeof(Byte));

		image = ImageIsBackground(HTML_ATTR(images)) ?
					HTML_ATTR(images->next) : HTML_ATTR(images);

		chPtr = (Byte*)doc_info->images;
		while(image != NULL)
		{
			(void)memcpy(chPtr, image->url, strlen(image->url));
			chPtr += (strlen(image->url) + 1);
			image = image->next;
		}
	}

	/* see how many anchors we have */
	ssize = 0;
	for(anchor = HTML_ATTR(anchor_data); anchor != NULL; anchor = anchor->next)
	{
		if(*anchor->href != '\0')
			ssize += (strlen(anchor->href) + 1);
	}
	ssize++;	/* terminating \0 */

	doc_info->anchors = (String)calloc(ssize, sizeof(Byte));

	chPtr = (Byte*)doc_info->anchors;
	for(anchor = HTML_ATTR(anchor_data); anchor != NULL; anchor = anchor->next)
	{
		if(*anchor->href != '\0')
		{
			(void)memcpy(chPtr, anchor->href, strlen(anchor->href));
			chPtr += (strlen(anchor->href) + 1);
		}
	}
	return(doc_info);
}

/*****
* Name:			XmHTMLFreeDocumentInfo
* Return Type: 	void
* Description: 	frees a XmHTMLDocumentInfo structure.
* In:
*	doc_info:	structure to be freed.
* Returns:
*	nothing.
*****/
void
XmHTMLFreeDocumentInfo(XmHTMLDocumentInfo *doc_info)
{
	if(doc_info == NULL)
		return;

	if(doc_info->bg_image)
		free(doc_info->bg_image);
	if(doc_info->images)
		free(doc_info->images);
	if(doc_info->anchors)
		free(doc_info->anchors);
	free(doc_info);
}

/*****
* Name: 		XmHTMLRedisplay
* Return Type: 	void
* Description: 	forces a layout recomputation of the currently loaded document
*				and triggers a redisplay.
* In:
*	w:			Widget for which to redo layout computation.
* Returns:
*	nothing
* Note:
*	This function is mostly useful in combination with the image updating
*	and/or replacing routines.
*****/
void
XmHTMLRedisplay(Widget w)
{
	XmHTMLWidget html;
	ToolkitAbstraction *tka;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "Redisplay");
		return;
	}

	html = (XmHTMLWidget)w;
	tka = HTML_ATTR(tka);

	/* recompute screen layout */
	_XmHTMLLayout(html);

	if(HTML_ATTR(gc) != NULL)
	{
		_XmHTMLClearArea(html, 0, 0, CORE_ATTR(width), CORE_ATTR(height));

		/* sync so the display is updated */
		tka->Sync(XtDisplay((Widget)html), False);

		XmUpdateDisplay((Widget)html);
		if(tka->IsManaged(HTML_ATTR(vsb)))
			XmUpdateDisplay(HTML_ATTR(vsb));
		if(tka->IsManaged(HTML_ATTR(hsb)))
			XmUpdateDisplay(HTML_ATTR(hsb));
	}
}

/*
   A function used to get around refreshing problems without
   the potential for resizing if _XmHTMLLayout(html);
   is called as in XmHTMLRedisplay.    ZSS March 2012
*/
void
XmHTMLRefresh(Widget w)
{
	XmHTMLWidget html;
	ToolkitAbstraction *tka;
	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "Refresh");
		return;
	}

	html = (XmHTMLWidget)w;
	tka = HTML_ATTR(tka);

	if(HTML_ATTR(gc) != NULL)
	{
		_XmHTMLClearArea(html, 0, 0, CORE_ATTR(width), CORE_ATTR(height));

		/* sync so the display is updated */
		tka->Sync(XtDisplay((Widget)html), False);

		XmUpdateDisplay((Widget)html);
		if(tka->IsManaged(HTML_ATTR(vsb)))
			XmUpdateDisplay(HTML_ATTR(vsb));
		if(tka->IsManaged(HTML_ATTR(hsb)))
			XmUpdateDisplay(HTML_ATTR(hsb));
	}
}
