#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* callbacks.c : XmHTML callback routines
*
* This file Version	$Revision$
*
* Creation date:		Mon Dec  2 19:58:52 GMT+0100 1996
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
* Revision 1.1  2011/06/30 16:10:37  rwcox
* Cadd
*
* Revision 1.14  1998/04/27 06:58:21  newt
* Now contains all callbacks XmHTML can issue
*
* Revision 1.13  1998/04/04 06:28:01  newt
* XmHTML Beta 1.1.3
*
* Revision 1.12  1997/10/26 23:50:15  newt
* Bugfix 10/22/97-01
*
* Revision 1.11  1997/10/23 00:24:50  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.10  1997/08/30 00:45:33  newt
* my_strdup -> strdup and _XmHTMLWarning proto changes.
*
* Revision 1.9  1997/08/01 12:57:23  newt
* my_strdup -> strdup
*
* Revision 1.8  1997/05/28 01:44:43  newt
* Extended XmHTMLGetHeadAttributes to copy the value of the <SCRIPT> and
* <STYLE> head attributes.
*
* Revision 1.7  1997/04/29 14:24:02  newt
* bugfix in linkCallback. Added XmHTMLGetHeadAttributes.
*
* Revision 1.6  1997/03/20 08:07:50  newt
* changes in parseLinks
*
* Revision 1.5  1997/03/02 23:14:55  newt
* Changes due to changes in XmHTMLAnchorCallbackStruct
*
* Revision 1.4  1997/02/11 02:06:18  newt
* Bugfix in ActivateCallback
*
* Revision 1.3  1997/01/09 06:54:56  newt
* expanded copyright marker
*
* Revision 1.2  1997/01/09 06:43:09  newt
* bugfix in ParseLinks
*
* Revision 1.1  1996/12/19 02:17:06  newt
* Initial Revision
*
*****/
/*****
* Note:
* 	The XmNarmCallback resource is served in XmHTML.c, routine ExtendEnd.
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

/* Local includes */
#include "toolkit.h"
#include XmHTMLPrivateHeader

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/
static XmHTMLLinkDataPtr ParseLinks(XmHTMLObject *list, int *num_link);
static XmHTMLMetaDataPtr ParseMeta(XmHTMLObject *list, int *num_meta);

#define HEAD_FREE_DOCTYPE { \
	if(head->doctype) free(head->doctype); head->doctype = NULL; \
}

#define HEAD_FREE_TITLE { \
	if(head->title) free(head->title); head->title = NULL; \
}

#define HEAD_FREE_BASE { \
	if(head->base) free(head->base); head->base = NULL; \
}

#define HEAD_FREE_SCRIPT { \
	if(head->script) free(head->script); head->script = NULL; \
	if(head->script_lang) free(head->script_lang); head->script_lang = NULL; \
}

#define HEAD_FREE_STYLE { \
	if(head->style_type) free(head->style_type); \
	if(head->style) free(head->style); \
	head->style_type = NULL; head->style = NULL; \
}

#define HEAD_FREE_META { \
	if(head->num_meta) { \
		int i; for(i = 0; i < head->num_meta; i++) { \
			if(head->meta[i].http_equiv) free(head->meta[i].http_equiv); \
			if(head->meta[i].name) free(head->meta[i].name); \
			if(head->meta[i].content) free(head->meta[i].content); \
		} free(head->meta); \
	} head->meta = NULL; head->num_meta = 0; \
}

#define HEAD_FREE_LINK { \
	if(head->num_link) { \
		int i; for(i = 0; i < head->num_link; i++) { \
			if(head->link[i].url) free(head->link[i].url); \
			if(head->link[i].rel) free(head->link[i].rel); \
			if(head->link[i].rev) free(head->link[i].rev); \
			if(head->link[i].title) free(head->link[i].title); \
		} free(head->link); \
	} head->link = NULL; head->num_link = 0; \
}

/*** Private Variable Declarations ***/

static XmHTMLLinkDataPtr
ParseLinks(XmHTMLObject *list, int *num_link)
{
	static XmHTMLLinkDataPtr link = NULL;
	XmHTMLObject *temp = list;
	int i = 0;
	char *tmp;

	/* We have got some links. Allocate memory */
	link = (XmHTMLLinkDataPtr)calloc(*num_link, sizeof(XmHTMLLinkDataRec));

	for(; temp != NULL && temp->id != HT_BODY && i < *num_link;
		temp = temp->next)
	{
		if(temp->id == HT_LINK)
		{
			if(temp->attributes == NULL)	/* invalid link member */
				continue;

			/* get value of REL tag */
			if((tmp = _XmHTMLTagGetValue(temp->attributes, "rel")) != NULL)
			{
				/* make lowercase */
				my_locase(tmp);
				link[i].rel = tmp;
			}
			else /* get value of REV tag */
			{
				if((tmp = _XmHTMLTagGetValue(temp->attributes, "rev")) != NULL)
				{
					/* make lowercase */
					my_locase(tmp);
					link[i].rev = tmp;
				}
				else	/* invalid link member */
					continue;
			}

			/* get value of URL tag */
			if((tmp = _XmHTMLTagGetValue(temp->attributes, "href")) != NULL)
				link[i].url = tmp;
			else	/* href is mandatory */
			{
				if(link[i].rel)
					free(link[i].rel);
				if(link[i].rev)
					free(link[i].rel);
				continue;
			}

			/* get value of TITLE tag */
			if((tmp = _XmHTMLTagGetValue(temp->attributes, "title")) != NULL)
				link[i].title = tmp;
			i++;
		}
	}
	/* adjust link count for actually allocated elements */
	*num_link = i;

	return(link);
}

static XmHTMLMetaDataPtr
ParseMeta(XmHTMLObject *list, int *num_meta)
{
	static XmHTMLMetaDataPtr meta = NULL;
	XmHTMLObject *temp = list;
	int i = 0;
	char *tmp;

	/* We have got some links. Allocate memory */
	meta = (XmHTMLMetaDataPtr)calloc(*num_meta, sizeof(XmHTMLMetaDataRec));

	for(; temp != NULL && temp->id != HT_BODY && i < *num_meta;
		temp = temp->next)
	{
		if(temp->id == HT_META)
		{
			if(temp->attributes == NULL)	/* invalid meta member */
				continue;

			/* get value of http-equiv tag */
			if((tmp = _XmHTMLTagGetValue(temp->attributes,
				"http-equiv")) != NULL)
			{
				/* make lowercase */
				my_locase(tmp);
				meta[i].http_equiv = tmp;
			}
			else /* get value of name tag */
			{
				if((tmp = _XmHTMLTagGetValue(temp->attributes, "name")) != NULL)
				{
					/* make lowercase */
					my_locase(tmp);
					meta[i].name = tmp;
				}
				else	/* invalid meta element */
					continue;
			}

			/* get value of content tag */
			if((tmp = _XmHTMLTagGetValue(temp->attributes, "content")) != NULL)
				meta[i].content = tmp;
			else	/* invalid meta element */
			{
				if(meta[i].http_equiv)
					free(meta[i].http_equiv);
				if(meta[i].name)
					free(meta[i].name);
				continue;
			}
			i++;
		}
	}
	/* adjust meta count for actually allocated elements */
	*num_meta = i;
	return(meta);
}

/*****
* Name:			_XmHTMLLinkCallback
* Return Type:	void
* Description:	calls installed callback routines.
* In:
*	w:			widget to check
*	id:			id of callback to call ( = reason field)
* Returns:
*	nothing.
*****/
void
_XmHTMLLinkCallback(XmHTMLWidget html)
{
	XmHTMLObject *temp = HTML_ATTR(elements), *start = NULL;
	XmHTMLLinkCallbackStruct cbs;
	int i, num_link = 0;

	/* initialize callback fields */
	cbs.reason = XmCR_HTML_LINK;
	cbs.num_link = 0;
	cbs.event = NULL;

	/* count how many link elements we have */
	for(temp = HTML_ATTR(elements); temp != NULL && temp->id != HT_BODY;
		temp = temp->next)
	{
		if(temp->id == HT_LINK)
		{
			num_link++;
			start = (num_link == 1 ? temp : start);
		}
	}
	/* no <LINK> found, call with a zero links field */
	if(num_link == 0 || start == NULL)
	{
		cbs.link = NULL;
		TkaCallCallbackList(html, link, &cbs);
		return;
	}

	/* parse all link elements */
	cbs.link = ParseLinks(start, &num_link);
	cbs.num_link = num_link;

	TkaCallCallbackList(html, link, &cbs);

	/* free everything */
	for(i = 0; i < num_link; i++)
	{
		if(cbs.link[i].rel)
			free(cbs.link[i].rel);
		if(cbs.link[i].rev)
			free(cbs.link[i].rev);
		if(cbs.link[i].url)
			free(cbs.link[i].url);
		if(cbs.link[i].title)
			free(cbs.link[i].title);
	}
	free(cbs.link);
}

/*****
* Name: 		_XmHTMLTrackCallback
* Return Type: 	void
* Description: 	routine associated with the XmNanchorTrackCallback resource.
*				fills in the appropriate fields in the XmHTMLCallbackStruct.
* In:
*	html:		widget for which callback is to be activated
*	event:		event that triggered this callback to happen.
*	anchor:		anchor data (if any)
* Returns:
*	nothing.
* Note:
*	not a single member of the anchorCallbackStruct may be freed by the
*	user. Havoc will occur otherwise.
*****/
void
_XmHTMLTrackCallback(XmHTMLWidget html, XEvent *event, XmHTMLAnchor *anchor)
{
	XmHTMLAnchorCallbackStruct cbs;

	_XmHTMLDebug(3, ("callbacks.c: _XmHTMLTrackCallback Start\n"));

	(void)memset(&cbs, 0, sizeof(XmHTMLAnchorCallbackStruct));

	/* initialize callback fields */
	cbs.reason = XmCR_HTML_ANCHORTRACK;
	cbs.event = event;

	if(anchor != NULL)
	{
		cbs.url_type = anchor->url_type;	/* doesn't matter */
		cbs.line     = anchor->line;
		cbs.href     = anchor->href;
		cbs.target   = anchor->target;
		cbs.rel      = anchor->rel;
		cbs.rev      = anchor->rev;
		cbs.title    = anchor->title;
		cbs.doit     = False;				/* doesn't matter */
		cbs.visited  = anchor->visited;		/* doesn't matter */
	}

	TkaCallCallbackList(html, anchor_track, &cbs);

	_XmHTMLDebug(3, ("callbacks.c: _XmHTMLTrackCallback End\n"));
}

/*****
* Name: 		_XmHTMLActivateCallback
* Return Type: 	Boolean
* Description: 	routine associated with the XmNactivateCallback resource.
*				fills in the appropriate fields in the XmHTMLCallbackStruct.
* In:
*	html:		widget for which callback is to be activated
*	event:		event that triggered this callback to happen.
*	anchor:		data of activated anchor (if any).
* Returns:
*	value of the doc_modified field.
* Note:
*	not a single member of the anchorCallbackStruct may be freed by the
*	user. Havoc will occur otherwise.
*****/
Boolean
_XmHTMLActivateCallback(XmHTMLWidget html, XEvent *event, XmHTMLAnchor *anchor)
{
	XmHTMLAnchorCallbackStruct cbs;
	XmHTMLObjectTableElement jump_anchor = NULL;
	XmHTMLAnchor *tmp;

	_XmHTMLDebug(3, ("callbacks.c: _XmHTMLActivateCallback Start\n"));

	/* sanity check */
	if(anchor == NULL)
		return(False);

	/* set to zero */
	(void)memset(&cbs, 0, sizeof(XmHTMLAnchorCallbackStruct));

	/* initialize callback fields */
	cbs.reason   = XmCR_ACTIVATE;
	cbs.event    = event;
	cbs.url_type = anchor->url_type;
	cbs.line     = anchor->line;
	cbs.href     = anchor->href;
	cbs.target   = anchor->target;
	cbs.rel      = anchor->rel;
	cbs.rev      = anchor->rev;
	cbs.title    = anchor->title;
	cbs.doit     = False;
	cbs.visited  = anchor->visited;
	cbs.doc_modified = False;

	TkaCallCallbackList(html, activate, &cbs);

	if(cbs.doc_modified)
		return(True);

	/*
	* If we have a local anchor, see if we should mark it as visited
	* and if we should jump to it. The jumping itself is postponed to the
	* end of this routine.
	*/
	if(anchor->url_type == ANCHOR_JUMP)
	{
		/* set new foreground color */
		if(cbs.visited)
		{
			/* first check if this anchor wasn't already visited */
			if(!anchor->visited)
			{
				int i;
				Byte line_style;
				/* mark all other anchors pointing to the same name as well */
				for(i = 0 ; i < HTML_ATTR(anchor_words) ; i++)
				{
					/* check if this anchor matches */
					tmp = HTML_ATTR(anchors)[i].owner->anchor;
					if(!(strcasecmp(tmp->href, anchor->href)))
					{
						int j;
						/* a match, set the foreground of the master block */
						HTML_ATTR(anchors)[i].owner->fg =
							HTML_ATTR(anchor_visited_fg);

						/* change underline style as well! */
						line_style = HTML_ATTR(anchor_visited_line);
						if(HTML_ATTR(anchors)[i].self->line_data & LINE_STRIKE)
							line_style |= LINE_STRIKE;

						/* update all words for this anchor */
						for(j = 0; j < HTML_ATTR(anchors)[i].owner->n_words;
							j++)
						{
							HTML_ATTR(anchors)[i].owner->words[j].line_data =
								line_style;
						}
					}
					/* skip remaining anchor words of the master block */
					while(i < HTML_ATTR(anchor_words) - 1 &&
						HTML_ATTR(anchors)[i].owner ==
							HTML_ATTR(anchors)[i+1].owner)
						i++;
				}
			}
		}
		if(cbs.doit)
		{
			jump_anchor = _XmHTMLGetAnchorByName(html, anchor->href);

			if(jump_anchor == NULL)
			{
				_XmHTMLWarning(__WFUNC__(html, "_XmHTMLActivateCallback"),
					XMHTML_MSG_28, anchor->href);
			}
			else
			{
				_XmHTMLDebug(3, ("callbacks.c: _XmHTMLActivateCallback, "
					"internal anchor referenced: name = %s, id = %i, y = %i\n",
					anchor->href, jump_anchor->id, jump_anchor->y));
			}
		}
	}

	/* jump to the requested anchor only if we have a vertical scrollbar */
	/* fix 01/30/97-05, kdh */
	/* fix 10/22/97-01, kdh */
	if(jump_anchor && HTML_ATTR(needs_vsb))
	{
		int value;

		_XmHTMLDebug(3, ("callbacks.c: _XmHTMLActivateCallback, "
			"calling _XmHTMLMoveToPos, y = %i\n", jump_anchor->y));

		/* check slider value and adjust if necessary */
		value = jump_anchor->y - jump_anchor->height;

		_XmHTMLAdjustVerticalScrollValue(HTML_ATTR(vsb), &value);

		_XmHTMLMoveToPos(HTML_ATTR(vsb), html, value);
	}

	_XmHTMLDebug(3, ("callbacks.c: _XmHTMLActivateCallback End\n"));

	return(False);
}

/*****
* Name: 		_XmHTMLDocumentCallback
* Return Type: 	Boolean
* Description: 	XtNdocumentCallback driver
* In:
*	PARSER:		current parser.
*	verified:	true when the flushed stack is balanced, False when not.
* Returns:
* 	False when another pass on the current text is requested/required,
*	True when not.
*****/
Boolean
_XmHTMLDocumentCallback(XmHTMLWidget html, Boolean html32, Boolean verified,
	Boolean balanced, Boolean terminated, int pass_level)
{
	XmHTMLDocumentCallbackStruct cbs;

	if(TkaHasCallback(html, document_callback))
	{
		cbs.reason     = XmCR_HTML_DOCUMENT;
		cbs.event      = (XEvent*)NULL;
		cbs.html32     = html32;
		cbs.verified   = verified;
		cbs.balanced   = balanced;
		cbs.terminated = terminated;
		cbs.pass_level = pass_level;
		cbs.redo       = !balanced;

		TkaCallCallbackList(html, document, &cbs);

		return(cbs.redo);
	}
	return(True);
}

/*****
* Name: 		freeHeadMembers
* Return Type: 	void
* Description: 	frees the requested members of the given HeadAttributes
* In:
*	*head:		structure of which to free members
*	mask_bits:	fields to free
* Returns:
*	nothing
*****/
static void
freeHeadAttributes(XmHTMLHeadAttributes *head, Byte mask_bits)
{
	if(mask_bits & HeadDocType)
	{
		HEAD_FREE_DOCTYPE;
	}
	if(mask_bits & HeadTitle)
	{
		HEAD_FREE_TITLE;
	}
	if(mask_bits & HeadBase)
	{
		HEAD_FREE_BASE;
	}
	if(mask_bits & HeadScript)
	{
		HEAD_FREE_SCRIPT;
	}
	if(mask_bits & HeadStyle)
	{
		HEAD_FREE_STYLE;
	}
	if(mask_bits & HeadMeta)
	{
		HEAD_FREE_META;
	}
	if(mask_bits & HeadLink)
	{
		HEAD_FREE_LINK;
	}
}

/*****
* Name: 		XmHTMLGetHeadAttributes
* Return Type: 	Boolean
* Description: 	fills the given HeadAttributes with the requested document
*				head elements.
* In:
*	w:			XmHTMLWidget id
*	*head:		structure to fill
*	mask_bits:	indicates what members to fill
* Returns:
*	True when a <head></head> block is present, False if not.
*****/
Boolean
XmHTMLGetHeadAttributes(Widget w, XmHTMLHeadAttributes *head, Byte mask_bits)
{
	XmHTMLWidget html;
	XmHTMLObject *tmp, *link_start = NULL, *meta_start = NULL;
	int num_link = 0, num_meta = 0;
	static String func = "GetHeadAttributes";
	Boolean head_found = True;

	/* sanity check */
	if(!head)
	{
		_XmHTMLWarning(__WFUNC__(w, func), XMHTML_MSG_21, "NULL", func);
		return(False);
	}

	/****
	* Don't bother to check a thing when we only have to clear all
	* attributes.
	****/
	if(mask_bits == HeadClear)
	{
		freeHeadAttributes(head, HeadAll);
		return(False);
	}

	/* another sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, func);
		return(False);
	}
	html = (XmHTMLWidget)w;

	/* free requested members */
	freeHeadAttributes(head, mask_bits);

	/* empty document */
	if(HTML_ATTR(elements) == NULL)
		return(False);

	/* walk until we reach HT_HEAD or HT_BODY, whatever comes first */
	for(tmp = HTML_ATTR(elements); tmp != NULL && tmp->id != HT_HEAD
		&& tmp->id != HT_BODY; tmp = tmp->next)
	{
		/* pick up doctype if we happen to see it */
		if(tmp->id == HT_DOCTYPE && tmp->attributes &&
			(mask_bits & HeadDocType || mask_bits == HeadAll))
			head->doctype = strdup(tmp->attributes);
	}

	/* no head found */
	if(tmp == NULL || tmp->id == HT_BODY)
	{
		head_found = False;
		tmp = HTML_ATTR(elements);
	}

	/* we have found the head. */
	tmp = tmp->next;	/* move past it */

	/* Go and fill all members */
	for(; tmp != NULL; tmp = tmp->next)
	{
		switch(tmp->id)
		{
			case HT_LINK:
				num_link++;
				link_start = (num_link == 1 ? tmp : link_start);
				break;
			case HT_META:
				num_meta++;
				meta_start = (num_meta == 1 ? tmp : meta_start);
				break;
			case HT_ISINDEX:
				if(mask_bits & HeadIsIndex || mask_bits == HeadAll)
					head->is_index = True;
				break;
			case HT_TITLE:
				if((mask_bits & HeadTitle || mask_bits == HeadAll)
					&& !tmp->is_end)
				{
					String start, end;

					/* pick up the text, its all in a single element */
					tmp = tmp->next;

					/* sanity check */
					if(!tmp->element)
						break;

					/* skip leading... */
					for(start = tmp->element; *start != '\0' &&
						isspace(*start); start++);

					/* sanity */
					if(*start == '\0')
						break;

					/* ...and trailing whitespace */
					for(end = &start[strlen(start)-1]; *end != '\0' &&
						isspace(*end); end--);

					/* always backs up one to many */
					end++;

					/* sanity */
					if(end - start <= 0)
						break;

  					/* duplicate the title */
					head->title = my_strndup(start, end - start);

					/* expand escape sequences */
					_XmHTMLExpandEscapes(head->title,
						HTML_ATTR(bad_html_warnings));
				}
				break;
			case HT_BASE:
				if((mask_bits & HeadBase || mask_bits == HeadAll) &&
					tmp->attributes)
					head->base = _XmHTMLTagGetValue(tmp->attributes, "href");
				break;
			case HT_SCRIPT:
				if((mask_bits & HeadScript || mask_bits == HeadAll)
					&& !tmp->is_end && tmp->attributes && head->script == NULL)
				{
					head->script_lang = _XmHTMLTagGetValue(tmp->attributes,
						"language");

					/* pick up the text, its all in a single element */
					tmp = tmp->next;

					/* sanity check */
					if(!tmp->element)
						break;

					/* copy contents */
					head->script = strdup(tmp->element);
				}
				break;
			case HT_STYLE:
				if((mask_bits & HeadStyle || mask_bits == HeadAll)
					&& !tmp->is_end && tmp->attributes && head->style == NULL)
				{
					head->style_type = _XmHTMLTagGetValue(tmp->attributes,
						"type");

					/* pick up the text, its all in a single element */
					tmp = tmp->next;

					/* sanity check */
					if(!tmp->element)
						break;

					/* copy contents */
					head->style = strdup(tmp->element);
				}
				break;
			default:
				break;
		}
	}
	/* fill in remaining link and meta members */
	if(mask_bits & HeadMeta)
	{
		if(num_meta)
			head->meta = ParseMeta(meta_start, &num_meta);
		head->num_meta = num_meta;
	}
	if(mask_bits & HeadLink)
	{
		if(num_link)
			head->link = ParseLinks(link_start, &num_link);
		head->num_link = num_link;
	}
	/* we found a head */
	return(head_found);
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
_XmHTMLFocusOutCallback(XmHTMLWidget html, XEvent *event)
{
	static XmAnyCallbackStruct cbs;

	cbs.reason = XmCR_LOSING_FOCUS;
	cbs.event = event;
	TkaCallCallbackList(html, losing_focus, &cbs);
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
_XmHTMLFocusInCallback(XmHTMLWidget html, XEvent *event)
{
	static XmAnyCallbackStruct cbs;

	cbs.reason = XmCR_FOCUS;
	cbs.event = event;
	TkaCallCallbackList(html, focus, &cbs);
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
_XmHTMLMotionCallback(XmHTMLWidget html, XEvent *event)
{
	static XmAnyCallbackStruct cbs;

	cbs.reason = XmCR_HTML_MOTIONTRACK;
	cbs.event = event;
	TkaCallCallbackList(html, motion_track, &cbs);
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
_XmHTMLInputCallback(XmHTMLWidget html, XEvent *event)
{
	static XmAnyCallbackStruct cbs;

	cbs.reason = XmCR_INPUT;
	cbs.event = event;
	TkaCallCallbackList(html, input, &cbs);
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
_XmHTMLArmCallback(XmHTMLWidget html, XEvent *event)
{
	static XmAnyCallbackStruct cbs;

	cbs.reason = XmCR_ARM;
	cbs.event = event;

	TkaCallCallbackList(html, arm, &cbs);
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
_XmHTMLImagemapCallback(XmHTMLWidget html, XmHTMLImage *image,
	XmHTMLImagemapCallbackStruct *cbs)
{
	/* set to zero */
	(void)memset(cbs, 0, sizeof(XmHTMLImagemapCallbackStruct));
	cbs->reason = XmCR_HTML_IMAGEMAP;
	cbs->map_name = image->map_url;
	cbs->image_name = image->html_image->url;

	/*****
	* If references to both a server and client-side imagemap
	* are present, the owner of this image has an anchor pointing
	* to the server-side imagemap, and the map_url points
	* to a map in the server-side imagemap.
	*****/
	if(image->owner && image->owner->anchor && *(image->map_url) == '#')
		cbs->map_name = image->owner->anchor->href;

	_XmHTMLDebug(10, ("map.c: _XmHTMLCheckImagemaps, calling "
		"imagemap_callback for imagemap %s used in image %s\n",
		cbs->map_name, cbs->image_name));

	/* trigger the imagemap callback */
	TkaCallCallbackList(html, imagemap, cbs);
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
_XmHTMLObjectCreateCallback(XmHTMLWidget html, XmHTMLExtObj *object)
{
	XmHTMLObjectCallbackStruct cbs;

	(void)memset(&cbs, 0, sizeof(XmHTMLObjectCallbackStruct));

	/* common callback data */
	cbs.reason = XmCR_HTML_OBJECTCREATE;
	cbs.event  = NULL;		/* meaningless for OBJECTCREATE */

	/* parser element data */
	cbs.tag_id        = object->tag->id;
	cbs.tag_user_data = object->tag->user_data;

	/* parent for externally creatable widget */
	cbs.parent    = HTML_ATTR(work_area);

	/* object's widget. Must be created by the user */
	cbs.object    = NULL;

	/* internal id */
	cbs.object_id = object->id;

	/* object display flags */
	cbs.scrollable = True;			/* truly embedded widget */
	cbs.wrap       = True;

	/*****
	* Object reparent flags.
	* If window is non-zero upon return and reparent is set to True,
	* XmHTML will actually *swallow* the window, i.e., it will take control
	* of the application represented by the given window by using the
	* X11R6 XReparentWindow call.
	* The scrollable and wrap fields are ignored when window is non-zero.
	*****/
	cbs.window     = 0;
	cbs.reparent   = False;

	cbs.user_data  = NULL;

	/* object data */
	cbs.attributes = object->attributes;
	cbs.x          = object->x;
	cbs.y          = object->y;
	cbs.width      = object->attributes->width;
	cbs.height     = object->attributes->height;

	/* internal data */
	cbs.is_frame   = HTML_ATTR(is_frame);
	cbs.doit       = True;	/* unused for OBJECTCREATE */

	TkaCallCallbackList(html, object, &cbs);
}

/*****
* Name: 		_XmHTMLFrameDoneCallback
* Return Type: 	void
* Description: 	frame child creation finished notifier
* In:
*	html:		XmHTMLWidget id;
*	frame:		XmHTMLFrameWidget data that is created.
*	widget:		frame child widget id
* Returns:
*	nothing
*****/
void
_XmHTMLFrameDoneCallback(XmHTMLWidget html, XmHTMLFrameWidget *frame,
	Widget widget)
{
	XmHTMLFrameCallbackStruct cbs;

	/* inform user that this frame is finished */
	if(!TkaHasCallback(html, frame_callback))
		return;

	cbs.reason = XmCR_HTML_FRAME;
	cbs.event = NULL;
	cbs.src = frame->src;
	cbs.name = frame->name;
	cbs.html = widget;
	cbs.doit = False;

	/* call the callback list */
	TkaCallCallbackList(html, frame, &cbs);
}

/*****
* Name: 		_XmHTMLFrameDestroyCallback
* Return Type: 	int
* Description: 	frame destruction notifier
* In:
*	html:		XmHTMLWidget id;
*	frame:		frame data;
* Returns:
*	-1: no callback installed;
*	 0: clear frame data but don't destroy the widget;
*	 1: clear and destroy;
*****/
int
_XmHTMLFrameDestroyCallback(XmHTMLWidget html, XmHTMLFrameWidget *frame)
{
	XmHTMLFrameCallbackStruct cbs;

	/* inform user that this frame is about to be destroyed */
	if(!TkaHasCallback(html, frame_callback))
		return(-1);

	cbs.reason = XmCR_HTML_FRAMEDESTROY;
	cbs.event = NULL;
	cbs.src = frame->src;
	cbs.name = frame->name;
	cbs.html = frame->frame;
	cbs.doit = True;

	/* call the callback list */
	TkaCallCallbackList(html, frame, &cbs);

	return((int)cbs.doit);
}

/*****
* Name: 		_XmHTMLFrameCreateCallback
* Return Type: 	void
* Description:  frame creation notifier
* In:
*	html:		XmHTMLWidget id
*	frame:		frame data
* Returns:
*	Widget id of frame to use, NULL otherwise
*****/
Widget
_XmHTMLFrameCreateCallback(XmHTMLWidget html, XmHTMLFrameWidget *frame)
{
	XmHTMLFrameCallbackStruct cbs;

	/* inform user that this frame is about to be created */
	if(!TkaHasCallback(html, frame_callback))
		return(NULL);

	cbs.reason = XmCR_HTML_FRAMECREATE;
	cbs.event = NULL;
	cbs.src = frame->src;
	cbs.name = frame->name;
	cbs.html = NULL;
	cbs.doit = True;

	/* call the callback list */
	TkaCallCallbackList(html, frame, &cbs);

	/* reset/create a new frame widget and return to caller */
	return(_XmHTMLCreateFrame(html, frame, &cbs));
}

/*****
* Name:			_XmHTMLEventProcess
* Return Type: 	Boolean
* Description: 	calls the XmNeventCallback callback resource for the
*				given event.
* In:
*	html:		XmHTMLWidget id;
*	event:		actual event data;
*	ht_event:	private event data;
* Returns:
*	Value of the doc_modified field.
*****/
Boolean
_XmHTMLEventProcess(XmHTMLWidget html, XEVENT *event, HTEvent *ht_event)
{
	XmHTMLEventCallbackStruct cbs;

	cbs.reason    = XmCR_HTML_EVENT;
	cbs.event     = event;
	cbs.type      = ht_event->type;
	cbs.data      = ht_event->data;
	cbs.doc_modified = False;

	TkaCallCallbackList(html, event, &cbs);

	/*****
	* We forbid any document changes for the onLoad and onUnLoad events.
	* If it does happen, we exit as we can not guarantee consistency
	* of the internal data structures.
	*****/
	if((ht_event->type == XmCR_HTML_LOAD ||
		ht_event->type == XmCR_HTML_UNLOAD) && cbs.doc_modified)
	{
		_XmHTMLError(__WFUNC__(html, "_XmHTMLEventProcess"),
			"Fatal: document content modified during processing of the "
			"HTML4.0 %s event.\n    Internal data consistency can no "
			"longer be maintained.",
			ht_event->type == XmCR_HTML_LOAD ? "onLoad" : "onUnLoad");
	}
	return(cbs.doc_modified);
}

/*****
* Name:			_XmHTMLEventFreeDatabase
* Return Type: 	void
* Description: 	destroys all registered events. This routine is called
*				when the current document is being unloaded.
* In:
*	old:		current XmHTMLWidget id;
*	html:		new XmHTMLWidget id;
* Returns:
*	nothing.
*****/
void
_XmHTMLEventFreeDatabase(XmHTMLWidget old, XmHTMLWidget html)
{
	int i;
	for(i = 0; i < ATTR_HTML(old, nevents); i++)
	{
		XmHTMLEventCallbackStruct cbs;
		cbs.reason = XmCR_HTML_EVENTDESTROY;
		cbs.event  = NULL;
		cbs.type   = ATTR_HTML(old, events)[i]->type;
		cbs.data   = ATTR_HTML(old, events)[i]->data;
		cbs.doc_modified = False;

		TkaCallCallbackList(old, event, &cbs);

		/* delete event entry from the event array */
		free(ATTR_HTML(old, events)[i]);
	}

	/* Free the event array itself */
	if(ATTR_HTML(old, events))
		free(ATTR_HTML(old, events));

	ATTR_HTML(old, events) = HTML_ATTR(events) = (HTEvent**)NULL;
	ATTR_HTML(old, nevents) = HTML_ATTR(nevents) = 0;
}
