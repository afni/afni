#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* object.c : XmHTML embedded object support
*
* This file Version	$Revision$
*
* Creation date:		Thu Jun  4 01:21:06 CEST 1998
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				XmHTML Developers Account
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
* Revision 1.1  2011/06/30 16:10:38  rwcox
* Cadd
*
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "toolkit.h"
#include XmHTMLPrivateHeader

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/

/*** Private Variable Declarations ***/

XmHTMLParserTag **non_std_html_tokens = NULL;
int n_non_std_tokens = 0;

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
objectInitializeTags(void)
{
	n_non_std_tokens = (int)HT_ZTEXT + 1;
	non_std_html_tokens = (XmHTMLParserTag**)calloc(n_non_std_tokens,
							sizeof(XmHTMLParserTag*);

	/*****
	* TODO
	* Initialize all default tokens?
	*****/
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
XmHTMLElementId
objectAddTag(String element, Boolean terminated,
	unsigned long tag_flags, XtPointer user_data)
{
	static XmHTMLParserTag *new_tag;
	int i;

	/* search array of user-defined tokens for an empty slot */
	for(i = (int)HT_ZTEXT; i < n_non_std_tokens; i++)
		if(non_std_html_tokens[i]->id == -1)
			break;

	if(i == n_non_std_html_tokens)
	{
		n_non_std_html_tokens++;
		non_std_html_tokens = (XmHTMLParserTag**)realloc(non_std_html_tokens,
				n_non_std_html_tokens * sizeof(XmHTMLParserTag));
	}
	new_tag = non_std_html_tokens[i];

	new_tag->tag = strndup(element, strlen(element));
	new_tag->terminated = terminated;
	new_tag->tag_flags  = tag_flags;
	new_tag->user_data  = user_data;
	new_tag->id         = i;

	return(new_tag);
}

XmHTMLExtObj*
_XmHTMLObjectCreate(XmHTMLWidget html, String attributes,
	XmHTMLParserTag *ele)
{
	static XmHTMLExtObj *object = NULL;
	static XmHTMLTagAttributes *attr  = NULL;

	/* all possible tag attributes we pre-check */
	static String tag_attr[] = {"classid", "codebase", "data", "src",
		"type", "codetype", "standby", "align", "halign", "valign",
		"height", "width", "border", "hspace", "vspace", "usemap",
		"background", "bgcolor", "href", "name", "events"};

	static int n_tag_attr = sizeof(tag_attr) / sizeof(String);
	int i;
	unsigned long valid_flags = 0L;
	String checked_tags[21];

	/* create a new object */
	object = (XmHTMLExtObj*)calloc(1, sizeof(XmHTMLExtObj));

	/*****
	* Create a new attribute structure. We can safely use malloc
	* as all fields will either get a value or NULL when the attributes
	* have been checked.
	*****/
	attr = (XmHTMLTagAttributes*)malloc(sizeof(XmHTMLTagAttributes));

	/* check the various element flags */
	if(attributes != NULL)
	{
		/* store raw attributes */
		attr->attributes = strdup(attributes);

		/*****
		* No need to reset the checked_tags array. XmHTMLTagGetValue will
		* return NULL if no value could be found.
		*****/

		/* check all possible flags */
		for(i = 0; i < n_tag_attr; i++)
		{
			/* was this attribute defined for this tag? */
			if((ele->flags & (1<<(i+1))) == (1<<(i+1)))
			{
				/* it was, get it's value */
				checked_tags[i] = _XmHTMLTagGetValue(attributes, tag_attr[i]);
				if(checked_tags[i] != NULL)
					valid_flags |= (1<<(i+1));
			}
		}
		/*****
		* Set fields. It is up to the user to check the flags field to see
		* which fields contain valid values.
		*****/
		attr->classid    = checked_tags[0];
		attr->codebase   = checked_tags[1];
		attr->data       = checked_tags[2];
		attr->src        = checked_tags[3];
		attr->type       = checked_tags[4];
		attr->codetype   = checked_tags[5];
		attr->standby    = checked_tags[6];
		attr->align      = checked_tags[7];
		attr->halign     = checked_tags[8];
		attr->valign     = checked_tags[9];
		attr->height     = checked_tags[10] ? atoi(checked_tags[10]) : 0;
		attr->width      = checked_tags[11] ? atoi(checked_tags[11]) : 0;
		attr->border     = checked_tags[12] ? atoi(checked_tags[12]) : 0;
		attr->hspace     = checked_tags[13] ? atoi(checked_tags[13]) : 0;
		attr->vspace     = checked_tags[14] ? atoi(checked_tags[14]) : 0;
		attr->usemap     = checked_tags[15];
		attr->background = checked_tags[16];
		attr->bgcolor    = checked_tags[17];
		attr->href       = checked_tags[18];
		attr->name       = checked_tags[19];

		/*****
		* FIXME
		* Honor HTML4.0 events.
		*****/

		/* forget events for now */
		if(valid_tags & HT_TAG_EVENTS)
		{
			free(checked_tags[20]);
		}
		attr->events     = NULL;
		attr->nevents    = 0;
		valid_flags     &= ~HT_TAG_EVENTS;

		/* store valid flags */
		attr->flags      = valid_flags;
	}
	else
		attr->flags = 0L;	/* no valid attributes found */

	/* store */
	object->attributes = attr;

	/* call create callback */
	if(!(_XmHTMLObjectCreateCallback(html, object));
	{
		/* check all possible flags */
		for(i = 0; i < n_tag_attr; i++)
		{
			/* did this attribute have a value? */
			if((valid_flags & (1<<(i+1))) == (1<<(i+1)))
				free(checked_tags[i]);
		}
		free(attr);
		free(object);
		return(NULL);
	}
	return(object);
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
XmHTMLElementId
XmHTMLObjectDefine(String element, Boolean terminated,
	unsigned long attribute_flags, XtPointer user_data)
{
	static XmHTMLParserTag *new_tag;

	/* sanity */
	if(element == NULL || *element == '\0')
		return(NULL);

	/* initialize external tag array if not already done */
	if(!n_non_std_tokens)
		objectInitializeTags();

	new_tag = objectAddTag(element, terminated, attribute_flags, user_data);

	return(new_tag);
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
XmHTMLObjectStatus
XmHTMLObjectUndefine(XmHTMLElementId element_id)
{
	return(OBJECT_UNIMPLEMENTED);
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
XmHTMLObjectStatus
XmHTMLObjectReconfigure(Widget w, XmHTMLObjectId object_id, int x, int y,
	Dimension width, Dimension height)
{
	return(OBJECT_UNIMPLEMENTED);
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
XmHTMLObjectStatus
XmHTMLObjectShow(Widget w, XmHTMLObjectId object_id, Boolean show)
{
	return(OBJECT_UNIMPLEMENTED);
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
XmHTMLObjectStatus
XmHTMLObjectRaise(Widget w, XmHTMLObjectId object_id)
{
	return(OBJECT_UNIMPLEMENTED);
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
XmHTMLObjectStatus
XmHTMLObjectLower(Widget w, XmHTMLObjectId object_id)
{
	return(OBJECT_UNIMPLEMENTED);
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
XmHTMLObjectId
XmHTMLObjectCreate(XmHTMLElementId element_id)
{
	return(OBJECT_UNIMPLEMENTED);
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
XmHTMLObjectStatus
XmHTMLObjectRegister(Widget w, XmHTMLObjectId object_id)
{
	return(OBJECT_UNIMPLEMENTED);
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
XmHTMLObjectStatus
XmHTMLObjectUnregister(Widget w, XmHTMLObjectId object_id)
{
	return(OBJECT_UNIMPLEMENTED);
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
XmHTMLElementId
XmHTMLObjectGetElementData(Widget w, XmHTMLObjectId object_id)
{
	return(NULL);
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
XmHTMLObjectDestroy(XmHTMLObjectId object_id)
{
	return;
}

/*****
* Code supported by Eric Bello
* Need to integrate this
*****/

#if 0
/*****
* ChangeLog
* $Log$
* Revision 1.1  2011/06/30 16:10:38  rwcox
* Cadd
*
*EB: revision 1.4
*EB: date: 1998/06/24 18:50:16;  author: belloer;  state: Exp;  lines: +31 -15
*EB: Secured destruction phase2 of widget when parent is being destroyed. Secured starting point for list destruction in _XmHTMLUserDestroy(), and changed signature accordingly.
*EB: ----------------------------
*EB: revision 1.3
*EB: date: 1998/06/23 20:17:45;  author: belloer;  state: Exp;  lines: +1 -1
*EB: Remove XtUnmanageChild() call that led to a crash (??)
*EB: ----------------------------
*EB: revision 1.2
*EB: date: 1998/06/22 14:48:59;  author: belloer;  state: Exp;  lines: +22 -10
*EB: Bound some user data into embedded objects
*EB: ----------------------------
*EB: revision 1.1
*EB: date: 1998/06/12 17:13:20;  author: knoplje;  state: Exp;
*EB: inserted XmHTML library
* Revision 1.1  1996/12/19 02:17:10  newt
* Initial Revision
*
*****/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* motif includes */
#include <Xm/Form.h>
#include "toolkit.h"
#include XmHTMLPrivateHeader
#include "XmHTMLfuncs.h"

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/

static Widget createCallback(XmHTMLWidget html, String attributes, String user_tag, void *tag_user_data, void **p_user_data);
static void createdCallback(XmHTMLWidget html, String attributes, String user_tag, void *tag_user_data, Widget w, void **p_user_data);
static int destroyCallback(XmHTMLWidget html, String attributes, String user_tag, void *tag_user_data, Widget w, void *user_data);


/*** Private Variable Declarations ***/

static XmHTMLUser *current_user_word = NULL ;

/********
****** Public Functions
********/
void
_XmHTMLStartUser(void)
{
  current_user_word = NULL ;
}

void
_XmHTMLUserDestroy(XmHTMLWidget html)
{
  XmHTMLUser *next_entry = NULL ;
  XmHTMLUser *entry = html->html.user_data;

  if (!entry)
    return ;

  while(entry)
    {
      int do_it ;

      next_entry = entry->next;

      do_it = destroyCallback(html, NULL, NULL, NULL, entry->w, entry->user_data);

      if (! html->core.being_destroyed && /* if html is being destroyed, this is
					   * too late : the widget has already
					   * been destroyed.
					   */
	  do_it &&
	  entry->w)
	{
	  XtDestroyWidget(entry->w);
	}
      entry->w = (Widget)0 ; /* sanity */
      if (entry->name)
	free(entry->name);
      entry->name = NULL ; /* sanity */
      entry->next = entry->prev = NULL; /* sanity */
      free(entry);
      entry = next_entry;
    }
  html->html.user_data = NULL; /* sanity */
}

XmHTMLUser *
_XmHTMLUserAdd(XmHTMLWidget html, String attributes, String user_tag, void *tag_user_data)
{
	static XmHTMLUser *entry = NULL;
	Widget user_widget = (Widget)0;
	void *user_data = NULL;

	/*****
	* HTML user widgets are children of the workarea.
	* Making them a direct child of the widget itself messes up scrolling.
	*****/
/* 	parent = html->html.work_area; */

	if(attributes == NULL)
		return(NULL);

	/* Request a widget from user */
	user_widget = createCallback(html, attributes, user_tag, tag_user_data, &user_data);

	if (user_widget == (Widget)0)
   	       return(NULL);

	/* Create and initialise a new entry */
	entry = (XmHTMLUser*)malloc(sizeof(XmHTMLUser));
	(void)memset(entry, 0, sizeof(XmHTMLUser));

	/* record information in entry */
	entry->w = user_widget ;
	entry->name = XtName(entry->w);
	entry->align  = _XmHTMLGetImageAlignment(attributes);/* abusive, but so handy ! */
	if (entry->name)
	  entry->name = strdup(entry->name);
	else
	  entry->name = strdup("XmHTMLUserWidget");
	entry->prev = current_user_word;
	entry->next = NULL;
	entry->user_data = user_data;

	/*****
	*  Now, fill the entry with size information
	* and manage the widget
	*****/
	{
	  if(entry->w)
	    {
	      Dimension w = 0, h = 0;
	      Arg args[64];
	      int argc ;

	      /*
	       * Set values. We place this thing completely off screen so
	       * no nasty things happen when this widget is mapped to it's correct
	       * position for the first time.
	       */
	      argc = 0;
	      XtSetArg(args[argc], XmNmappedWhenManaged, False); argc++;
	      XtSetArg(args[argc], XmNx, 0); argc++;
	      XtSetArg(args[argc], XmNy, 0); argc++;
	      XtSetValues(entry->w, args, argc);

	      /* get widget dimensions */
	      XtVaGetValues(entry->w,
			    XmNwidth, &w,
			    XmNheight, &h,
			    NULL);
	      entry->width = w;
	      entry->height = h;

	      XtManageChild(entry->w);
	    }
	  else
	    {
	      entry->width = 0;
	      entry->height = 0;
	    }
	}

	/* update scratch */
	if(html->html.user_data && current_user_word)
	{
		entry->prev = current_user_word;
		current_user_word->next = entry;
	}
	else
		html->html.user_data = entry;

	current_user_word = entry ;

	createdCallback(html, attributes, user_tag, tag_user_data, entry->w, &entry->user_data);

shutdown:
	return entry ;
}

static Widget createCallback(XmHTMLWidget html, String attributes, String user_tag, void *tag_user_data, void **p_user_data)
{
        XmHTMLUserCallbackStruct cbs;

	(void)memset(&cbs, 0, sizeof(XmHTMLUserCallbackStruct));
	cbs.reason = XmCR_HTML_EMBEDCREATE;
	cbs.event = NULL;
	cbs.tag = user_tag;          /* user must strdup() this one */
	cbs.tag_user_data = tag_user_data;
	cbs.attributes = attributes; /* user must strdup() this one */
	cbs.parent = html->html.work_area;
	cbs.user_widget = (Widget)0;
	cbs.is_valid_widget = ! html->core.being_destroyed;
	cbs.is_frame = html->html.is_frame;
	cbs.do_it = True;

	XtCallCallbackList((Widget)html, html->html.user_callback, &cbs);

	if (p_user_data)
	  *p_user_data = cbs.user_data;
	return cbs.user_widget;
}

static void createdCallback(XmHTMLWidget html, String attributes, String user_tag, void *tag_user_data, Widget w, void **p_user_data)
{
        XmHTMLUserCallbackStruct cbs;

	(void)memset(&cbs, 0, sizeof(XmHTMLUserCallbackStruct));
	cbs.reason = XmCR_HTML_EMBED;
	cbs.event = NULL;
	cbs.tag = user_tag;          /* user must strdup() this one */
	cbs.tag_user_data = tag_user_data;
	cbs.attributes = attributes; /* user must strdup() this one */
	cbs.parent = XtParent(w);
	cbs.user_widget = w ;
	cbs.is_valid_widget = ! html->core.being_destroyed;
	cbs.is_frame = html->html.is_frame;
	cbs.do_it = True;
	if (p_user_data)
	  cbs.user_data = *p_user_data;

	XtCallCallbackList((Widget)html, html->html.user_callback, &cbs);

	if (p_user_data)
	  *p_user_data = cbs.user_data;
}

static int destroyCallback(XmHTMLWidget html, String attributes, String user_tag, void *tag_user_data, Widget w, void *user_data)
{
        XmHTMLUserCallbackStruct cbs;

	(void)memset(&cbs, 0, sizeof(XmHTMLUserCallbackStruct));
	cbs.reason = XmCR_HTML_EMBEDDESTROY;
	cbs.event = NULL;
	cbs.tag = user_tag;          /* user must strdup() this one */
	cbs.tag_user_data = tag_user_data;
	cbs.attributes = attributes; /* user must strdup() this one */
	cbs.parent = XtParent(w);
	cbs.user_widget = w ;
	cbs.is_valid_widget = ! html->core.being_destroyed;
	cbs.is_frame = html->html.is_frame;
	cbs.do_it = True;
	cbs.user_data = user_data;

	XtCallCallbackList((Widget)html, html->html.user_callback, &cbs);

	return (cbs.do_it ? 1 : 0);
}

#endif
