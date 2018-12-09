#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* events.c : HTML4.0 event routines
*
* This file Version	$Revision$
*
* Creation date:		Fri Nov 14 14:48:28 GMT+0100 1997
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
*
* Copyright (C) 1994-1997 by Ripley Software Development
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
* Revision 1.2  1998/04/27 06:59:11  newt
* tka stuff
*
* Revision 1.1  1998/04/04 06:27:19  newt
* Initial Revision
*
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

/*** Private Variable Declarations ***/
#ifdef NDEBUG
static
#endif /* NDEBUG */
String xmhtml_event_names[XmCR_HTML_USEREVENT] = {
	"onload", "onunload", "onsubmit", "onreset", "onfocus", "onblur",
	"onselect", "onchange", "onclick", "ondblclick", "onmousedown",
	"onmouseup", "onmouseover", "onmousemove", "onmouseout",
	"onkeypress", "onkeydown", "onkeyup"
};

static HTEvent*
storeEvent(XmHTMLWidget html, int type, XtPointer data)
{
#ifdef HTML40_EVENTS_HASHED
	HTEvent *ht_event = NULL;

	if((HashGet(HTML_ATTR(events), (unsigned long)data,
		(unsigned long*)ht_event)))
		return(ht_event);

	/* not found, add this event */
	ht_event = (HTEvent*)malloc(sizeof(HTEvent));

	/* store data */
	ht_event->type = type;
	ht_event->data = data;

	/* insert in event hashtable */
	HashPut(HTML_ATTR(events), (unsigned long)data, (unsigned long)ht_event);

	return(ht_event);

#else
	int i;

	/*
	* check event array to see if we've already got an event with the
	* same data
	*/
	for(i = 0; i < html->html.nevents; i++)
		if(html->html.events[i]->data == data)
			return(html->html.events[i]);

	/*****
	* Not yet in event array. Enlarge or create the event array.
	*****/
	if(html->html.events != NULL)
	{
		html->html.events = (HTEvent**)realloc(html->html.events,
								sizeof(HTEvent*)*(html->html.nevents+1));
	}
	else	/* no event array yet */
		html->html.events  = (HTEvent**)calloc(1, sizeof(HTEvent*));

	/* Add new event entry */
	html->html.events[html->html.nevents] = (HTEvent*)malloc(sizeof(HTEvent));

	/* store it */
	html->html.events[html->html.nevents]->type = type;
	html->html.events[html->html.nevents]->data = data;

	/* keep event counter up to data */
	html->html.nevents++;

	/* and return new event to caller */
	return(html->html.events[html->html.nevents-1]);
#endif
}

static HTEvent*
checkEvent(XmHTMLWidget html, int type, String attributes)
{
	String chPtr;
	XtPointer data;

	if(attributes != NULL &&
		(chPtr = _XmHTMLTagGetValue(attributes,
			xmhtml_event_names[type])) != NULL)
	{
		if((data = html->html.event_proc((Widget)html, chPtr,
			html->html.client_data)) != NULL)
		{
			free(chPtr);
			return(storeEvent(html, type, data));
		}
		free(chPtr);
	}
	return(NULL);
}

/*****
* Name: 		_XmHTMLCheckCoreEvents
* Return Type: 	AllEvents*
* Description: 	checks for the presence of the so-called core events:
*				onClick, onMouse and onKey event classes which are possible
*				for a wide range of HTML objects.
* In:
*	html:		XmHTML widget id;
*	attributes:	string to be checked for event definitions.
*	*mask_...:	define which events have been found. Updated upon return.
* Returns:
*	An event array.
*****/
AllEvents*
_XmHTMLCheckCoreEvents(XmHTMLWidget html, String attributes,
	unsigned long *mask_return)
{
	AllEvents events;
	static AllEvents *events_return = NULL;
	Boolean have_events = False;
	unsigned long mask = 0L;

	/* default mask */
	*mask_return = mask;
	events_return = NULL;

	/* don't do a damn thing if we can't process any scripts or events */
	if(!html->html.event_proc || !html->html.event_callback)
		return(NULL);

	/* reset */
	(void)memset(&events, 0, sizeof(AllEvents));

	/* process all possible core events */
	if((events.onClick = checkEvent(html, XmCR_HTML_CLICK,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_CLICK;
	}
	if((events.onDblClick = checkEvent(html, XmCR_HTML_DOUBLE_CLICK,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_DOUBLECLICK;
	}
	if((events.onMouseDown = checkEvent(html, XmCR_HTML_MOUSEDOWN,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_MOUSEDOWN;
	}
	if((events.onMouseUp = checkEvent(html, XmCR_HTML_MOUSEUP,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_MOUSEUP;
	}
	if((events.onMouseOver = checkEvent(html, XmCR_HTML_MOUSEOVER,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_MOUSEOVER;
	}
	if((events.onMouseMove = checkEvent(html, XmCR_HTML_MOUSEMOVE,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_MOUSEMOVE;
	}
	if((events.onMouseOut = checkEvent(html, XmCR_HTML_MOUSEOUT,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_MOUSEOUT;
	}
	if((events.onKeyPress = checkEvent(html, XmCR_HTML_KEYPRESS,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_KEYPRESS;
	}
	if((events.onKeyDown = checkEvent(html, XmCR_HTML_KEYDOWN,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_KEYDOWN;
	}
	if((events.onKeyUp = checkEvent(html, XmCR_HTML_KEYUP,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_KEYUP;
	}

	/* alloc & copy if we found any events */
	if(have_events)
	{
		events_return = (AllEvents*)malloc(sizeof(AllEvents));
		events_return = (AllEvents*)memcpy(events_return, (const void*)&events,
							sizeof(AllEvents));
		/* update return mask */
		*mask_return = mask;
	}
	return(events_return);
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
AllEvents*
_XmHTMLCheckFormEvents(XmHTMLWidget html, String attributes,
	unsigned long *mask_return)
{
	AllEvents events;
	static AllEvents *events_return = NULL;
	Boolean have_events = False;
	unsigned long mask = 0L;

	*mask_return = 0L;
	events_return = NULL;

	/* don't do a damn thing if we can't process any scripts or events */
	if(!html->html.event_proc || !html->html.event_callback)
		return(NULL);

	/* reset */
	(void)memset(&events, 0, sizeof(AllEvents));

	/* check core events */
	if((events_return =
		_XmHTMLCheckCoreEvents(html, attributes, &mask)) != NULL)
		have_events = True;

	/* process all possible form events */
	if((events.onSubmit = checkEvent(html, XmCR_HTML_SUBMIT,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_SUBMIT;
	}
	if((events.onReset = checkEvent(html, XmCR_HTML_RESET,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_RESET;
	}
	if((events.onFocus = checkEvent(html, XmCR_HTML_FOCUS,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_FOCUS;
	}
	if((events.onBlur = checkEvent(html, XmCR_HTML_BLUR,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_BLUR;
	}
	if((events.onSelect = checkEvent(html, XmCR_HTML_SELECT,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_SELECT;
	}
	if((events.onChange = checkEvent(html, XmCR_HTML_CHANGE,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_CHANGE;
	}

	/* alloc & copy if we found any events */
	if(have_events)
	{
		/* no core events found */
		if(!events_return)
		{
			events_return = (AllEvents*)malloc(sizeof(AllEvents));
			events_return = (AllEvents*)memcpy(events_return,
								(const void*)&events, sizeof(AllEvents));
		}
		else	/* has got core events as well */
		{
			events_return->onSubmit = events.onSubmit;
			events_return->onReset  = events.onReset;
			events_return->onFocus  = events.onFocus;
			events_return->onBlur   = events.onBlur;
			events_return->onSelect = events.onSelect;
			events_return->onChange = events.onChange;
		}
		/* update return mask */
		*mask_return = mask;
	}
	return(events_return);
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
AllEvents*
_XmHTMLCheckBodyEvents(XmHTMLWidget html, String attributes,
	unsigned long *mask_return)
{
	AllEvents events;
	static AllEvents *events_return;
	Boolean have_events = False;
	unsigned long mask = 0L;

	*mask_return = 0L;
	events_return = NULL;

	/* don't do a damn thing if we can't process any scripts or events */
	if(!html->html.event_proc || !html->html.event_callback)
		return(NULL);

	/* reset */
	(void)memset(&events, 0, sizeof(AllEvents));

	/* check core events */
	if((events_return =
		_XmHTMLCheckCoreEvents(html, attributes, &mask)) != NULL)
		have_events = True;

	/* process all possible body events */
	if((events.onLoad = checkEvent(html, XmCR_HTML_LOAD,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_LOAD;
	}
	if((events.onUnload= checkEvent(html, XmCR_HTML_UNLOAD,
		attributes)) != NULL)
	{
		have_events = True;
		mask |= EVENT_UNLOAD;
	}

	/* alloc & copy if we found any events */
	if(have_events)
	{
		/* no core events found */
		if(!events_return)
		{
			events_return = (AllEvents*)malloc(sizeof(AllEvents));
			events_return = (AllEvents*)memcpy(events_return,
						(const void*)&events, sizeof(AllEvents));
		}
		else	/* has got core events as well */
		{
			events_return->onLoad   = events.onLoad;
			events_return->onUnload = events.onUnload;
		}
		/* update return mask */
		*mask_return = mask;
	}
	return(events_return);
}


