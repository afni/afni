#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* plc.c : XmHTML Progressive Loader Context interfacing routines.
*
* This file Version	$Revision$
*
* Creation date:		Thu Jun 12 16:46:34 GMT+0100 1997
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
* Revision 1.6  1998/04/27 07:02:39  newt
* tka stuff
*
* Revision 1.5  1998/04/04 06:28:25  newt
* XmHTML Beta 1.1.3
*
* Revision 1.4  1997/10/23 00:25:14  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.3  1997/08/31 17:37:58  newt
* image dimensions bugfix & XCreatePixmapFromBitmap cast
*
* Revision 1.2  1997/08/30 01:25:52  newt
* All progressively loaded images now share a single GC.
* Added support for on-the-fly scaling.
* The XImage code is now shared with normal image loading.
* Fixed transparency stuff.
* Screen updating now shares code from paint.c: _XmHTMLDrawImage.
*
* Revision 1.1  1997/08/01 12:51:48  newt
* Initial Revision
*
*****/ 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>

#if defined (HAVE_LIBPNG) || defined(HAVE_LIBZ)
#include <zlib.h>
#endif

#if defined(HAVE_LIBJPEG)
#include <jpeglib.h>
#endif

#include "toolkit.h"
#include XmHTMLPrivateHeader

#ifdef HAVE_XCCP_H
#include "XCCP.h"
#endif
#include "plc.h"

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/
static void _PLCInsert(PLC *plc);
static void _PLCRemove(PLC *plc);
static void _PLCRun(PLC *plc);
static void _PLCEndData(PLC *plc);
static void _PLCRecomputeDelays(XmHTMLWidget html);

#ifdef PLC_WORKPROCS
static Boolean _PLCSubCycler(XtPointer call_data);
#else
static void _PLCRecomputeDelays(XmHTMLWidget html);
#endif	/* PLC_WORKPROCS */

/*****
* PLCProc transfer, finalize and low-level init functions for images.
*****/
static void _PLC_IMG_Init(PLC *plc);
static void _PLC_IMG_Transfer(PLC *plc);
static void _PLC_IMG_Finalize(PLC *plc);

static void _PLC_IMG_UpdateScreen(XmHTMLWidget html, XmHTMLImage *image,
	XmHTMLObjectTableElement elePtr, int src_y, Dimension height);
static void _PLC_IMG_UpdateScreenCopies(XmHTMLWidget html, XmHTMLImage *image,
	int src_y, Dimension height);

/*****
* PLCProc transfer, finalize and low-level init functions for documents.
*****/
static void _PLC_DOC_Init(PLC *plc);
static void _PLC_DOC_Transfer(PLC *plc);
static void _PLC_DOC_Finalize(PLC *plc);

/*****
* PLCProc transfer, finalize and low-level init functions for unknown objects.
*****/
static void _PLC_ANY_Init(PLC *plc);
static void _PLC_ANY_Transfer(PLC *plc);
static void _PLC_ANY_Finalize(PLC *plc);

/*** Private Variable Declarations ***/

/***************
***** PLC Support functions
***************/

/*****
* Name:			_PLCDataRequest
* Return Type:	Boolean
* Description:	makes a get_data() request for the current PLC
* In:
*	plc:		current PLC
* Returns:
*	True when request was served, False if not. plc_status is also updated
*	to reflect actual request return code.
*****/
Boolean
_PLCDataRequest(PLC *plc)
{
	int status;
	static XmHTMLPLCStream plc_context;

	if(plc == NULL)
		return(False);

	_XmHTMLDebug(14, ("plc.c: _PLCDataRequest called for %s\n", plc->url));

	/* *very* usefull sanity */
	if(plc->max_in == 0 || plc->max_in < plc->min_in)
		plc->max_in = plc->input_size;

	/*****
	* next_in is the current position in the destination buffer,
	* so we need to make sure that next and max_in do not exceed input 
	* buffer size
	*****/
	if(plc->left + plc->max_in > plc->buf_size)
		plc->max_in = plc->buf_size - plc->left;

	/* yet another sanity */
	if(plc->max_in && plc->min_in >= plc->max_in)
		plc->min_in = 0;

	/* fill stream buffer */
	plc_context.total_in  = plc->total_in;	/* bytes received so far */
	plc_context.min_out   = plc->min_in;	/* minimum no of bytes requested */
	plc_context.max_out   = plc->max_in;	/* maximum no of bytes requested */
	plc_context.user_data = plc->user_data;	/* user_data for this PLC */

	_XmHTMLDebug(14, ("plc.c: _PLCDataRequest, requesting anywhere between %i "
		"and %i bytes\n", plc->min_in, plc->max_in));

	/* get data from the external data stream */
	if((status = plc->sf.get_data(&plc_context, plc->input_buffer)) > 0)
	{
		/* bad copy, issue warning but proceed. */
		if(status < plc->min_in)
		{
			_XmHTMLWarning(__WFUNC__(plc->object->plc_any.owner, "_PLCGetData"),
				XMHTML_MSG_95, status, plc->min_in, "minimally");
		}
		if(status > plc->max_in)
		{
			_XmHTMLWarning(__WFUNC__(plc->object->plc_any.owner, "_PLCGetData"),
				XMHTML_MSG_95, status, plc->max_in, "maximally");
			status = plc->max_in;
		}

		_XmHTMLDebug(14, ("plc.c: _PLCDataRequest, got %i bytes.\n", status));

		/* more than min_in bytes returned, activate plc */
		plc->plc_status = PLC_ACTIVE;

		/* update received byte count */
		plc->total_in += status;

		/*****
		* move data left to the beginning of the buffer (thereby discarding
		* already processed data)
		*****/
		if(plc->left)
			plc->buffer = (Byte*)memmove(plc->buffer,
				plc->buffer + (plc->size - plc->left), plc->left);

		/* append newly received data */
		(void)memcpy(plc->buffer + plc->left, plc->input_buffer, status);

		/* this many bytes are valid in the buffer */
		plc->size = plc->left + status;
		/* reset current ptr position */
		plc->next_in = plc->buffer;

		/* this many bytes left for reading in the buffer */
		plc->left += status;

		return(True);
	}

	/* check return value in most logical (?) order */
	if(status == STREAM_RESIZE)
	{
		/* we have been requested to resize the buffers */
		if(plc_context.max_out <= 0)
		{
			/* this *is* definitly an error */
			_XmHTMLWarning(__WFUNC__(plc->object->plc_any.owner,
				"_PLCDataRequest"), XMHTML_MSG_96);
			return(False);
		}

		/* resize input buffer */
		plc->input_buffer = (Byte*)realloc(plc->input_buffer,
								plc_context.max_out * sizeof(Byte));
		plc->input_size   = plc_context.max_out;
		plc->buf_size     = plc_context.max_out;
		plc->max_in       = plc_context.max_out;

		/*****
		* Always backtrack if we have data left in the current buffer.
		* We make it ourselves easy here and let the user worry about it.
		*****/
		if(plc->left)
		{
			plc->total_in -= plc->left;
			plc->left      = 0;
			plc->next_in   = NULL;
			plc->size      = 0;
		}
		/* resize current data buffer */
		plc->buffer = (Byte*)realloc(plc->buffer, plc->buf_size * sizeof(Byte));

		/* and call ourselves again with the new buffers in place */
		return(_PLCDataRequest(plc->self));
	}

	if(status == STREAM_SUSPEND)
	{
		/* not enough data available, suspend this plc */
		_XmHTMLDebug(14, ("plc.c: _PLCDataRequest, suspending this PLC\n"));
		plc->plc_status = PLC_SUSPEND;
		plc->plc_data_status = STREAM_SUSPEND;
	}
	else if(status == STREAM_END)
	{
		/* all data has been received, terminate plc */
		_XmHTMLDebug(14, ("plc.c: _PLCDataRequest, terminating PLC\n"));
		plc->plc_status = PLC_COMPLETE;
		plc->plc_data_status = STREAM_END;
	}
	else
	{
		/* plc has been aborted */
		_XmHTMLDebug(14, ("plc.c: _PLCDataRequest, aborting PLC\n"));
		plc->plc_status = PLC_ABORT;
		plc->plc_data_status = STREAM_ABORT;
	}
	return(False);
}

/*****
* Name: 		_PLCEndData
* Return Type: 	void
* Description:	calls the end_data() function to signal this PLC is
*				terminating.
* In: 
*	plc:		current PLC
* Returns:
*
*****/
static void
_PLCEndData(PLC *plc)
{
	static XmHTMLPLCStream plc_context;
	PLCImage *any_image = &(plc->object->plc_any_image);
	XmImageInfo *image;

	/* potential memory leak if XmNprogressiveEndProc resource is empty */
	if(plc->sf.end_data == NULL)
	{
		_XmHTMLWarning(__WFUNC__(any_image->owner, "_PLCEndData"),
			XMHTML_MSG_97);
		return;
	}

	_XmHTMLDebug(14, ("plc.c: _PLCEndData called for %s\n", plc->url));

	plc_context.total_in  = plc->total_in;	/* bytes received so far */
	plc_context.min_out   = 0;				/* meaningless */
	plc_context.max_out   = 0;				/* meaningless */
	plc_context.user_data = plc->user_data;	/* user_data for this PLC */

	if(plc->object->type == plcAny || plc->object->type == plcDocument)
	{
		plc->sf.end_data(&plc_context, NULL, XmNONE,
			(plc->plc_status == PLC_COMPLETE));
		return;
	}
	image = any_image->info;
	plc->sf.end_data(&plc_context, (XtPointer)image, XmPLC_IMAGE, 
		(plc->plc_status == PLC_COMPLETE));
}

/*****
* Name: 		_PLCReadOK
* Return Type: 	size_t
* Description: 	copy len bytes to buf from an ImageBuffer
* In: 
*	*plc:		current PLC
*	buf:		data destination
*	len:		no of bytes to copy
* Returns:
*	actual no of bytes read or 0 on failure or end of buffer.
*	This function will make a data request if the current buffer runs out
*	of data.
*****/
size_t 
_PLCReadOK(PLC *plc, Byte *buf, int len)
{
	/* plc->left is the number of bytes left in the input buffer. */
	if(len <= plc->left)
	{
		buf = (Byte*)memcpy(buf, plc->next_in, len);
		/* new position in buffer */
		plc->next_in += len;
		/* this many bytes still available */
		plc->left -= len;
		return(len);
	}
	/* not enough data available, make a request */
	plc->min_in = len - plc->left;
	plc->max_in = PLC_MAX_BUFFER_SIZE;

	if(!(_PLCDataRequest(plc)))
		return(0);	/* suspended, aborted or end of data */

	/* read again */
	return(_PLCReadOK(plc, buf, len));
}

/*****
* Name: 		_PLCGetDataBlock
* Return Type: 	int
* Description: 	gets the next amount of data from the input buffer
* In: 
*	plc:		current PLC
*	buf:		storage buffer, filled upon return.
* Returns:
*	no of bytes copied into buf or 0 when no more data.
*****/
size_t
_PLCGetDataBlock(PLC *plc, Byte *buf)
{
	Byte count = 0;

	if(!_PLCReadOK(plc, &count, 1))
		return(0);

	if(((int)count != 0) && (!_PLCReadOK(plc, buf, (int)count)))
		return(0);

	return((size_t)count);
}

/***************
***** PLC usage functions
***************/

/*****
* Name: 		_XmHTMLPLCCreate
* Return Type: 	PLC*
* Description: 	creates a PLC for the given widget
* In: 
*	html:		XmHTMLWidget id;
*	url:		object for which this PLC is created;
*	priv..:		private data to be registered for this PLC. 
*	type:		type of PLC to be created
* Returns:
*	a newly created PLC.
* Note:
*	Type indicates what type of object should be created. It can be
*	XmNONE, XmPLC_IMAGE or XmPLC_DOCUMENT.
*	The priv_data argument is not the same as the user_data field. It allows
*	an application to attach internal data to a PLC.
****/
PLC*
_XmHTMLPLCCreate(XmHTMLWidget html, XtPointer priv_data, String url, Byte type)
{
	static PLC *plc;

	_XmHTMLDebug(14, ("plc.c: _XmHTMLPLCCreate for %s\n", url));

	plc = (PLC*)malloc(sizeof(PLC));
	memset(plc, 0, sizeof(PLC));

	plc->url      = strdup(url);
	plc->buffer   = (Byte*)malloc(PLC_MAX_BUFFER_SIZE);
	plc->buf_size = (int)PLC_MAX_BUFFER_SIZE;
	plc->size     = (int)0;
	plc->left     = (int)0;
	plc->next_in  = (Byte*)NULL;

	plc->input_buffer = (Byte*)malloc(PLC_MAX_BUFFER_SIZE);
	plc->input_size   = (int)PLC_MAX_BUFFER_SIZE;
	plc->total_in = (int)0;
	plc->max_in   = (int)PLC_MAX_BUFFER_SIZE;
	plc->min_in   = (int)0;

	plc->object   = (PLCObject*)calloc(1, sizeof(PLCObject));
	plc->object->plc_any.owner = html;

	/* a privatly ownded GC for XPutImage to use */
	if(html->html.plc_gc == NULL)
	{
		XGCValues xgc;

		xgc.function = GXcopy;
		xgc.plane_mask = AllPlanes;

		html->html.plc_gc = XCreateGC(XtDisplay((Widget)html),
			(XtIsRealized((Widget)html) ? XtWindow(html->html.work_area) :
				DefaultRootWindow(XtDisplay((Widget)html))), 
				GCFunction | GCPlaneMask , &xgc);
	}

	plc->plc_status      = PLC_ACTIVE;
	plc->plc_data_status = STREAM_OK;

	plc->priv_data   = priv_data;

	/* must be updated by caller */
	plc->user_data   = (XtPointer)NULL;

	/* can be overriden by caller */
	plc->sf.get_data = html->html.get_data;
	plc->sf.end_data = html->html.end_data;

	/* object specific transfer, finalize and low-level init functions */
	if(type == XmPLC_IMAGE)
	{
		plc->object->type = plcAnyImage;
		plc->transfer     = (PLCProc)_PLC_IMG_Transfer;
		plc->finalize     = (PLCProc)_PLC_IMG_Finalize;
		plc->sf.c_new     = (PLCProc)_PLC_IMG_Init;
		plc->obj_set      = False;
	}
	else if(type == XmPLC_DOCUMENT)
	{
		plc->object->type = plcDocument;
		plc->transfer     = (PLCProc)_PLC_DOC_Transfer;
		plc->finalize     = (PLCProc)_PLC_DOC_Finalize;
		plc->sf.c_new     = (PLCProc)_PLC_DOC_Init;
		plc->obj_set      = False;
	}
	else /* type is unknown */
	{
		plc->object->type = plcAny;
		plc->transfer     = (PLCProc)_PLC_ANY_Transfer;
		plc->finalize     = (PLCProc)_PLC_ANY_Finalize;
		plc->sf.c_new     = (PLCProc)_PLC_ANY_Init;
		plc->obj_set      = False;
	}
	/* these *must* be set when the plc->sf.c_new() function is called */
	plc->init        = (PLCProc)NULL;
	plc->destructor  = (PLCProc)NULL;

	plc->initialized = False;

	/*****
	* These must be set by the caller, and can be set by the
	* init method (which happens to be the case for images).
	*****/
	plc->obj_funcs[0] = (PLCProc)NULL;
	plc->obj_funcs[1] = (PLCProc)NULL;
	plc->obj_funcs[2] = (PLCProc)NULL;
	plc->curr_obj_func      = 0;
	plc->obj_funcs_complete = False;

	plc->next_plc = NULL;
	plc->prev_plc = NULL;

	plc->self = plc;

	/* insert it */
	_PLCInsert(plc);

	return(plc);
}

/*****
* Name: 		_PLCInsert
* Return Type:	void
* Description:	insert a PLC in the PLC ringbuffer
* In: 
*	plc:		current PLC
* Returns:
*	nothing.
*****/
static void
_PLCInsert(PLC *plc)
{
	PLC *tmp;
	XmHTMLWidget html = plc->object->plc_any.owner;

	_XmHTMLDebug(14, ("plc.c: _XmHTMLPLCInsert for %s\n", plc->url));

	/* first element, let it point to itself */
	if(html->html.plc_buffer == NULL)
	{
		plc->next_plc = plc->prev_plc = plc;
		html->html.plc_buffer = plc;
		html->html.num_plcs++;
		return;
	}

	/*
	* We already have elements in the plc buffer.
	* The new plc is inserted as the next element of the current PLC so it
	* will get activated immediatly.
	*/
	tmp = html->html.plc_buffer->next_plc;
	tmp->prev_plc = plc;
	plc->next_plc = tmp;
	plc->prev_plc = html->html.plc_buffer;
	html->html.plc_buffer->next_plc = plc;

	/* keep up running PLC count as well */
	html->html.num_plcs++;
}

/*****
* Name: 		_PLCRemove
* Return Type:	void
* Description:	removes a PLC from the PLC ringbuffer and calls the object 
*				destructor method.
* In: 
*	plc:		current PLC
* Returns:
*	nothing.
*****/
static void
_PLCRemove(PLC *plc)
{
	PLC *next, *prev;
	XmHTMLWidget html = plc->object->plc_any.owner;

	_XmHTMLDebug(14, ("plc.c: _PLCRemove for %s\n", plc->url));

	/* call finalize method if this plc was ended prematurely */
	if(plc->obj_funcs_complete == False)
		plc->finalize(plc->self);

	/*****
	* call the end_data() function to signal the user that this PLC is
	* about to be destroyed.
	*****/
	_PLCEndData(plc->self);

	/* call destructor method */
	plc->destructor(plc->self);

	/* now remove it */
	next = plc->next_plc;
	prev = plc->prev_plc;

	/* this is the last PLC in the plc ringbuffer */
	if(next == plc->self || prev == plc->self)
	{
		/* kill the main plc cycler */
		html->html.plc_buffer = NULL;
		_XmHTMLKillPLCCycler(html);
	}
	else
	{
		next->prev_plc = prev;
		prev->next_plc = next;

		/* if this is the current plc, advance the ring buffer */
		if(html->html.plc_buffer == plc->self)
			html->html.plc_buffer = next;
	}

	/*****
	* If no more PLC's are left in the buffer, call the end_data() function
	* to signal the user that we are done loading progressively.
	*****/
	if(html->html.plc_buffer == NULL || html->html.num_plcs == 1)
	{
		if(plc->sf.end_data != NULL)
			plc->sf.end_data(NULL, NULL, XmPLC_FINISHED, True); 
	}

	/* and destroy the PLC itself */
	free(plc->url);
	free(plc->object);			/* all object data */
	free(plc->buffer);			/* current data buffer */
	free(plc->input_buffer);	/* current input buffer */
	free(plc);
	plc = NULL;

	/* keep up running PLC count as well */
	if(html->html.num_plcs)
		html->html.num_plcs--;

	if(html->html.num_plcs == 0 && html->html.plc_buffer != NULL)
		_XmHTMLWarning(__WFUNC__(html, "_PLCRemove"), XMHTML_MSG_98);
}

#ifndef PLC_WORKPROCS
/*****
* Name: 		_PLCRecomputeDelays
* Return Type: 	void
* Description: 	computes a new PLC polling interval.
* In: 
*	html:		XmHTMLWidget id;
* Returns:
*	nothing, but the PLC polling interval is updated upon return.
* Note1:
*	Adjust delay using connection effectiveness (pload below).
*	We use a simple equation: x = -y + 50, where y is the connection
*	effectiveness and x is the required delay adjustment. This is a simple
*	ramp with start point at (50,0) and ending point at (-50,100), which
*	cuts the current delay in two with a pload of 100% (all PLC's active)
*	and doubles it with a pload of 0% (all PLC's suspended).
* Note2:
*	This function is only called when PLC cycling is done using timeouts.
*****/
static void
_PLCRecomputeDelays(XmHTMLWidget html)
{
	int delay, min_delay, max_delay, nplcs, i, nactive, pload, pinc;
	PLC *plc;

	if((nplcs = html->html.num_plcs) == 0)
	{
		html->html.plc_delay = html->html.plc_def_delay;
		return;
	}

	delay     = html->html.plc_delay;
	min_delay = html->html.plc_min_delay;
	max_delay = html->html.plc_max_delay;
	plc       = html->html.plc_buffer;
	
	/* make a guess at the effectiveness of the user's connection */
	for(i = 0, nactive = 0; i < nplcs; i++, plc = plc->next_plc)
		if(plc->plc_status == PLC_ACTIVE)
			nactive++;

	/*****
	* compute new polling interval using the equation mentioned above.
	*****/
	pload = (nactive/(float)nplcs)*100;
	pinc = delay * (-pload+50)/100;
	delay += pinc;

	if(delay < min_delay)
		delay = min_delay;
	if(delay > max_delay)
		delay = max_delay;

	html->html.plc_delay = delay;
}
#endif /* !PLC_WORKPROCS */

/*****
* Name: 		_PLCRun
* Return Type:	void
* Description:	activate a PLC
* In: 
*	plc:		current PLC
* Returns:
*	nothing.
* Note:
*	This is the actual PLC cycler. It will activate routines as necessary.
*****/
static void
_PLCRun(PLC *plc)
{
	XmHTMLWidget html;

	_XmHTMLDebug(14, ("plc.c: _PLCRun for %s\n", plc->url));

	/* see if the object has been set */
	if(plc->obj_set == False)
	{
		plc->sf.c_new(plc->self);
		return;
	}

	/* check if we have been suspended in between calls */
	html = plc->object->plc_any.owner;

	if(html->html.plc_suspended)
	{
		_XmHTMLDebug(14, ("plc.c: _PLCRun, plc suspension, returning.\n"));
		return;
	}

	/* see if we have been initialized */
	if(plc->initialized == False)
	{
		/* call object initializer */
		plc->init(plc->self);
		return;
	}

	/* we have been initialized, call the current obj function */
	plc->obj_funcs[plc->curr_obj_func](plc->self);

	/*****
	* If the plc_status is PLC_ACTIVE or PLC_COMPLETE when the above
	* function returns, call the object transfer function as well.
	*****/
	if(plc->plc_status == PLC_ACTIVE || plc->plc_status == PLC_COMPLETE)
		plc->transfer(plc->self);

	/*****
	* if the object functions are finished, call the finalize method
	* as well.
	*****/
	if(plc->obj_funcs_complete == True)
	{
		plc->finalize(plc->self);
		plc->plc_status = PLC_COMPLETE;
	}
}

#ifdef PLC_WORKPROCS
/*****
* Name: 		_PLCSubCycler
* Return Type: 	Boolean
* Description: 	PLC cycling engine when work procedures are to be used.
* In: 
*	call_data:	data registered for this function.
* Returns:
*	False when this function should be called again by X (as long as
*	there are PLC's in the list), and True when processing is complete.
*	This function is actually of type XtWorkProc.
*****/
static Boolean
_PLCSubCycler(XtPointer call_data)
{
	XmHTMLWidget html = (XmHTMLWidget)call_data;
	PLC *plc = html->html.plc_buffer;

	/* return if we haven't got any PLC's installed */
	if(plc == NULL || html->html.plc_suspended)
		return(True);

	_XmHTMLDebug(14, ("plc.c: _PLCSubCycler for %s\n", plc->url));

	html->html.plc_proc_id = None;

	/*****
	* As this is called thru a work procedure, we don't have to concern
	* ourselves with the speedups in the timeout variant. It will be called
	* *much* sooner.
	*****/
	switch(plc->plc_status)
	{
		case PLC_SUSPEND:
			/*****
			* _PLCDataRequest failed on previous call, skip it this time
			* to give the connection a bit more time and set plc_status so
			* it will get activated the next time this plc is called.
			*****/
			plc->plc_status = PLC_ACTIVE;
			/* move to next plc */
			html->html.plc_buffer = plc->next_plc;
			break;
			
		case PLC_ACTIVE:
			_PLCRun(plc->self);
			/* move to next plc */
			html->html.plc_buffer = plc->next_plc;
			break;

		case PLC_COMPLETE:
		case PLC_ABORT:
			/* _PLCRemove will advance to the next PLC by itself */
			_PLCRemove(plc->self);
			break;

		default:
			_XmHTMLWarning(__WFUNC__(html, "_PLCSubCycler"),
				XMHTML_MSG_99, plc->plc_status);
			/* kill it next time it's called */
			plc->plc_status = PLC_ABORT;
			/* move to next plc */
			html->html.plc_buffer = plc->next_plc;
			break;
	}

	/* continue processing if we have any plc's left */
	if(html->html.plc_buffer != NULL)
		return(False);

	return(True);
}
#endif	/* PLC_WORKPROCS */

/*****
* Name: 		_XmHTMLPLCCyler
* Return Type: 	void
* Description: 	main PLC cycling engine.
* In: 
*	call_data:	data registed for this function.
*	proc_id:	unused;
* Returns:
*	nothing.
* Note:
*	When PLC_WORKPROCS was defined during compilation, this routine starts
*	up a subcycler which will do the real PLC processing using work procedures.
*	The default is to use a dynamic polling interval using timeouts (causing
*	this function to become a XtTimerCallbackProc).
*	This function is called only *once* from within the main XmHTML code:
*	when new text has been set into the widget.
*****/
/*ARGSUSED*/
void
_XmHTMLPLCCycler(XtPointer call_data, XtIntervalId *proc_id)
{
	XmHTMLWidget html = (XmHTMLWidget)call_data;
	PLC *plc;
	int nplcs, i;

	/*****
	* Return if we haven't got any PLC's installed or if progressive
	* image loading has been suspended.
	*****/
	if((plc = html->html.plc_buffer) == NULL || html->html.plc_suspended)
		return;

	/* make sure we aren't grabbing anything (total server lockup!) */
	XUngrabPointer(XtDisplay((Widget)html), CurrentTime);

	nplcs = html->html.num_plcs;

#ifdef PLC_WORKPROCS
	_XmHTMLDebug(14, ("plc.c: _XmHTMLPLCCycler, starting subCycler\n"));

	html->html.plc_proc_id = (XtWorkProcId)
		XtAppAddWorkProc(XtWidgetToApplicationContext((Widget)html),
			(XtWorkProc)_PLCSubCycler, (XtPointer)html);
#else
	_XmHTMLDebug(14, ("plc.c: _XmHTMLPLCCycler for %s\n", plc->url));

	html->html.plc_proc_id = None;

	switch(plc->plc_status)
	{
		case PLC_SUSPEND:
			/*****
			* _PLCDataRequest failed on previous call, skip it this time
			* to give the connection a bit more time and set plc_status so
			* it will get activated the next time this plc is called.
			*****/
			plc->plc_status = PLC_ACTIVE;

			/* move to next plc */
			html->html.plc_buffer = plc->next_plc;

			/*****
			* To prevent all to much slowdown (this routine is called by
			* a timer), we cycle thru the full list of installed plc's to see
			* if we have an active one. If we do, we fall thru this case and
			* activate it. While checking, we also activate any suspended plc's
			* so they will get called the next time this routine is activated.
			*****/
			for(i = 0; i < nplcs - 1; i++)
			{
				plc = html->html.plc_buffer;
				if(plc->plc_status == PLC_ACTIVE)
					break;
				else	/* activate this plc */
					plc->plc_status = PLC_ACTIVE;
				html->html.plc_buffer = plc->next_plc;
			}
			/* all plc's suspended, break out */
			if(plc->plc_status != PLC_ACTIVE)
				break;

			/* we have found an active plc, fall through */
			
		case PLC_ACTIVE:
			_PLCRun(plc->self);

			/*****
			* Speedup: if this plc has finished, remove it right away,
			* there's no need to do this on the next call to this routine as
			* the finalizing and removal of this plc does not involve any
			* data requests.
			*****/
			if(plc->plc_status == PLC_COMPLETE || plc->plc_status == PLC_ABORT)
				_PLCRemove(plc->self);
			else	/* move to next plc */
				html->html.plc_buffer = plc->next_plc;
			break;

		case PLC_COMPLETE:
		case PLC_ABORT:
			/* _PLCRemove will advance to the next PLC by itself */
			_PLCRemove(plc->self);
			break;

		default:
			_XmHTMLWarning(__WFUNC__(html, "_XmHTMLPLCCycler"),
				XMHTML_MSG_99, plc->plc_status);
			/* kill it next time it's called */
			plc->plc_status = PLC_ABORT;
			/* move to next plc */
			html->html.plc_buffer = plc->next_plc;
			break;
	}

	/* adjust PLC intervals */
	_PLCRecomputeDelays(html);

	/* and re-instate ourselves if we have any plc's left */
	if(html->html.plc_buffer != NULL)
	{
		XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)html),
			html->html.plc_delay, (XtTimerCallbackProc)_XmHTMLPLCCycler,
			(XtPointer)html);
	}
#endif /* !PLC_WORKPROCS */
}

/*****
* Name: 		_XmHTMLKillPLCCycler
* Return Type: 	void
* Description: 	kills all outstanding PLC procedures.
* In: 
*	html:		XmHTMLWidget id for which to kill all PLC's
* Returns:
*	nothing, but the list of PLC's of the given XmHTMLWidget has been cleared
*	upon return.
* Note:
*	This function is called when the current document is discarded (by
*	loading a new document or destroying the widget).
*****/
void
_XmHTMLKillPLCCycler(XmHTMLWidget html)
{
	PLC *plc = html->html.plc_buffer;

	_XmHTMLDebug(14, ("plc.c: _XmHTMLKillPLCCycler\n"));

	/* kill any outstanding plc cycler timeouts */
	if(html->html.plc_proc_id != None)
	{
		html->html.plc_suspended = True;

#ifdef PLC_WORKPROCS
		XtRemoveWorkProc((XtWorkProcId)html->html.plc_proc_id);
#else
		XtRemoveTimeOut(html->html.plc_proc_id);
#endif	/* PLC_WORKPROCS */
		html->html.plc_proc_id = None;
	}

	/* reset PLC timeout to the default value */
	html->html.plc_delay = html->html.plc_def_delay;

	if(plc == NULL)
	{
		/* restore defaults */
		html->html.num_plcs      = 0;
		html->html.plc_suspended = True;
		html->html.plc_delay     = html->html.plc_def_delay;
		return;
	}

	/* now remove all outstanding plc's */
	while(html->html.plc_buffer != NULL)
	{
		/* abort all outstanding PLC's */
		plc = html->html.plc_buffer;

		_XmHTMLDebug(14, ("plc.c: _XmHTMLKillPLCCycler, aborting %s\n",
			plc->url));

		plc->plc_status = PLC_ABORT;
		_PLCRemove(plc);
	}
	/* restore defaults */
	html->html.num_plcs      = 0;
	html->html.plc_suspended = True;
	html->html.plc_delay     = html->html.plc_def_delay;

	/* free the plc_gc if it's still here */
	if(html->html.plc_gc)
	{
		XFreeGC(XtDisplay((Widget)html), html->html.plc_gc);
		html->html.plc_gc = (GC)NULL;
	}
}

/*****
* Object-specific routines.
* There are three sets of them, each with three functions:
*	_PLC_IMG class: image PLC functions;
*	_PLC_DOC class: document PLC functions;
*	_PLC_ANY class: unknown PLC functions;
* The three functions are:
*	_Init:     function that performs initialization of the object for which
*              a PLC is to be used. This may involve getting more
*              object-specific data. When this function finishes, it should
*              set the obj_set field to True.
*	_Transfer: function that needs to perform *intermediate* transfer of
*              object-specific data to the final destination;
*	_Finalize: function that needs to perform *final* transfer of
*              object-specific data to the final destination;
*****/

/***************
***** Image PLC object and supporting functions
***************/

/*****
* Name: 		_PLC_IMG_Init
* Return Type: 	void
* Description: 
* In: 
*	plc:		current PLC
* Returns:
*	nothing. Upon return, the object field of the current PLC is updated to
*	reflect the type of image that is to be loaded (includes image-specific
*	object functions).
*****/
static void
_PLC_IMG_Init(PLC *plc)
{
	Byte obj_type = plcAnyImage, img_type = IMAGE_UNKNOWN;
	Byte magic[10];
	static Byte png_magic[8] = {137, 80, 78, 71, 13, 10, 26, 10};

	_XmHTMLDebug(14, ("plc.c: _PLC_IMG_Init for %s\n", plc->url));

	/* get first 10 bytes to determine the image type */
	plc->min_in = 10;
	plc->max_in = PLC_MAX_BUFFER_SIZE;

	/*****
	* We *need* to know the image type before we can start doing
	* anything.
	*****/
	if(!(_PLCDataRequest(plc)))
		return;

	memcpy(magic, plc->buffer, 10);

	/* check image types we known of. Do most (?) logical order */
	if(!(strncmp((char*)magic, "GIF87a", 6)) || 
		!(strncmp((char*)magic, "GIF89a", 6)))
	{
		obj_type = plcGIF;
		img_type = IMAGE_GIF;
		plc->init         = _PLC_GIF_Init;
		plc->destructor   = _PLC_GIF_Destructor;
		plc->obj_funcs[0] = _PLC_GIF_ScanlineProc;
		plc->object->plc_gif_image.info = (XmImageInfo*)plc->priv_data;
	}
	/* compatible gif */
	else if(!(strncmp((char*)magic, "GZF87a", 6)) || 
		!(strncmp((char*)magic, "GZF89a", 6)))
	{
#if defined (HAVE_LIBPNG) || defined (HAVE_LIBZ)
		obj_type = plcGZF;
		img_type = IMAGE_GZF;
		plc->init         = _PLC_GZF_Init;
		plc->destructor   = _PLC_GZF_Destructor;
		plc->obj_funcs[0] = _PLC_GZF_ScanlineProc;
		plc->object->plc_gzf_image.info = (XmImageInfo*)plc->priv_data;
#endif /* HAVE_LIBPNG || HAVE_LIBZ */
	}
	else if(magic[0] == 0xff && magic[1] == 0xd8 && magic[2] == 0xff)
	{
#ifdef HAVE_LIBJPEG
		obj_type = plcJPEG;
		img_type = IMAGE_JPEG;
		plc->init         = _PLC_JPEG_Init;
		plc->destructor   = _PLC_JPEG_Destructor;
		plc->obj_funcs[0] = _PLC_JPEG_ScanlineProc;
		plc->object->plc_jpeg_image.info = (XmImageInfo*)plc->priv_data;
#endif /* HAVE_LIBJPEG */
	}
	else if(!(memcmp(magic, png_magic, 8)))
	{
#ifdef HAVE_LIBPNG

/*** internal configuration define, don't define this yourself ***/
#ifdef PLC_PNG
		obj_type = plcPNG;
		img_type = IMAGE_PNG;
		plc->init         = _PLC_PNG_Init;
		plc->destructor   = _PLC_PNG_Destructor;
		plc->obj_funcs[0] = _PLC_PNG_ScanlineProc;
		plc->object->plc_png_image.info = (XmImageInfo*)plc->priv_data;
#endif /* PLC_PNG */

#endif /* HAVE_LIBPNG */
	}
	else if(!(strncmp((char*)magic, "/* XPM */", 9)))
	{
		obj_type = plcXPM;
		img_type = IMAGE_XPM;
		plc->init         = _PLC_XPM_Init;
		plc->destructor   = _PLC_XPM_Destructor;
		plc->obj_funcs[0] = _PLC_XPM_ScanlineProc;
		plc->object->plc_xpm_image.info = (XmImageInfo*)plc->priv_data;
	}
	else if(!(strncmp((char*)magic, "#define", 7)) ||
		(magic[0] == '/' && magic[1] == '*'))
	{
		obj_type = plcXBM;
		img_type = IMAGE_XBM;
		plc->init         = _PLC_XBM_Init;
		plc->destructor   = _PLC_XBM_Destructor;
		plc->obj_funcs[0] = _PLC_XBM_ScanlineProc;
		plc->object->plc_xbm_image.info = (XmImageInfo*)plc->priv_data;
	}

	/* check if we got an image */
	if(obj_type == plcAnyImage)
	{
		_XmHTMLWarning(__WFUNC__(plc->object->plc_any.owner, "_PLC_IMG_Init"),
			XMHTML_MSG_100, plc->url);
		plc->plc_status = PLC_ABORT;
		return;
	}
	plc->object->plc_any.type = obj_type;
	plc->object->plc_any_image.info->type = img_type;
	plc->obj_set = True;
	return;
}

/*****
* Name: 		_PLC_IMG_Transfer
* Return Type: 	void
* Description: 	intermediate image transfer function
* In: 
*	plc:		current PLC.
* Returns:
*	nothing.
* Note:
*	When this routine is called for the first time, it initializes all common
*	image fields (XImage, pixmap, color arrays, clipmask, etc...).
*
*	The actual image composition is split in six parts:
*	1. color counting; the numbers of colors used by the new chunk of data is
*	   counted, and the RGB arrays for these colors are filled;
*	2. data pixelization; the new chunk of raw data is mapped to the pixel
*	   values assigned in the previous step;
*	3. color allocation; if new colors are to be allocated that's done now;
*	4. XImage updating; scanlines represented by the new chunk of data are
*	   added to the existing scanlines already present in the XImage;
*	5. Pixmap updating; the newly added scanlines are copied into the
*	   destination drawable (a Pixmap);
*	6. Display updating: the updated portion of the pixmap is copied to screen.
*****/
static void
_PLC_IMG_Transfer(PLC *plc)
{
	PLCImage *any_image = &(plc->object->plc_any_image);
	XmHTMLImage *image  = any_image->image;
	XmImageInfo *info   = any_image->info;
	XmHTMLWidget html   = any_image->owner;
	int width, height, lo, hi;
	int col_cnt;	/* no of colors in image, accumulated */
	int npixels;	/* no of allocated pixel values, accumulated */
	Boolean pixels[XmHTML_MAX_IMAGE_COLORS];
	register int i;
	Byte *ptr, *data;
	int ypos, nlines;
	Display *dpy  = XtDisplay((Widget)html);

	_XmHTMLDebug(14, ("plc.c: _PLC_IMG_Transfer for %s\n", plc->url));

	if(info == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "_PLC_IMG_Transfer"),
			XMHTML_MSG_101, plc->url);
		plc->plc_status = PLC_ABORT;
		return;
	}

	/* don't do a thing if we haven't received any new data */
	if(any_image->prev_pos == any_image->data_pos)
	{
		_XmHTMLDebug(14, ("plc.c: _PLC_IMG_Transfer end, no new data to "
			"process\n"));
		return;
	}

	/* no HTML image stored yet, go pick it up */
	if(image == NULL)
	{
		Window win;
		Boolean need_redisplay = False;

		for(image = html->html.images; image != NULL &&
			image->html_image != info; image = image->next);
		if(image == NULL)
		{
			_XmHTMLWarning(__WFUNC__(html, "_PLC_IMG_Transfer"),
				XMHTML_MSG_102, plc->url);
			plc->plc_status = PLC_ABORT;
			return;
		}
		image->options |= IMG_PROGRESSIVE;

		any_image->image = image;

		/* if we are realized we have a window */
		if(XtIsRealized((Widget)html))
			win = XtWindow(html->html.work_area);
		else
			win = DefaultRootWindow(dpy);

		/* store original image dimensions */
		info->swidth  = any_image->width;
		info->sheight = any_image->height;

		/*****
		* See if any dimensions were specified on the <IMG> element
		* or if we may not perform scaling.
		*****/
		if(!ImageHasDimensions(image) || !ImageInfoScale(info))
		{
			/* store used image dimensions */
			info->width  = image->width  = any_image->width;
			info->height = image->height = any_image->height;

			/*****
			* keep requested dimensions if we have them. Set to real
			* image dimensions otherwise.
			*****/
			if(!ImageHasDimensions(image))
			{
				image->swidth  = image->width;
				image->sheight = image->height;
			}
		}
		/* we have dimensions and we may scale */
		else
		{
			/* store used image dimensions */
			info->width  = image->width  = image->swidth;
			info->height = image->height = image->sheight;

			/* check see if we really need to scale */
			if(image->swidth != any_image->width ||
				image->sheight != any_image->height)
			{
				/* we need to scale */
				any_image->is_scaled = True;
				any_image->scaled_data =
					(Byte*)malloc(image->swidth * image->sheight);
			}
		}

		/*
		* Update imageWord dimensions as well.
		* owner can be NULL if this is the body image.
		*/
		if(image->owner != NULL && image->owner->words != NULL &&
			image->owner->words[0].image == image)
		{
			/*
			* If the owning word had it's dimensions wrong (not set before)
			* we need to recompute the screen layout.
			*/
			if(image->owner->words[0].width != image->width ||
				image->owner->words[0].height != image->height)
			{

				/* store new image dimensions */
				image->owner->words[0].width = image->width;
				image->owner->words[0].height = image->height;

				/* redo screen layout as it will be incorrectly by now */
				need_redisplay = True;
			}
		}

		/* allocate pixmaps */
		if((any_image->pixmap = XCreatePixmap(dpy, win, image->width,
			image->height, html->html.xcc->visualInfo->depth)) == None)
		{
			_XmHTMLWarning(__WFUNC__(html, "_PLC_IMG_Transfer"),
				XMHTML_MSG_66, plc->url);
			plc->plc_status = PLC_ABORT;
			return;
		}
		/*****
		* pre-tile the pixmap with the body image (if this isn't the body
		* image of course)
		*****/
		if(html->html.body_image && !ImageIsBackground(html->html.body_image)
			&& !ImageDelayedCreation(html->html.body_image) &&
			BodyImageLoaded(html->html.body_image->html_image))
		{
			XGCValues values;
			unsigned long valuemask;
			int tile_width, tile_height, x_dist, y_dist, ntiles_x, ntiles_y;
			int x_offset, y_offset, tsx, tsy, xs, ys;

			tile_width  = html->html.body_image->width;
			tile_height = html->html.body_image->height;

			/* compute correct image offsets */
			xs = image->owner->words[0].x - html->html.scroll_x;
			ys = image->owner->words[0].y - html->html.scroll_y;

			x_dist = html->html.scroll_x + xs;
			y_dist = html->html.scroll_y + ys;

			ntiles_x = (int)(x_dist/tile_width);
			ntiles_y = (int)(y_dist/tile_height);

			x_offset = x_dist - ntiles_x * tile_width;
			y_offset = y_dist - ntiles_y * tile_height;

			tsx = xs - x_offset;
			tsy = ys - y_offset;

			valuemask = GCTile|GCFillStyle|GCTileStipXOrigin|GCTileStipYOrigin;
			values.fill_style = FillTiled;
			values.tile = html->html.body_image->pixmap;
			values.ts_x_origin = tsx;
			values.ts_y_origin = tsy;

			XChangeGC(dpy, html->html.bg_gc, valuemask, &values);

			/* a plain fillrect will redraw the background portion */
			XFillRectangle(dpy, any_image->pixmap, html->html.bg_gc, 0, 0,
				image->width, image->height);
		}
		else
		{
			/* fill it with the current background for now */
			XSetForeground(dpy, html->html.gc, html->html.body_bg);
			XFillRectangle(dpy, any_image->pixmap, html->html.gc, 0, 0,
				image->width, image->height);
			XSetForeground(dpy, html->html.gc, html->html.body_fg);
		}

		image->pixmap = any_image->pixmap;

		/* allocate fully transparent clipmask */
		if(any_image->transparency == XmIMAGE_TRANSPARENCY_BG)
		{
			int clipsize = 0;
			info->options |= XmIMAGE_CLIPMASK;

			/* compute amount of data required for this clipmask */
			i = image->width;

			/* make it byte-aligned */
			while((i % 8))
				i++;

			/* this many bytes on a row */
			i /= 8;

			/* size of clipmask */
			clipsize = i * image->height;

			/* raw clipmask data */
			any_image->clip_data = (Byte*)calloc(clipsize, sizeof(Byte));
			/* fully transparent clipmask */
			any_image->clipmask = XCreatePixmapFromBitmapData(dpy, win, 
				(char*)any_image->clip_data, image->width, image->height,
				1, 0, 1);
		}
		/* destination clipmask */
		image->clip = any_image->clipmask;
		/* temporary clipmask data */
		info->clip  = any_image->clip_data;

		/* allocate image RGB values */
		info->options |= XmIMAGE_RGB_SINGLE;
		info->reds   = (Dimension*)calloc(3 * any_image->cmapsize,
						sizeof(Dimension));
		info->greens = info->reds   + any_image->cmapsize;
		info->blues  = info->greens + any_image->cmapsize;

		/* reset used colors array */
		for(i = 0; i < XmHTML_MAX_IMAGE_COLORS; i++)
		{
			any_image->used[i] = 0;
			any_image->xcolors[i] = 0L;
		}

		any_image->nused = 1;

		/* update all copies of this image */
		if(image->child)
			_XmHTMLImageUpdateChilds(image);

		/* create a working XImage for this plc */
		if(any_image->ximage == NULL)
		{
			any_image->ximage = _XmHTMLCreateXImage(html, html->html.xcc,
				image->width, image->height, plc->url);

			if(any_image->ximage == NULL)
			{
				plc->plc_status = PLC_ABORT;
				return;
			}
		}

		/* redo screen layout as it will be incorrect by now */
		if(need_redisplay)
			XmHTMLRedisplay((Widget)html);
	}

	/*****
	* Always get used image dimensions. 
	*****/
	width  = image->width;
	height = image->height;

	/*****
	* Step 1: color usage
	*****/

	/* last known processed data */
	ptr = any_image->data + any_image->prev_pos;
	/* last known index of allocated colors */
	col_cnt = any_image->nused;

	/* store pixel indices */
	for(i = any_image->prev_pos; i < any_image->data_pos; i++, ptr++) 
	{
		if(any_image->used[(int)*ptr] == 0) 
		{
			any_image->used[(int)*ptr] = col_cnt;
			col_cnt++;
		}
	}
	col_cnt--;

	/*****
	* Now go and fill the RGB arrays. Only do this if the new chunk of data
	* contains new colors.
	*****/
	if(any_image->nused != col_cnt+1)
	{
		memset(pixels, 0, XmHTML_MAX_IMAGE_COLORS*sizeof(Boolean));
		for(i = 0; i < XmHTML_MAX_IMAGE_COLORS; i++) 
		{
			int indx;

			if(any_image->used[i] != 0) 
			{
				indx = any_image->used[i] - 1;
				pixels[indx] = True;
				info->reds[indx]   = any_image->cmap[i].red;
				info->greens[indx] = any_image->cmap[i].green;
				info->blues[indx]  = any_image->cmap[i].blue;
			}
		}
	}

	/*****
	* Step 2a: image scaling (optional)
	*****/
	if(any_image->is_scaled)
	{
		Byte *img_data, *ilptr, *ipptr, *elptr, *epptr;
		int ix, iy, ex, ey, src_w, src_h;

		/* get ptr to scaled image data */
		data = any_image->scaled_data;
		img_data = any_image->data;

		/* get real image dimensions */
		src_w = any_image->width;
		src_h = any_image->height;

		/* initialize scaling */
		elptr = epptr = data;

		/* starting scanline index */
		ey = any_image->sc_start/width;

		/* scaling is done from top to bottom, left to right */
		for(; ey < height; ey++, elptr += width)
		{
			/* vertical pixel skip */
			iy = (src_h * ey) / height;
			epptr = elptr;
			ilptr = img_data + (iy * src_w);
			/* don't overrun */
			if(iy*src_w > any_image->data_pos)
				break;
			for(ex = 0; ex < width; ex++, epptr++)
			{
				/* horizontal pixel skip */
				ix = (src_w * ex) / width;
				ipptr = ilptr + ix;
				*epptr = *ipptr;
			}
		}
		/* update scaled data end position */
		any_image->sc_end = ey*width;
	}

	/*****
	* Step 2b: clipmask creation (optional)
	* We just redo the entire image.
	*****/
	if(any_image->transparency == XmIMAGE_TRANSPARENCY_BG)
	{
		int j, bcnt;
		Byte *cptr = any_image->clip_data;
		Window win;

		if(any_image->is_scaled)
		{
			ptr = any_image->scaled_data;
			/* last known processed scanline */
			lo  = any_image->sc_start/width;
			/* maximum scanline on this pass */
			hi  = any_image->sc_end/width;
		}
		else
		{
			ptr = any_image->data;
 			hi = (any_image->data_pos)/width;
			lo = (any_image->prev_pos)/width;
		}

		/* pick up were we left off. Saves prev_pos conditionals */
		for(i = 0; i < lo; i++)
		{
			for(j = 0, bcnt = 0; j < width; j++)
			{
				if((bcnt % 8) == 7 || j == (width-1))
					cptr++;
				bcnt++;
				ptr++;
			}
		}

		/* process next amount of data */
		for(i = lo; i < hi; i++)
		{
			for(j = 0, bcnt = 0; j < width; j ++)
			{
				if(*ptr != any_image->bg_pixel) 
					*cptr += bitmap_bits[(bcnt % 8)];
				if((bcnt % 8) == 7 || j == (width-1))
					cptr++;
				bcnt++;
				ptr++;
			}
		}
		/* destroy existing bitmap */
		if(any_image->clipmask != None)
			XFreePixmap(dpy, any_image->clipmask);

		/* if we are realized we have a window */
		if(XtIsRealized((Widget)html))
			win = XtWindow(html->html.work_area);
		else
			win = DefaultRootWindow(dpy);

		/* create new one */
		any_image->clipmask = XCreatePixmapFromBitmapData(dpy, win, 
				(char*)any_image->clip_data, width, height, 1, 0, 1);
		/* save it */
		image->clip = any_image->clipmask;

		/* update child copies */
		if(image->child)
			_XmHTMLImageUpdateChilds(image);
	}

	/*****
	* Step 3: image pixelization.
	* Replaces each pixel value in the decoded image data with the
	* indices in our own pixel array.
	* Needs to be done after clipmask creation as this modifies the image
	* data.
	*****/

	/* last known processed data */
	ptr = any_image->data + any_image->prev_pos;
	for(i = any_image->prev_pos; i < any_image->data_pos; i++) 
	{
		*ptr = (Byte)(any_image->used[(int)*ptr] - 1);
		ptr++;
	}

	/*****
	* Step 4: color allocation.
	* Only allocate colors if the new chunk of data contains colors that
	* haven't been allocated yet.
	*****/
	if(any_image->nused != col_cnt+1)
	{
		_XmHTMLDebug(14, ("plc.c: _PLC_IMG_Transfer, allocating %i additional "
			"colors (%i total)\n", col_cnt - any_image->nused + 1, col_cnt));

		npixels = image->npixels;
		XCCGetPixelsIncremental(html->html.xcc, info->reds, info->greens,
			info->blues, any_image->cmapsize, pixels, any_image->xcolors,
			&npixels);

		_XmHTMLDebug(14, ("plc.c: _PLC_IMG_Transfer, after allocation, npixels "
			": %i\n", npixels));

		/* update used color count */
		any_image->nused = col_cnt+1;
		image->npixels = npixels;
	}

	/*****
	* Step 5: XImage updating.
	* We have our colors, now do the ximage. If the image is being scaled,
	* use the pre-scaled image data, else use the real image data.
	*****/
	if(any_image->is_scaled)
	{
		data = any_image->scaled_data + any_image->sc_start;
		lo = any_image->sc_start;
		hi = any_image->sc_end;
	}
	else
	{
		data = any_image->data + any_image->prev_pos;
		lo = any_image->prev_pos;
		hi = any_image->data_pos;
	}

	_XmHTMLFillXImage(html, any_image->ximage, html->html.xcc, data,
		any_image->xcolors, &lo, &hi);

	/*****
	* XImage updated, make all positions aligned on completed scanline
	* boundaries.
	*****/
	any_image->prev_pos = (any_image->data_pos/any_image->width) *
		any_image->width;

	/* current scanline */
	ypos = lo/width;

	/* no of scanlines added on this pass */
	nlines = (hi - lo)/width;

	/* check if we aren't exceeding height of the image */
	my_assert((ypos + nlines <= height));

	/*****
	* Step 6: destination updating.
	* XImage data processed, copy newly added scanlines to the destination
	* pixmap.
	*****/
	_XmHTMLDebug(14, ("plc.c: _PLC_IMG_Transfer, updating image between "
		"scanline %i to %i\n", ypos, ypos + nlines));

	/* update pixmap */
	XPutImage(dpy, any_image->pixmap, html->html.plc_gc, any_image->ximage,
		 0, ypos, 0, ypos, width, nlines);

	/*****
	* Step 7: display update. The first call updates the master image and
	*         the second call updates any copies of this image.
	* Safety check: only do it when the image has an owner (e.i., it isn't
	* the background image).
	*****/
	if(image->owner)
	{
		_PLC_IMG_UpdateScreen(html, image, image->owner, ypos, nlines);
		_PLC_IMG_UpdateScreenCopies(html, image, ypos, nlines);
	}

	/* all done */
	return;
}

/*****
* Name: 		_PLC_IMG_Finalize
* Return Type:	void
* Description:	Image PLC final transfer function
* In: 
*	plc:		current PLC
* Returns:
*	nothing.
*****/
static void
_PLC_IMG_Finalize(PLC *plc)
{
	PLCImage *any_image;
	XmHTMLWidget html;
	XmImageInfo *info;
	XmHTMLImage *image;

	/* obj_set will be false if this PLC was terminated during init phase */
	if(plc == NULL || !plc->obj_set)
		return;

	any_image = &(plc->object->plc_any_image);
	html   = any_image->owner;
	info   = any_image->info;
	image  = any_image->image;

	_XmHTMLDebug(14, ("plc.c: _PLC_IMG_Finalize for %s\n", plc->url));

	/* this must *always* be destroyed */
	if(any_image->ximage)
		XDestroyImage(any_image->ximage);

	/* no longer need the scaled data, image has been fully processed */
	if(any_image->is_scaled)
	{
		free(any_image->scaled_data);
		any_image->scaled_data = (Byte*)NULL;
		any_image->is_scaled = False;
	}

	/*****
	* Transfer stuff to the info structure. We *need* to test the
	* existence of info 'cause it might disappear when this PLC is aborted.
	*****/
	if(info)
	{
		info->data         = any_image->data;		/* decoded image data */
		info->clip         = any_image->clip_data;	/* raw clipmask data */
		info->bg           = any_image->bg_pixel;	/* background pixel index */
		info->colorspace   = any_image->colorclass;	/* image colorclass */
		info->transparency = any_image->transparency;/* image transparency */
		info->depth        = any_image->depth;		/* image depth */
		info->ncolors      = any_image->nused-1;	/* no of colors in image */
		info->scolors      = any_image->ncolors;	/* original no of cols */
		info->width        = any_image->width;		/* reset to real width */
		info->height       = any_image->height;		/* reset to real height */

		/* this image is no longer being loaded progressively */
		info->options &= ~XmIMAGE_PROGRESSIVE;

		/*****
		* Adjust image RGB components to only contain the number of colors
		* used in this image.
		* Only do it when we have allocated any colors for this image (we
		* didn't allocate colors if this PLC was aborted prematurely).
		*****/
		if(info->ncolors && info->reds != NULL &&
			info->ncolors != any_image->cmapsize)
		{
			/* temporary storage */
			Dimension *reds, *greens, *blues;

			/* save old RGB arrays */
			reds   = info->reds;
			greens = info->greens;
			blues  = info->blues;

			/* allocate new RGB arrays */
			info->reds   = (Dimension*)calloc(3 * info->ncolors,
								sizeof(Dimension));
			info->greens = info->reds   + info->ncolors;
			info->blues  = info->greens + info->ncolors;

			/* copy old RGB arrays */
			info->reds   = (Dimension*)memcpy(info->reds, reds,
								info->ncolors*sizeof(Dimension));
			info->greens = (Dimension*)memcpy(info->greens, greens,
								info->ncolors*sizeof(Dimension));
			info->blues  = (Dimension*)memcpy(info->blues, blues,
								info->ncolors*sizeof(Dimension));

			/* free old RGB arrays */
			free(reds);

			/* update this as well */
			info->scolors = info->ncolors;
		}
	}

	/* update all copies of this image */
	if(image != NULL)
	{
		/* no longer progressive */
		image->options &= ~IMG_PROGRESSIVE;
		if(image->child != NULL)
			_XmHTMLImageUpdateChilds(image);
	}

	/* free remaining PLC resources */
	if(any_image->cmap)
		free(any_image->cmap);
	if(any_image->bg_cmap)
		free(any_image->bg_cmap);
	if(any_image->buffer)
		free(any_image->buffer);

	/*****
	* last and final check: if this is the body image, clear the display
	* area so it will get redrawn properly. Don't do it when we haven't
	* got a GC: we do not yet have a window then.
	*****/
	if(image && ImageIsBackground(image) && html->html.gc != NULL)
	{
		XClearArea(XtDisplay(html->html.work_area),
			XtWindow(html->html.work_area), 0, 0, html->core.width,
			html->core.height, True);
	}
	/* make sure we are updated */
	XmUpdateDisplay((Widget)html);
	return;
}

/*****
* Name: 		_PLC_IMG_UpdateScreen
* Return Type: 	void
* Description: 	Image PLC image->screen transfer function
* In: 
*	html:		XmHTMLWidget id;
*	image:		image data;
*	elePtr:		image owner;
*	src_y:		starting scanline index;
*	height:		number of scanlines to render;
* Returns:
*	nothing.
*****/
static void
_PLC_IMG_UpdateScreen(XmHTMLWidget html, XmHTMLImage *image,
	XmHTMLObjectTableElement elePtr, int src_y, Dimension height)
{
	int tmp = image->height;

	/* set bogus image height */
	image->height = height;
	/* paint it */
	_XmHTMLDrawImage(html, elePtr, src_y, False);
	/* restore real image height */
	image->height = tmp;
}

/*****
* Name:			_PLC_IMG_UpdateScreenCopies
* Return Type: 	void
* Description: 	updates the screen for each copy of the given image.
* In: 
*	html:		XmHTMLWidget id;
*	image:		parent image;
*	src_y:		starting scanline index;
*	height:		number of scanlines to render;
* Returns:
*	nothing.
*****/
static void
_PLC_IMG_UpdateScreenCopies(XmHTMLWidget html, XmHTMLImage *image, int src_y,
	Dimension height)
{
	XmHTMLImage *tmp;

	/* walk the list of child images and call UpdateScreen for each copy */
	for(tmp = image->child; tmp != NULL; tmp = tmp->child)
	{
		if(tmp->owner)
			_PLC_IMG_UpdateScreen(html, tmp, tmp->owner, src_y, height);
	}
}

/***************
***** Document PLC object and supporting functions
***************/

static void
_PLC_DOC_Init(PLC *plc)
{
	_XmHTMLDebug(14, ("plc.c: _PLC_DOC_Init for %s\n", plc->url));
	return;
}

static void
_PLC_DOC_Transfer(PLC *plc)
{
	_XmHTMLDebug(14, ("plc.c: _PLC_DOC_Transfer for %s\n", plc->url));
	plc->obj_set = True;
	return;
}

static void
_PLC_DOC_Finalize(PLC *plc)
{
	_XmHTMLDebug(14, ("plc.c: _PLC_DOC_Finalize for %s\n", plc->url));
	plc->obj_set = True;
	return;
}

/***************
***** Unknown PLC object and supporting functions
***************/
static void
_PLC_ANY_Init(PLC *plc)
{
	_XmHTMLDebug(14, ("plc.c: _PLC_ANY_Init for %s\n", plc->url));
	return;
}

static void
_PLC_ANY_Transfer(PLC *plc)
{
	_XmHTMLDebug(14, ("plc.c: _PLC_ANY_Transfer for %s\n", plc->url));
	return;
}

static void
_PLC_ANY_Finalize(PLC *plc)
{
	_XmHTMLDebug(14, ("plc.c: _PLC_ANY_Finalize for %s\n", plc->url));
	return;
}

/********
****** Public Functions
********/

/* suspend progressive image loading */
void
XmHTMLImageProgressiveSuspend(Widget w)
{
	XmHTMLWidget html;
	PLC *plc;
	int i;

	if(w == NULL || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "ImageProgressiveSuspend");
		return;
	}
	html = (XmHTMLWidget)w;

	/* nothing to suspend */
	if((plc = html->html.plc_buffer) == NULL)
		return;

	/* first suspend all active PLC's. Don't mess with other PLC states. */
	for(i = 0; i < html->html.num_plcs; plc = plc->next_plc, i++)
	{
		if(plc->plc_status == PLC_ACTIVE)
			plc->plc_status = PLC_SUSPEND;
	}

	if(html->html.plc_proc_id)
	{
#ifdef PLC_WORKPROCS
		XtRemoveWorkProc((XtWorkProcId)html->html.plc_proc_id);
#else
		XtRemoveTimeOut(html->html.plc_proc_id);
#endif
		html->html.plc_proc_id = None;
	}
	/* set global PLC suspension flag */
	html->html.plc_suspended = True;
}

/* reactivate progressive image loading */
void
XmHTMLImageProgressiveContinue(Widget w)
{
	XmHTMLWidget html;
	PLC *plc;
	int i;

	if(w == NULL || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "ImageProgressiveContinue");
		return;
	}
	html = (XmHTMLWidget)w;

	/* nothing to do */
	if((plc = html->html.plc_buffer) == NULL)
		return;

	/* first activate all suspended PLC's. Don't mess with other PLC states. */
	for(i = 0; i < html->html.num_plcs; plc = plc->next_plc, i++)
	{
		if(plc->plc_status == PLC_SUSPEND)
			plc->plc_status = PLC_ACTIVE;
	}

	/* reactivate cycler */
	html->html.plc_suspended = False;
	_XmHTMLPLCCycler((XtPointer)html, NULL);
}

/* terminate progressive image loading */
void
XmHTMLImageProgressiveKill(Widget w)
{
	XmHTMLWidget html;

	if(w == NULL || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "ImageProgressiveKill");
		return;
	}
	html = (XmHTMLWidget)w;

	if(html->html.plc_buffer == NULL)
		return;

	/* kill the bastards! */
	html->html.plc_suspended = True;
	_XmHTMLKillPLCCycler(html);
}

/*****
* Name: 		_XmHTMLPLCCheckIntervals
* Return Type: 	void
* Description: 	validates the delays for the PLC Cycler.
* In: 
*	html:		XmHTMLWidget id
* Returns:
*	nothing, but the PLC delay values can have changed when this function
*	returns.
*****/
void
_XmHTMLPLCCheckIntervals(XmHTMLWidget html)
{
	int delay, min_delay, max_delay, new_delay;
	Boolean delay_reset = False;

	delay = HTML_ATTR(plc_delay);
	min_delay = HTML_ATTR(plc_min_delay);
	max_delay = HTML_ATTR(plc_max_delay);

	if(min_delay <= 0)
	{
		_XmHTMLWarning(__WFUNC__(html, "CheckPLCIntervals"), XMHTML_MSG_16,
			"Minimum", min_delay, PLC_MIN_DELAY);
		min_delay = PLC_MIN_DELAY;
	}

	if(delay < min_delay)
	{
		if(min_delay < PLC_DEFAULT_DELAY)
			new_delay = PLC_DEFAULT_DELAY;
		else
			new_delay = min_delay * 50;

		_XmHTMLWarning(__WFUNC__(html, "CheckPLCIntervals"), XMHTML_MSG_16,
			"Initial", delay, new_delay);
		delay = new_delay;
		delay_reset = True;
	}

	if(max_delay <= min_delay)
	{
		new_delay = min_delay <= PLC_MAX_DELAY ?
					PLC_MAX_DELAY : min_delay * 100;

		_XmHTMLWarning(__WFUNC__(html, "CheckPLCIntervals"), XMHTML_MSG_17,
			max_delay, "Minimum", min_delay, new_delay);
		max_delay = new_delay;
	}

	/* can't do anything with this, reset to default values */
	if(max_delay <= delay && !delay_reset)
	{
		_XmHTMLWarning(__WFUNC__(html, "CheckPLCIntervals"), XMHTML_MSG_17,
			max_delay, "Initial", min_delay, PLC_MAX_DELAY);

		delay     = PLC_DEFAULT_DELAY;
		min_delay = PLC_MIN_DELAY;
		max_delay = PLC_MAX_DELAY;
	}

	HTML_ATTR(plc_delay) = HTML_ATTR(plc_def_delay) = delay;
	HTML_ATTR(plc_min_delay) = min_delay;
	HTML_ATTR(plc_max_delay) = max_delay;
}
