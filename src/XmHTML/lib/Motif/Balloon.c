#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* Balloon.c : XmBalloon main routines
*
* This file Version	$Revision$
*
* Creation date:		Sun Nov  2 19:18:37 GMT+0100 1997
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
* Revision 1.1  1998/04/04 06:27:15  newt
* Initial Revision
*
*****/ 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

#include <X11/IntrinsicP.h> 
#include <X11/StringDefs.h>
#include <X11/extensions/shape.h>
#include <X11/Xmu/Converters.h>
#include <Xm/RepType.h>
#include <Xm/XmStrDefs.h>

#include "BalloonP.h"
#include "XmHTMLfuncs.h"

/* We only want to have XmHTML's error functions out of XmHTMLI.h */
#define XmHTML_ERROR_FUNCS
#include "XmHTMLI.h"
#undef XmHTML_ERROR_FUNCS

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/
/***
* Usefull macros
***/
#define BALLOON			XmBalloonWidget balloon
#define ATTR(field)	balloon->balloon.field
#define BORDER_SIZE(w)    ((w)->balloon.border_size)
#define WINDOW_WIDTH(w)    (2.0 - BORDER_SIZE(w)*2)
#define WINDOW_HEIGHT(w)   (2.0 - BORDER_SIZE(w)*2)
#define Xx(x,y,t)  ((int)((t)->mx * (x) + (t)->bx + 0.5))
#define Xy(x,y,t)  ((int)((t)->my * (y) + (t)->by + 0.5))
#define Xwidth(w,h,t)  ((int)((t)->mx * (w) + 0.5))
#define Xheight(w,h,t) ((int)((t)->my * (h) + 0.5))

/***
* Class methods
***/
/*  */
static void ClassInitialize(void);

/* class initialize method */ 
static void Initialize(Widget request, Widget init, ArgList args,
	Cardinal *num_args);

/* class Realize method */
static void Realize(Widget w, XtValueMask *valueMask,
	XSetWindowAttributes *attrs);

/* class set_values method */
static Boolean SetValues(Widget current, Widget request, Widget set,
	ArgList args, Cardinal *num_args);

/* class destroy method */
static void Destroy(Widget w);

/***
* Private Functions
***/
static void popupBalloon(XtPointer client_data, XtIntervalId *id);
static void popdownBalloon(XtPointer client_data, XtIntervalId *id);
static void computeFontInfo(BALLOON);
static void checkGC(BALLOON);
static Boolean setLabel(BALLOON, String label);
static void drawBalloonSquared(Widget w, Position x, Position y, int width);
static void drawBalloonShaped(Widget w, Position x, Position y, int width);
static void drawText(Display *dpy, XmBalloonWidget balloon, Drawable drawable,
	GC gc, int x_offset, int y_offset);
static void setTransform(Transform *t, int xx1, int xx2, int xy1, int xy2,
	double tx1, double tx2, double ty1, double ty2);
static void FillArc(Display *dpy, Drawable d, GC gc, Transform *t, double x,
	double y, double width, double height, int angle1, int angle2);

/*** Private Variable Declarations ***/
static XmRepTypeId corner_style_repid, balloon_style_repid;

/* static resources */
#define Offset(field) XtOffsetOf(XmBalloonRec, balloon.field)

static XtResource resources[] =
{
	{
		XmNforeground,
		XmCForeground, XmRPixel,
		sizeof(Pixel), Offset(foreground),
		XmRString, "black"
	},
#if XtSpecificationRelease < 5
	{
		XmNfont,
		XmCFont, XmRFontStruct,
		sizeof(XFontStruct*), Offset(font),
		XmRString, 
        	"-adobe-helvetica-bold-r-normal-*-12-*-*-*-*-*-*-*"
	},
#else
	{
		XmNfontSet,
		XmCFontSet, XmRFontSet,
		sizeof(XFontSet), Offset(fontset),
		XmRString,
			"*-adobe-helvetica-bold-r-*-*-*-100-*-*-p-*-*-*"
	},
#endif
	{
		XmNpopupDelay,
		XmCPopupDelay, XmRInt,
		sizeof(int), Offset(popup_delay),
		XmRString, "500"
	},
	{
		XmNpopdownDelay,
		XmCPopdownDelay, XmRInt,
		sizeof(int), Offset(popdown_delay),
		XmRString, "0"
	},
	{
		XmNmarginWidth,
		XmCMarginWidth, XmRHorizontalDimension,
		sizeof(Dimension), Offset(margin_width),
		XmRImmediate, (XtPointer)2
	},
	{
		XmNmarginHeight,
		XmCMarginHeight, XmRVerticalDimension,
		sizeof(Dimension), Offset(margin_height),
		XmRImmediate, (XtPointer)2
	},
	{
		XmNvalue,
		XmCValue, XmRString,
		sizeof(String), Offset(label),
		XmRString, (String)NULL
	},
	{
		XmNcornerStyle,
		XmCCornerStyle, XmRCornerStyle,
		sizeof(XtEnum), Offset(corner_style),
		XmRImmediate, (XtPointer)XmCORNER_STRAIGHT
	},
	{
		XmNballoonStyle,
		XmCBalloonStyle, XmRBalloonStyle,
		sizeof(XtEnum), Offset(balloon_style),
		XmRImmediate, (XtPointer)XmBALLOON_SQUARE
	},
	{
		XmNtransparent,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(transparent),
		XmRString, "False"
	},
#if 0
	{
		XmNbackingStore,
		XmCBackingStore, XmRBackingStore,
		sizeof(int), Offset(backing_store),
		XmRString, "default"
	},
#endif
	{
		XmNborderSize,
		XmCBorderSize, XmRFloat,
		sizeof(float), Offset(border_size),
		XtRString, "0.075"
	},
	{
		XmNleftOffset,
		XmCOffset, XmRInt,
		sizeof(int), Offset(left_offset),
		XtRString, "15"
	},
	{
		XmNtopOffset,
		XmCOffset, XmRInt,
		sizeof(int), Offset(top_offset),
		XtRString, "5"
	},
};
#undef Offset

/****
* Define the widget class record.
****/
XmBalloonClassRec xmBalloonClassRec = {
											/* core class fields		*/
{
	(WidgetClass)&overrideShellClassRec,	/* superclass				*/
	"XmBalloon",							/* class_name				*/
	(Cardinal)sizeof(XmBalloonRec),			/* widget size				*/
	ClassInitialize,						/* class_init				*/
	(XtWidgetClassProc)NULL,				/* class_part_init			*/
	(XtEnum)FALSE,							/* class_inited				*/
	(XtInitProc)Initialize,					/* initialize				*/
	(XtArgsProc)NULL,						/* init_hook				*/
	(XtRealizeProc)Realize,					/* realize					*/
	(XtActionList)0,						/* actions					*/
	(Cardinal)0,							/* num_actions				*/
	(XtResourceList)resources,				/* resources				*/
	(Cardinal)XtNumber(resources),			/* num_resources			*/
	NULLQUARK,								/* xrm_class				*/
	TRUE,									/* compress_motion			*/
	(XtEnum)FALSE,							/* compress_exposure		*/
	TRUE,									/* compress enterleave		*/
	FALSE,									/* visibility_interest		*/
	(XtWidgetProc)Destroy,					/* destroy					*/
	XtInheritResize,						/* resize					*/
	XtInheritExpose,						/* expose					*/
	(XtSetValuesFunc)SetValues,				/* set_values				*/
	(XtArgsFunc)NULL,						/* set_values_hook			*/
	XtInheritSetValuesAlmost,				/* set_values_almost		*/
	(XtArgsProc)NULL,						/* get_values_hook			*/
	XtInheritAcceptFocus,					/* accept_focus				*/
	XtVersion,								/* version					*/
	(XtPointer)NULL,						/* callback_private			*/
	XtInheritTranslations,					/* tm_table					*/
	XtInheritQueryGeometry,					/* query_geometry			*/
	XtInheritDisplayAccelerator,			/* display_accelerator		*/
	(XtPointer)0,							/* extension				*/
},
											/* composite_class fields	*/
{
	XtInheritGeometryManager,				/* geometry_manager   		*/    	
	XtInheritChangeManaged,					/* change_managed			*/	
	XtInheritInsertChild,					/* insert_child				*/	
	XtInheritDeleteChild,					/* delete_child				*/	
	NULL									/* extension				*/	
}, 
											/* shell class fields		*/
{
	(XtPointer)NULL,						/* extension				*/
},
											/* override_shell fields	*/
{
	0,										/* none 					*/
},
											/* balloon_class fields		*/
{
	0,										/* none 					*/
}
};

WidgetClass xmBalloonWidgetClass = (WidgetClass) & xmBalloonClassRec;

/********
**** Widget Methods
********/

static void
ClassInitialize(void)
{
	static char *corners[] = {"straight", "beveled", "slant", "round"};
	static char *balloons[] = {"square", "shaped"};

#if 0
	XtAddConverter(XmRString, XmRBackingStore, XmuCvtStringToBackingStore,
		NULL, 0);
#endif

	XmRepTypeRegister(XmCCornerStyle, corners, NULL, 4);
	corner_style_repid = XmRepTypeGetId(XmCCornerStyle);

	XmRepTypeRegister(XmCBalloonStyle, balloons, NULL, 2);
	balloon_style_repid = XmRepTypeGetId(XmCBalloonStyle);
}

/*****
* Name: 		Initialize
* Return Type: 	void
* Description: 	XmBalloonWidgetClass initialize method
* In:
*	request:	widget with resource values set as requested by the argument
*				list, resource database and widget defaults
*	init:		same widget with values as modified by superclass initialize()
*				methods
*	args:		argument list passed to XtCreateWidget
*	num_args:	number of entries in the argument list
* Returns:
*	nothing, but init is updated with checked/updated resource values.
*****/
static void
Initialize(Widget request, Widget init, ArgList args, Cardinal *num_args)
{
	XmBalloonWidget balloon = (XmBalloonWidget)init;
	XmBalloonWidget req     = (XmBalloonWidget)request;
	int shape_event_base, shape_error_base;

	/* initialize fields */
	ATTR(gc)			= NULL;
	ATTR(popped)		= False;
	ATTR(pop_x)		    = (Position)0;
	ATTR(pop_y)	    	= (Position)0;
	ATTR(popup_id)    	= (XtIntervalId)0;
	ATTR(popdown_id)	= (XtIntervalId)0;
	ATTR(context)		= XtWidgetToApplicationContext(XtParent(request));

	balloon->core.x     = 0;
	balloon->core.y     = 0;
	balloon->core.width = 1;
	balloon->core.height= 1;

	/* select default backingStore model */
	ATTR(backing_store) = Always + WhenMapped + NotUseful;

	/* check for correct XmNcornerStyle value */
	if(!XmRepTypeValidValue(corner_style_repid, ATTR(corner_style), init))
	{
		_XmHTMLWarning(__WFUNC__(balloon, "initialize"),
			"Bad XmNcornerStyle value, reset to XmCORNER_STRAIGHT");
		ATTR(corner_style) = XmCORNER_STRAIGHT;
	}

	/* check for correct XmNballoonStyle value */
	if(!XmRepTypeValidValue(balloon_style_repid, ATTR(balloon_style), init))
	{
		_XmHTMLWarning(__WFUNC__(balloon, "Initialize"),
			"Bad XmNballoonStyle value, reset to XmBALLOON_SQUARE");
		ATTR(balloon_style) = XmBALLOON_SQUARE;
	}

	/* When a shaped balloon is requested, check if the server supports it. */
	if(ATTR(balloon_style) == XmBALLOON_SHAPED &&
		!XShapeQueryExtension(XtDisplay(balloon), &shape_event_base,
			&shape_error_base))
	{
		_XmHTMLWarning(__WFUNC__(balloon, "Initialize"),
			"Shape extension not supported by XServer, resetting "
			"XmNballoonStyle to XmBALLOON_SQUARE.");
		ATTR(balloon_style) = XmBALLOON_SQUARE;
	}
	ATTR(shape_window) = (ATTR(balloon_style) == XmBALLOON_SHAPED);
	ATTR(shape_mask)   = None;
	ATTR(shape_gc)     = 0;
	ATTR(shape_width)  = 0;
	ATTR(shape_height) = 0;

	/* get font info */
	computeFontInfo(balloon);

	/* and create the GC's for us to use */
	checkGC(balloon);

	/* label string */
	if(req->balloon.label != NULL)
	{
		ATTR(source) = strdup(req->balloon.label);
		ATTR(source_len) = strlen(req->balloon.label);
	}
	else
	{
		ATTR(source) = NULL;
		ATTR(source_len) = 0;
	}
}

/*****
* Name:			Realize
* Return Type:	void
* Description:	XmBalloon realize class method
* In:
*	w:			XmBalloon Widget id;
*	valueMask:	mask to use for window creation;
*	attrs:		window attributes as set by parent classes
* Returns:
*	nothing.
*****/
static void
Realize(Widget w, XtValueMask *valueMask, XSetWindowAttributes *attrs)
{
	BALLOON = (XmBalloonWidget)w;

	if(ATTR(backing_store) != Always + WhenMapped + NotUseful)
	{
		attrs->backing_store |= ATTR(backing_store);
		*valueMask |= CWBackingStore;
	}

	if(ATTR(transparent))
	{
		attrs->background_pixel = ATTR(foreground);
		*valueMask |= CWBackPixel;
		*valueMask &= ~CWBackPixmap;
	}
	XtCreateWindow(w, (unsigned int)InputOutput, (Visual*)CopyFromParent,
		*valueMask, attrs);
}

/*****
* Name:			SetValues
* Return Type:	Boolean
* Description:	xmBalloonWidgetClass SetValues method.
* In:
*	current:	copy of widget before any set_values methods are called
*	request:	copy of widget after resources have changed but before any
*				set_values methods are called
*	set:		widget with resources set and as modified by any superclass
*				methods that have called XtSetValues()
*	args:		argument list passed to XtSetValues, done by programmer
*	num_args:	no of args
* Returns:
*	True if a changed resource requires a redisplay, False otherwise.
*****/
static Boolean
SetValues(Widget current, Widget request, Widget set, ArgList args,
	Cardinal *num_args)
{
	XmBalloonWidget w_curr = (XmBalloonWidget)current;
	XmBalloonWidget w_req  = (XmBalloonWidget)request;
	XmBalloonWidget w_new  = (XmBalloonWidget)set;
	int i;

	/*****
	* Values of cancelWaitPeriod and waitPeriod are accepted without checking
	*****/

	/* widget colors */
	if(w_new->balloon.foreground != w_curr->balloon.foreground  ||
		w_new->core.background_pixel != w_curr->core.background_pixel)
	{
		checkGC(w_new);
	}

	/* label string, check if a new one has been provided */
	for(i = 0; i < *num_args; i++)
	{
		if(!strcmp(XmNlabelString, args[i].name))
		{
			/* a new label has been specified */
			setLabel(w_new, w_req->balloon.label);
			break;
		}
	}
	/* new label set while widget is up, change it */
	if(i != *num_args && w_curr->balloon.popped)
		popupBalloon((Widget)w_new, 0);

	return(FALSE);
}

/*****
* Name: 		Destroy
* Return Type: 	void
* Description: 	XmBalloon destroy method. Deletes the GC we have been using
*				and frees the space allocated for the label.
* In: 
*	w:			XmBalloonWidget id;
* Returns:
*	nothing.
*****/
static void
Destroy(Widget w)
{
	XmBalloonWidget balloon = (XmBalloonWidget)w;

	/* kill any outstanding timeouts */
	if(ATTR(popup_id))
		XtRemoveTimeOut(ATTR(popup_id));
	if(ATTR(popdown_id))
		XtRemoveTimeOut(ATTR(popdown_id));

	/* release the GC */
	if(ATTR(gc))
		XtReleaseGC((Widget)balloon, ATTR(gc));

	/* release Shaped window stuff */
	if(ATTR(shape_gc))
		XtReleaseGC((Widget)balloon, ATTR(shape_gc));
	if(ATTR(shape_mask))
		XFreePixmap(XtDisplay(w), ATTR(shape_mask));

	/* and free the label */
	if(ATTR(source))
		free(ATTR(source));
}

/********
**** Supporting Functions
********/

static void
drawText(Display *dpy, XmBalloonWidget balloon, Drawable drawable, GC gc,
	int x_offset, int y_offset)
{
	/* draw the text in the window */
#if XtSpecificationRelease < 5
	XDrawString(dpy, drawable, gc,
		ATTR(margin_width) + x_offset,
		ATTR(margin_height) + ATTR(baseline) + y_offset,
		ATTR(source), ATTR(source_len));
#else
	XmbDrawString(dpy, drawable, ATTR(fontset), gc,
		ATTR(margin_width) + x_offset, 
		ATTR(margin_height) + ATTR(baseline) + y_offset,
		ATTR(source), ATTR(source_len));
#endif
}


/*****
* Name:			drawBalloonShaped
* Return Type:	void
* Description:	pops up the balloon widget as a shaped window
* In:
*	w:			XmBalloon Widget id;
*	x:			absolute x popup position;
*	y:			absolute y popup position;
*	width:		desired widget width;
* Returns:
*	nothing
* Note:
*	This routine composes a *clipmask* for the widget to use when
*	it is displayed. The clipmask is initially empty and gets filled
*	according to the selected options. Once it is filled, the text
*	is rendered in the selected color.
*****/
static void
drawBalloonShaped(Widget w, Position x, Position y, int width)
{
	BALLOON = (XmBalloonWidget)w;
	XGCValues xgc;
	int face_width, face_height, x_offset, y_offset;
	Dimension bwidth, bheight;
	Pixmap shape_mask;
	Display *dpy = XtDisplay(w);
	Window win = XtWindow(w);

	/* horizontal offset for text rendering */
	x_offset = ATTR(margin_width) + ATTR(font_width);
	y_offset = 0.25 * ATTR(font_height);

	bwidth = 2*ATTR(margin_width) + width + 2*x_offset;
	bheight = 2*ATTR(margin_height) + ATTR(font_height) + 2*y_offset;

	/* resize to fit */
	XtResizeWidget(w, bwidth, bheight, balloon->core.border_width);

	/* compute desired border size */
	setTransform(&ATTR(maskt), 0, bwidth, bheight, 0, -1.0, 1.0, -1.0, 1.0);

	face_width = abs(Xwidth(BORDER_SIZE(balloon), BORDER_SIZE(balloon),
		&(ATTR(maskt))));
	face_height = abs(Xheight(BORDER_SIZE(balloon), BORDER_SIZE(balloon),
		&(ATTR(maskt))));

	setTransform(&ATTR(t), face_width, bwidth - face_width,
		bheight - face_height, face_height, 
		-WINDOW_WIDTH(balloon)/2, WINDOW_WIDTH(balloon)/2,
		-WINDOW_HEIGHT(balloon)/2, WINDOW_HEIGHT(balloon)/2);

	/* Free up previous clipmask if the size differs */
	if(ATTR(shape_mask) &&
		ATTR(shape_width) != w->core.width &&
		ATTR(shape_height) != w->core.height)
	{
		XFreePixmap(dpy, ATTR(shape_mask));
		ATTR(shape_mask) = None;
	}

	/* allocate a clipmask (bitmap of depth one) */
	if(!(ATTR(shape_mask)))
	{
		ATTR(shape_mask) = XCreatePixmap(dpy, win, bwidth, bheight, 1);
		ATTR(shape_width) = bwidth;
		ATTR(shape_height) = bheight;
	}
	shape_mask = ATTR(shape_mask);

	/* simple gc */
	if(!(ATTR(shape_gc)))
		ATTR(shape_gc) = XCreateGC(dpy, shape_mask, 0, &xgc);

	/* make it fully transparent */
	XSetForeground(dpy, ATTR(shape_gc), 0);
	XFillRectangle(dpy, shape_mask, ATTR(shape_gc), 0, 0, bwidth, bheight);
	XSetForeground(dpy, ATTR(shape_gc), 1);

	/*****
	* Fill in the border bits if we have a border. If we aren't transparent
	* a filled arc is created.
	*****/
	if(ATTR(border_size) > 0.0 || !ATTR(transparent))
	{
		FillArc(dpy, shape_mask, ATTR(shape_gc), &(ATTR(maskt)),
			-1.0, -1.0, 2.0, 2.0, 0, 360*64);
	}

	/*****
	* if we are being transparent, erase the inner part of the disk
	* and fill the bits for the text. If we aren't transparent we don't
	* have to do this 'cause the bits set for the disk already cover the
	* bits that cover the text.
	*****/
	if(ATTR(transparent))
	{
		if(ATTR(border_size) > 0.0)
		{
			XSetForeground(dpy, ATTR(shape_gc), 0);
			FillArc(dpy, shape_mask, ATTR(shape_gc), &(ATTR(maskt)),
				-WINDOW_WIDTH(balloon)/2, -WINDOW_HEIGHT(balloon)/2,
				WINDOW_WIDTH(balloon), WINDOW_HEIGHT(balloon),
				0, 360*64);
			XSetForeground(dpy, ATTR(shape_gc), 1);
		}
		drawText(dpy, balloon, shape_mask, ATTR(shape_gc), x_offset, y_offset);
	}
	/* the highest enclosing widget is the widget itself */
	XShapeCombineMask(dpy, win, ShapeBounding, 0, 0, shape_mask, ShapeSet);

	/* erase clipmask */
	XSetForeground(dpy, ATTR(shape_gc), 0); 
	XFillRectangle(dpy, shape_mask, ATTR(shape_gc), 0, 0, bwidth, bheight);
	XSetForeground(dpy, ATTR(shape_gc), 1);

	/* draw clip shape */
	if(ATTR(transparent))
	{
		drawText(dpy, balloon, shape_mask, ATTR(shape_gc), x_offset, y_offset);
	}
	else
	{
		FillArc(dpy, shape_mask, ATTR(shape_gc), &(ATTR(t)),
			-WINDOW_WIDTH(balloon)/2, -WINDOW_HEIGHT(balloon)/2,
			WINDOW_WIDTH(balloon), WINDOW_HEIGHT(balloon),
			0, 360*64);
	}

	/* compose final clipmask */
	XShapeCombineMask(dpy, win, ShapeClip, 0, 0, shape_mask, ShapeSet);

	/* move to correct location */
	XtMoveWidget((Widget)balloon, x + 4, y + 4);

	/*****
	* pop it up.
	* Note that the label can change when the widget is already popped.
	*****/
	if(!ATTR(popped))
		XtPopup((Widget)balloon, XtGrabNone);
	ATTR(popped) = True;

	/* draw the text */
	drawText(dpy, balloon, win, ATTR(gc), x_offset, y_offset);

	/* if we have got a popdown timeout, add it */
	if(ATTR(popdown_delay))
	{
		ATTR(popdown_id) = XtAppAddTimeOut(ATTR(context),
			ATTR(popdown_delay), popdownBalloon, (XtPointer)balloon);
	}

}

/*****
* Name:			drawBalloonSquared
* Return Type:	void
* Description:	pops up the balloon widget as a simple square
* In:
*	w:			XmBalloon Widget id;
*	x:			absolute x popup position;
*	y:			absolute y popup position;
*	width:		desired widget width;
* Returns:
*	nothing
*****/
static void
drawBalloonSquared(Widget w, Position x, Position y, int width)
{
	XmBalloonWidget balloon = (XmBalloonWidget)w;
	Display *dpy = XtDisplay(w);

	/* resize to fit */
	XtResizeWidget((Widget)balloon, 2*ATTR(margin_width) + width, 
		2*ATTR(margin_height) + ATTR(font_height),
		balloon->core.border_width);

	/* move to correct location */
	XtMoveWidget(w, x + ATTR(left_offset), y + ATTR(top_offset));

	/*****
	* pop it up.
	* Note that the label can change when the widget is already popped.
	*****/
	if(!ATTR(popped))
		XtPopup((Widget)balloon, XtGrabNone);
	ATTR(popped) = True;

	/* do a FillRect to clear current label */
	XSetForeground(dpy, ATTR(gc), balloon->core.background_pixel);
	XFillRectangle(dpy, XtWindow((Widget)balloon), ATTR(gc), 0, 0,
		balloon->core.width, balloon->core.height);
	XSetForeground(dpy, ATTR(gc), ATTR(foreground));

	/* draw the text in the window */
	drawText(dpy, balloon, XtWindow((Widget)balloon), ATTR(gc), 0, 0);
}

/*****
* Name: 		popupBalloon
* Return Type: 	void
* Description: 	pops up a balloon widget;
* In: 
*	client_..:	timeout client data, this is a XmBalloonWidget.
*	id:			interval id. When 0 it means this function has been called
*				directly from within the widget code. This will cause any
*				existing timeouts to be removed.
* Returns:
*	nothing.
*****/
static void
popupBalloon(XtPointer client_data, XtIntervalId *id)
{
	XmBalloonWidget balloon = (XmBalloonWidget)client_data;
	Position x, y;
	XRectangle ink, logical;

	/* remove timeout if called from within the widget code */
	if(*id == (XtIntervalId)0 && ATTR(popup_id))
		XtRemoveTimeOut(ATTR(popup_id));

	/* always remove any outstanding popdownDelay */
	if(ATTR(popdown_id))
		XtRemoveTimeOut(ATTR(popdown_id));

	ATTR(popdown_id) = (XtIntervalId)0;
	ATTR(popup_id) = (XtIntervalId)0;

	/* get dimensions for the widget (depends on the current label text) */
#if XtSpecificationRelease < 5
	{
		int dir, ascent, descent;
		XCharStruct sw;
		XTextExtents(ATTR(font), ATTR(source), ATTR(source_len),
			&dir, &ascent, &descent, &sw); 
		logical.width = sw.width;
	}
#else
	XmbTextExtents(ATTR(fontset), ATTR(source), ATTR(source_len), &ink,
		&logical);
#endif

	/* No coordinates given, query to current pointer location */
	if(ATTR(pop_x) == 0 && ATTR(pop_y) == 0)
	{
		Widget parent = XtParent((Widget)balloon);
		Display *dpy = XtDisplay(parent);
		Window w = XtWindow(parent);
		Window root, child;
		int win_x, win_y, root_x, root_y;
		unsigned int mask_return;

		/* must get pointer position relative to child window position */
		if(!XQueryPointer(dpy, w, &root, &child, &root_x, &root_y, &win_x,
			&win_y, &mask_return))
			return;

		/*****
		* don't put x and y in the above call, win_x and win_y are int
		* whereas x and y are Position (short int)
		*****/
		x = (Position)win_x;
		y = (Position)win_y;
	}
	else /* translate local pointer position to global position */
	{
		x = ATTR(pop_x);
		y = ATTR(pop_y);
	}

	if(ATTR(shape_window))
		drawBalloonShaped((Widget)balloon, x, y, logical.width);
	else
		drawBalloonSquared((Widget)balloon, x, y, logical.width);

	/* if we have got a popdown timeout, add it */
	if(ATTR(popdown_delay))
	{
		ATTR(popdown_id) = XtAppAddTimeOut(ATTR(context),
			ATTR(popdown_delay), popdownBalloon, (XtPointer)balloon);
	}
}

/*****
* Name: 		popdownBalloon
* Return Type: 	void
* Description: 	Timeout when the popdownDelay resource has been set.
*				Pops down a currently popped up balloon.
* In: 
*	client_..:	timeout client data, this is a XmBalloonWidget.
*	id:			interval id. When 0 it means this function has been called
*				directly from within the widget code. This will cause any
*				existing timeouts to be removed.
* Returns:
*	nothing.
*****/
static void
popdownBalloon(XtPointer client_data, XtIntervalId *id)
{
	XmBalloonWidget balloon = (XmBalloonWidget)client_data;

	/* remove timeout if called from within the widget code */
	if(id == NULL && ATTR(popdown_id))
		XtRemoveTimeOut(ATTR(popdown_id));

	/* always remove any outstanding popupDelay */
	if(ATTR(popup_id))
		XtRemoveTimeOut(ATTR(popup_id));

	ATTR(popup_id) = (XtIntervalId)0;
	ATTR(popdown_id) = (XtIntervalId)0;

	/* pop it down */
	if(ATTR(popped))
	{
		XtPopdown((Widget)balloon);
		ATTR(popped) = False;
	}
}

/*****
* Name:			computeFontInfo
* Return Type: 	void
* Description: 	computes the required font extents for properly displaying
*				the label in the popped up widget.
* In: 
*	BALLOON:	XmBalloonWidget id;
* Returns:
*	nothing.
*****/
#if XtSpecificationRelease >= 5
static void
computeFontInfo(BALLOON)
{
	XRectangle ink;
	XRectangle logical;

	if(!ATTR(fontset))
		return;

	XmbTextExtents(ATTR(fontset), "1", 1,&ink, &logical);

	/*****
	* Collect font dimensions. Baseline offset is returned relative to the
	* *bottom* of the char, so we need to invert it.
	*****/
	ATTR(baseline)    = -logical.y;
	ATTR(font_width)  = logical.width;
	ATTR(font_height) = logical.height;
}

#else
/* R4 and below code */
static void
computeFontInfo(BALLOON)
{
	int dir, ascent, descent;
	XCharStruct sw;

	if(ATTR(font) == NULL)
		return;

	XTextExtents(ATTR(font), "1", 1, &dir, &ascent, &descent, &sw);

	/* collect font dimensions */
	ATTR(baseline)    = sw.ascent;
	ATTR(font_width)  = sw.width;
	ATTR(font_height) = sw.ascent + sw.descent;
}
#endif

/*****
* Name: 		checkGC
* Return Type: 	void
* Description: 	creates a GC for us to use;
* In: 
*	BALLOON:	XmBalloonWidget id;
* Returns:
*	nothing.
*****/
static void
checkGC(BALLOON)
{
	XGCValues xgc;

	xgc.foreground = ATTR(foreground);
	xgc.background = balloon->core.background_pixel;
	xgc.fill_style = FillSolid; 

#if XtSpecificationRelease < 5	
	xgc.font = ATTR(font->fid); 
#endif

	if(ATTR(gc))
		XtReleaseGC((Widget)balloon, ATTR(gc));
	ATTR(gc) = XtGetGC((Widget)balloon,
		GCForeground|GCBackground|GCFillStyle, &xgc);
}

/*****
* Name: 		setLabel
* Return Type: 	Boolean
* Description: 	checks is a new label has been provided, and if so updates
*				the previous label.
* In: 
*	BALLOON:	XmBalloonWidget id;
*	label:		new label. Can be NULL.
* Returns:
*	True when a new label has been provided, False if not.
* Note:
*	it's up to the caller to respond properly.
*****/
static Boolean
setLabel(BALLOON, String label)
{
	Boolean new_label = False;

	/* check if we had a previous label */
	if(ATTR(source))
	{
		/* new text provided */
		if(label)
		{
			/* see if it differs */
			if(strcmp(ATTR(source), label))
			{
				/* it does */
				new_label = True;

				/* free previous label */
				free(ATTR(source));

				/* copy new label */
				ATTR(source)     = strdup(label);
				ATTR(source_len) = strlen(label);
				ATTR(label)      = label;
			}
			else
				new_label = False;
		}
		else	/* have to clear current label */
		{
			new_label = True;

			/* free previous source */	
			free(ATTR(source));

			/* reset to NULL */
			ATTR(source) = ATTR(label) = NULL;
			ATTR(source_len) = 0;
		}
	}
	else	/* we didn't have a label to begin with */
	{
		if(label)
		{
			/* new text */
			new_label = True;

			/* copy */
			ATTR(source)     = strdup(label);
			ATTR(source_len) = strlen(label);
			ATTR(label)      = label;
		}
		else
			new_label = False;		/* still empty */
	}
	return(new_label);
}

static void
FillArc(Display *dpy, Drawable d, GC gc, Transform *t, double x, double y,
	double width, double height, int angle1, int angle2)
{
	int xx, xy, xw, xh;

	xx = Xx(x,y,t);
	xy = Xy(x,y,t);
	xw = Xwidth (width, height, t);
	xh = Xheight (width, height, t);
	if (xw < 0)
	{
		xx += xw;
		xw = -xw;
	}
	if (xh < 0)
	{
		xy += xh;
		xh = -xh;
	}
	XFillArc(dpy, d, gc, xx, xy, xw, xh, angle1, angle2);
}

static void
setTransform(Transform *t, int xx1, int xx2, int xy1, int xy2,
	double tx1, double tx2, double ty1, double ty2)
{
	t->mx = ((double) xx2 - xx1) / (tx2 - tx1);
	t->bx = ((double) xx1) - t->mx * tx1;
	t->my = ((double) xy2 - xy1) / (ty2 - ty1);
	t->by = ((double) xy1) - t->my * ty1;
}

/********
**** Public Convenience Functions
********/

/*****
* Name: 		XmCreateBalloon
* Return Type: 	Widget
* Description: 	creates an XmBalloonWidget.
* In: 
*	parent:		parent for this new widget;
*	name:		name for the newly created widget;
*	argslist:	list of arguments to be used for creating this widget;
*	argcount:	size of arglist;
* Returns:
*	the ID of the newly created widget on success, NULL on failure.
*****/
Widget
XmCreateBalloon(Widget parent, String name, ArgList arglist, Cardinal argcount)
{
	if(parent)
		return(XtCreateWidget(name, xmBalloonWidgetClass, parent,
			arglist, argcount));
	_XmHTMLWarning(__WFUNC__(NULL, "XmCreateBalloon"), "XmBalloon requires "
		"a non-NULL parent");
	return(NULL);
}

/*****
* Name: 		XmBalloonPopup
* Return Type: 	void
* Description: 	displays a XmBalloonWidget
* In: 
*	w:			widget ID, must be of class xmBalloonWidgetClass;
*	event:		event information for proper popup location;
*	label:		label to be displayed;
* Returns:
*	nothing.
*****/
void
XmBalloonPopup(Widget w, Position x, Position y, String label)
{
	BALLOON;

	/* sanity check */
	if(!w || !XmIsBalloon(w))
	{
		_XmHTMLBadParent(w, "XmBalloonPopup");
		return;
	}

	balloon = (XmBalloonWidget)w;

	/* not up, pop it up */
	if(!ATTR(popped))
	{
		/* verify label string */
		setLabel(balloon, label);

		ATTR(pop_x) = x;
		ATTR(pop_y) = y;
		ATTR(popup_id) = XtAppAddTimeOut(ATTR(context),
			ATTR(popup_delay), popupBalloon, (XtPointer)balloon);
		return;
	}
	/* already popped up, change text if we have a new label */
	if(setLabel(balloon, label))
	{
		/* if new label is NULL, pop it down */
		if(ATTR(source) == NULL)
		{
			XmBalloonPopdown(w);
		}
		else	/* new label provided, change it */
		{
			ATTR(pop_x) = x;
			ATTR(pop_y) = y;
			popupBalloon((XtPointer)balloon, 0); 
		}
	}
}

/*****
* Name: 		XmBalloonPopdown
* Return Type: 	void
* Description: 	pops down a popped up XmBalloonWidget
* In: 
*	w:			widget ID, must be of class xmBalloonWidgetClass;
* Returns:
*	nothing.
*****/
void
XmBalloonPopdown(Widget w)
{
	/* sanity check */
	if(!w || !XmIsBalloon(w))
	{
		_XmHTMLBadParent(w, "XmBalloonPopdown");
		return;
	}
	popdownBalloon((XtPointer)w, 0);
}

