#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* XmHTML.c : XmHTML main routines, Xt/Motif version
*
* This file Version	$Revision$
*
* Creation date:		Thu Nov 21 05:02:44 GMT+0100 1996
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
* Revision 1.1  2011/06/30 16:10:30  rwcox
* Cadd
*
* Revision 1.19  1998/04/27 06:56:21  newt
* tka: extracted all toolkit independent functions to private.c and public.c
*
* Revision 1.18  1998/04/04 06:27:51  newt
* XmHTML Beta 1.1.3
*
* Revision 1.17  1997/10/26 23:49:18  newt
* Bugfixes 10/22/97-01, 10/26/97-01
*
* Revision 1.16  1997/08/31 17:32:33  newt
* Again some fixes in _XmHTMLMoveToPos
*
* Revision 1.15  1997/08/30 00:38:58  newt
* Anchor Highlighting, form component traversal action routines, alpha
* channel support, loads of fixes in _XmHTMLMoveToPos, DrawRedisplay and
* SetValues.
*
* Revision 1.14  1997/08/01 12:54:48  newt
* Progressive image loading changes.
*
* Revision 1.13  1997/05/28 01:38:30  newt
* Added a check on the value of the XmNmaxImageColors resource. Added a check
* on the scrollbar dimensions. Modified the SetValues method to properly deal
* with the XmNbodyImage resource. Modified XmHTMLImageFreeImageInfo to call
* _XmHTMLFreeImageInfo.
*
* Revision 1.12  1997/04/29 14:22:14  newt
* A lot of changes in SetValues, Layout, XmHTMLTextSetString.
* Added XmHTMLXYToInfo.
*
* Revision 1.11  1997/04/03 05:31:02  newt
* Modified XmHTMLImageReplace and XmHTMLImageUpdate to immediatly render an
* image whenever possible. Added the XmHTMLFrameGetChild convenience function.
*
* Revision 1.10  1997/03/28 07:05:28  newt
* Frame interface added. 
* Horizontal scrolling bugfix. 
* Added all remaining action routines and adjusted translation table.
*
* Revision 1.9  1997/03/20 08:04:38  newt
* XmHTMLImageFreeImageInfo, XmNrepeatDelay, changes in public image functions
*
* Revision 1.8  1997/03/11 19:49:53  newt
* Fix 03/11/97-01, SetValues. 
* Added XmHTMLImageGetType, updated XmHTMLImageReplace and XmHTMLImageUpdate
*
* Revision 1.7  1997/03/04 18:45:15  newt
* ?
*
* Revision 1.6  1997/03/04 00:55:54  newt
* Delayed Image Loading changes: XmHTMLReplaceImage, XmHTMLUpdateImage 
* and XmHTMLRedisplay added
*
* Revision 1.5  1997/03/02 23:06:35  newt
* Added image/imagemap support; changed expose method; added background 
* image painting
*
* Revision 1.4  1997/02/11 02:05:44  newt
* Changes related to autosizing, exposure and scrolling
*
* Revision 1.3  1997/01/09 06:54:53  newt
* expanded copyright marker
*
* Revision 1.2  1997/01/09 06:42:40  newt
* XmNresizeWidth and XmNresizeHeight updates
*
* Revision 1.1  1996/12/19 02:17:03  newt
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
#include <X11/keysym.h>		/* for keycodes in HTMLProcessInput */
#include <X11/Xmu/StdSel.h>

#include <Xm/XmP.h>			/* XmField, XmPartOffset and private motif funcs. */
#include <Xm/DrawP.h>
#include <Xm/XmStrDefs.h>	/* For motif XmN macros */

/***** 
* Test if these macros are present. They are required for conversion of the
* enumeration resource values. I don't know if they are present in Motif 1.2.X. 
* If not here, spit out an error message.
* If you can't compile this because this error occurs, please contact us
* at the address given below and state what version of Motif you are using.
* Note: these are explicit syntax errors...
*****/
#ifndef XmPartOffset
	XmPartOffset macro undefined. Please contact ripley@xs4all.nl
#endif
#ifndef XmField
	XmField macro undefined. Please contact ripley@xs4all.nl
#endif

#include <Xm/DrawingA.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/RepType.h>

/* Our private header files */
#include "toolkit.h"
#include XmHTMLPrivateHeader

/* This is the anchor cursor */
#include "bitmaps/fingers.xbm"
#include "bitmaps/fingers_m.xbm"

/*** External Function Prototype Declarations ***/
/* 
* undocumented motif functions used: (declared in XmP.h): 
* _XmRedisplayGadgets (Widget, XEvent*, Region);
*/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/
/* Static resources */
#include "resources.h"

/*** Private Function Prototype Declarations ****/
/***
* Private functions 
***/
static void CreateAnchorCursor(XmHTMLWidget html);
static void CreateHTMLWidget(XmHTMLWidget html);
static void CheckAnchorUnderlining(XmHTMLWidget html, XmHTMLWidget req);
static void CheckAlignment(XmHTMLWidget html, XmHTMLWidget req);
static void ScrollCB(Widget w, XtPointer arg1, XtPointer arg2);

/* manage scrollbars if necessary */
#define SetScrollBars(HTML) { \
	if(ATTR_HTML(HTML, needs_hsb) && \
		!(ATTR_HTML(HTML,tka)->IsManaged(ATTR_HTML(HTML,hsb)))) \
		ATTR_HTML(HTML,tka)->ManageChild(ATTR_HTML(HTML, hsb)); \
	if(ATTR_HTML(HTML, needs_vsb) && \
		!(ATTR_HTML(HTML,tka)->IsManaged(ATTR_HTML(HTML,vsb)))) \
		ATTR_HTML(HTML,tka)->ManageChild(ATTR_HTML(HTML, vsb)); \
}

/***
* Class methods
***/
/* Primary ClassInitialize method */
static void ClassInitialize(void);

/* ClassPartInitialize method */
static void ClassPartInitialize(WidgetClass wc);

/* class initialize method */
static void Initialize(Widget request, Widget init, ArgList args,
	Cardinal *num_args);

/* class expose method */
static void Redisplay(Widget w, XEvent *event, Region region);

/* Expose event handler for the work area */
static void DrawRedisplay(Widget w, XmHTMLWidget html, XEvent *event);

/* VisibilityNotify event handler for the work area */
static void VisibilityHandler(Widget w, XmHTMLWidget html, XEvent *event);

/* MapNotify action routine for the work area */
static void Mapped(Widget w, XmHTMLWidget html, XEvent *event); 

/* class set_values method */
static Boolean SetValues(Widget current, Widget request, Widget set,
	ArgList args, Cardinal *num_args);

/* class get_values_hook method */
static void GetValues(Widget w, ArgList args, Cardinal *num_args);

/* class geometry_manager method */
static XtGeometryResult GeometryManager(Widget w, XtWidgetGeometry *request,
	XtWidgetGeometry *geometry_return);

/* class destroy method */
static void Destroy(Widget w);

/* Action routines */
static void	ExtendStart(Widget w, XEvent *event, String *params, 
		Cardinal *num_params);
static void	ExtendAdjust(Widget w, XEvent *event, String *params, 
		Cardinal *num_params);
static void	ExtendEnd(Widget w, XEvent *event, String *params, 
		Cardinal *num_params);
static void TrackMotion(Widget w, XEvent *event, String *params, 
		Cardinal *num_params);
static void TrackFocus(Widget w, XEvent *event, String *params, 
		Cardinal *num_params);
static void HTMLProcessInput(Widget w, XEvent *event, String *params, 
		Cardinal *num_params);
static void HTMLPageUpOrLeft(Widget w, XEvent *event, String *params, 
		Cardinal *num_params);
static void HTMLPageDownOrRight(Widget w, XEvent *event, String *params, 
		Cardinal *num_params);
static void HTMLIncrementUpOrLeft(Widget w, XEvent *event, String *params, 
		Cardinal *num_params);
static void HTMLIncrementDownOrRight(Widget w, XEvent *event, String *params, 
		Cardinal *num_params);
static void HTMLTopOrBottom(Widget w, XEvent *event, String *params, 
		Cardinal *num_params);
static void HTMLTraverseCurrent(Widget w, XEvent *event, String *params,
		Cardinal *num_params);
static void HTMLTraverseNext(Widget w, XEvent *event, String *params,
		Cardinal *num_params);
static void HTMLTraversePrev(Widget w, XEvent *event, String *params,
		Cardinal *num_params);
static void HTMLTraverseNextOrPrev(Widget w, XEvent *event, String *params,
		Cardinal *num_params);

/*** Private Variable Declarations ***/
static XmRepTypeId underline_repid, sb_policy_repid;
static XmRepTypeId sb_placement_repid, string_repid, icon_repid;
static XmRepTypeId enable_repid, conv_repid, load_repid;

/*****
* default translations
* Order of key translations is important: placing c <Key>osfPageUp after
* <key>osfPageUp will mask of the Ctrl key.
* XmHTML explicitly masks off all key modifiers it does not need for a
* certain action. This allows application programmers to use the same keys
* with modifiers for their own purposes and prevents that these key sequences
* are handled by these specific XmHTML action routines.
* This looks ugly, since it's a static block of text it doesn't take up that
* much data space.
*****/
static char translations[] = 
"Ctrl <Key>osfPageUp: page-up-or-left(1)\n\
Ctrl <Key>osfPageDown: page-down-or-right(1)\n\
Ctrl <Key>osfBeginLine: top-or-bottom(0)\n\
Ctrl <Key>osfEndLine: top-or-bottom(1)\n\
~Shift ~Meta ~Alt <Btn1Down>: extend-start() ManagerGadgetArm()\n\
~Shift ~Meta ~Alt <Btn1Motion>: extend-adjust() ManagerGadgetButtonMotion()\n\
~Shift ~Meta ~Alt <Btn1Up>: extend-end(PRIMARY, CUT_BUFFER0) ManagerGadgetActivate() traverse-current()\n\
~Shift ~Meta ~Alt <Btn2Down>:extend-start()\n\
~Shift ~Meta ~Alt <Btn2Motion>: extend-adjust()\n\
~Shift ~Meta ~Alt <Btn2Up>: extend-end(PRIMARY, CUT_BUFFER0)\n\
~Shift ~Meta ~Alt <Key>osfPageUp: page-up-or-left(0)\n\
~Shift ~Meta ~Alt <Key>osfPageDown: page-down-or-right(0)\n\
~Shift ~Meta ~Alt <Key>osfUp: increment-up-or-left(0)\n\
~Shift ~Meta ~Alt <Key>osfLeft: increment-up-or-left(1)\n\
~Shift ~Meta ~Alt <Key>osfDown: increment-down-or-right(0)\n\
~Shift ~Meta ~Alt <Key>osfRight: increment-down-or-right(1)\n\
<Key>osfHelp: ManagerGadgetHelp()\n\
Shift Ctrl <Key>Tab: ManagerGadgetPrevTabGroup()\n\
Ctrl <Key>Tab: ManagerGadgetNextTabGroup()\n\
<Key>Tab: traverse-next()\n\
<Motion>: track-motion()\n\
<Leave>: track-focus()\n\
<FocusIn>: track-focus()\n\
<FocusOut>: track-focus()\n\
<KeyDown>: process-html-input()\n\
<KeyUp>: process-html-input()";

/* Action routines provided by XmHTML */
static XtActionsRec actions[] = 
{
	{"extend-start",            (XtActionProc)ExtendStart},
	{"extend-adjust",           (XtActionProc)ExtendAdjust},
	{"extend-end",              (XtActionProc)ExtendEnd},
	{"page-up-or-left",         (XtActionProc)HTMLPageUpOrLeft},
	{"page-down-or-right",      (XtActionProc)HTMLPageDownOrRight},
	{"increment-up-or-left",    (XtActionProc)HTMLIncrementUpOrLeft},
	{"increment-down-or-right", (XtActionProc)HTMLIncrementDownOrRight},
	{"top-or-bottom",           (XtActionProc)HTMLTopOrBottom},
	{"track-motion",            (XtActionProc)TrackMotion},
	{"track-focus",             (XtActionProc)TrackFocus},
	{"process-html-input",      (XtActionProc)HTMLProcessInput},
	{"traverse-current",        (XtActionProc)HTMLTraverseCurrent},
	{"traverse-next",           (XtActionProc)HTMLTraverseNext},
	{"traverse-prev",           (XtActionProc)HTMLTraversePrev},
	{"traverse-next-or-prev",   (XtActionProc)HTMLTraverseNextOrPrev}
};

/* 
* copy of original list. Motif destroys the original list and therefore
* XmHTML crashes when we try to use the above list again.
*/
static XtActionsRec spareActions[] = 
{
	{"extend-start",            (XtActionProc)ExtendStart},
	{"extend-adjust",           (XtActionProc)ExtendAdjust},
	{"extend-end",              (XtActionProc)ExtendEnd},
	{"page-up-or-left",         (XtActionProc)HTMLPageUpOrLeft},
	{"page-down-or-right",      (XtActionProc)HTMLPageDownOrRight},
	{"increment-up-or-left",    (XtActionProc)HTMLIncrementUpOrLeft},
	{"increment-down-or-right", (XtActionProc)HTMLIncrementDownOrRight},
	{"top-or-bottom",           (XtActionProc)HTMLTopOrBottom},
	{"track-motion",            (XtActionProc)TrackMotion},
	{"track-focus",             (XtActionProc)TrackFocus},
	{"process-html-input",      (XtActionProc)HTMLProcessInput},
	{"traverse-current",        (XtActionProc)HTMLTraverseCurrent},
	{"traverse-next",           (XtActionProc)HTMLTraverseNext},
	{"traverse-prev",           (XtActionProc)HTMLTraversePrev},
	{"traverse-next-or-prev",   (XtActionProc)HTMLTraverseNextOrPrev}
};

/****
* Define the CompositeClassExtension record so we can accept objects.
****/
static CompositeClassExtensionRec htmlCompositeExtension = {
	NULL,									/* next_extension */
	NULLQUARK,								/* record_type */
	XtCompositeExtensionVersion,			/* version */
	sizeof(CompositeClassExtensionRec),		/* record_size */
	True									/* accept_objects */
#if XtSpecificationRelease >= 6
	, False									/* allows_change_managed_set */
#endif
};

/****
* Define the widget class record.
****/
XmHTMLClassRec xmHTMLClassRec = {
											/* core class fields	*/
{
#ifdef MOTIF_IN_DLL
	/*****
	* If Motif is in a DLL on Windows (VC++ 5.0), attempting to initialize
	* this with &xmManagerClassRec gives a compiler error telling us that
	* it's not a constant (there's some indirection going on there).  In
	* that case, we have to initialize this to NULL and set it in
	* XmCreateHTML before calling XtCreateWidget.  This means that
	* XmCreateHTML will be the only valid way to create an XmHTML widget,
	* but that's a small price to pay
	* 				-- DCW, 6 Jul 98
	*****/
	(WidgetClass) NULL,						/* superclass           */
#else
	(WidgetClass) &xmManagerClassRec,		/* superclass			*/
#endif
	"XmHTML",								/* class_name			*/
	sizeof(XmHTMLRec),						/* widget_size			*/
	ClassInitialize,						/* class_initialize	 	*/
	ClassPartInitialize,					/* class_part_init		*/
	FALSE,									/* class_inited		 	*/
	(XtInitProc)Initialize,					/* initialize		 	*/
	NULL,									/* initialize_hook		*/
	XtInheritRealize,						/* realize				*/
	actions,								/* actions				*/
	XtNumber(actions),						/* num_actions			*/
	resources,								/* resources			*/
	XtNumber(resources),					/* num_resources		*/
	NULLQUARK,								/* xrm_class			*/
	TRUE,									/* compress_motion		*/
	XtExposeCompressMaximal,				/* compress_exposure	*/
	TRUE,									/* compress_enterleave 	*/
	FALSE,									/* visible_interest	 	*/
	Destroy,								/* destroy				*/
	(XtWidgetProc)_XmHTMLResize,			/* resize			 	*/
	(XtExposeProc)Redisplay,				/* expose			 	*/
	(XtSetValuesFunc)SetValues,				/* set_values		 	*/
	NULL,									/* set_values_hook		*/
	XtInheritSetValuesAlmost,				/* set_values_almost	*/
	GetValues,								/* get_values_hook		*/
	XtInheritAcceptFocus,					/* accept_focus		 	*/
	XtVersion,								/* version				*/
	NULL,									/* callback_private	 	*/
	translations,							/* tm_table			 	*/
	XtInheritQueryGeometry,					/* query_geometry	 	*/
	XtInheritDisplayAccelerator,			/* display_accelerator	*/
	NULL									/* extension			*/
},
											/* composite_class fields */
{
	GeometryManager, 						/* geometry_manager	 	*/
	NULL,									/* change_managed	 	*/
	XtInheritInsertChild,					/* insert_child		 	*/
	XtInheritDeleteChild,					/* delete_child			*/
	NULL,									/* set by ClassPartInit	*/
},
											/* constraint_class fields */
{
	NULL,									/* resource list		*/	 
	0,										/* num resources		*/	 
	0,										/* constraint size		*/	 
	NULL,									/* init proc			*/	 
	NULL,									/* destroy proc			*/	 
	NULL,									/* set values proc		*/	 
	NULL									/* extension			*/
},
											/* manager_class fields */
{
	XtInheritTranslations,					/* translations			*/
	NULL,									/* syn_resources		*/
	0,										/* num_syn_resources 	*/
	NULL,									/* syn_cont_resources	*/
	0,										/* num_syn_cont_resources*/
	XmInheritParentProcess,					/* parent_process		*/
	NULL									/* extension 			*/	
},
											/* html_class fields */	 
{	
	0										/* none					*/
}	
};

/* Establish the widget class name as an externally accessible symbol. */
WidgetClass xmHTMLWidgetClass = (WidgetClass) &xmHTMLClassRec;

static void
TestRepId(XmRepTypeId id, String name)
{
	if(id == XmREP_TYPE_INVALID)
 		_XmHTMLWarning(__WFUNC__(NULL, "TestRepId"), XMHTML_MSG_8, name);
}

/*****
* Name:			ClassInitialize
* Return Type:	void
* Description:	Called by Intrinsics the first time a widget of this class
*				is instantiated
* In:
*	nothing
* Returns:
*	nothing
*****/
static void
ClassInitialize(void)
{
	static char *enable_models[] = {"automatic", "always", "never"};
	static char *conv_models[] = {"quick", "best", "fast", "slow", "disabled"};
	static char *line_styles[] = {"no_line", "single_line", "double_line",
								"single_dashed_line", "double_dashed_line"};
	static char *load_types[]  = {"normal", "load_normal",
								"progressive", "load_progressive",
								"incremental", "load_incremental",
								"suspend", "load_suspend",
								"abort", "load_abort"};
	static Byte load_values[] = {XmLOAD_NORMAL, XmLOAD_NORMAL,
								XmLOAD_PROGRESSIVE, XmLOAD_PROGRESSIVE,
								XmLOAD_INCREMENTAL, XmLOAD_INCREMENTAL,
								XmLOAD_SUSPEND, XmLOAD_SUSPEND,
								XmLOAD_ABORT, XmLOAD_ABORT};

	/* Usefull sanity: check compile version against current header version */
	if(XmHTMLVersion != VERSION)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "ClassInitialize"),
			"Version number mismatch: this library was compiled as\n"
			"    version %i using XmHTML.h version %i.",
			VERSION, XmHTMLVersion); 
	}

	/* Get appropriate representation type convertor id's */

	/* ScrollBar converters. */
	sb_policy_repid = XmRepTypeGetId(XmCScrollBarDisplayPolicy);
	TestRepId(sb_policy_repid, XmCScrollBarDisplayPolicy);

	sb_placement_repid = XmRepTypeGetId(XmCScrollBarPlacement);
	TestRepId(sb_placement_repid, XmCScrollBarPlacement);

	/* string direction converter */
	string_repid = XmRepTypeGetId(XmCAlignment);
	TestRepId(string_repid, XmCAlignment);

	/* vertical icon alignment */
	icon_repid = XmRepTypeGetId(XmCVerticalAlignment);
	TestRepId(icon_repid, XmCVerticalAlignment);

	/* XmCEnableMode resource class converter */
	XmRepTypeRegister(XmCEnableMode, enable_models, NULL, 3);
	enable_repid = XmRepTypeGetId(XmCEnableMode);
	TestRepId(enable_repid, XmCEnableMode);

	/* XmCConversionMode resource class converter */
	XmRepTypeRegister(XmCConversionMode, conv_models, NULL, 5);
	conv_repid = XmRepTypeGetId(XmCConversionMode);
	TestRepId(conv_repid, XmCConversionMode);

	/* XmCAnchorUnderlineType resource class converter */
	XmRepTypeRegister(XmCAnchorUnderlineType, line_styles, NULL, 5);
	underline_repid = XmRepTypeGetId(XmCAnchorUnderlineType);
	TestRepId(underline_repid, XmCAnchorUnderlineType);

	/* XmCLoadType resource class converter */
	XmRepTypeRegister(XmCLoadType, load_types, load_values, 10);
	load_repid = XmRepTypeGetId(XmCLoadType);
	TestRepId(load_repid, XmCLoadType);

	/* Register the XmNenableBadHTMLWarnings converter */
	XtSetTypeConverter(XmRString, XmRHTMLWarningMode,
		(XtTypeConverter)_XmHTMLCvtStringToWarning, NULL, 0, XtCacheAll, NULL);
}

/*****
* Name:			ClassPartInitialize
* Return Type: 	void
* Description:  object class method to initialize class part structure fields.
* In: 
*	subclass:	pointer to a widget class structure.
* Returns:
*	nothing.
* Note:
*	This routine initializes the Composite extension. XmHTML *must* be a
*	subclass of composite if we want to have it accept any type of Object
*	(including real Objects, Gadgets and Widgets).
*	Kindly donated by Youssef Ouaghli <Youssef.Ouaghli@elec.rma.ac.be>
*****/
static void
ClassPartInitialize(WidgetClass wc)
{
	XmHTMLWidgetClass html_wc = (XmHTMLWidgetClass)wc;

	htmlCompositeExtension.next_extension = html_wc->composite_class.extension;
	htmlCompositeExtension.accepts_objects = True;

	html_wc->composite_class.extension = (XtPointer)&htmlCompositeExtension;
}

/*****
* Name: 		Initialize
* Return Type: 	void
* Description: 	Called when the widget is instantiated
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
	XmHTMLWidget html = (XmHTMLWidget)init;
	XmHTMLWidget req  = (XmHTMLWidget)request;

	/* Initialize the global HTMLpart */

	/* select debug levels */
	_XmHTMLSelectDebugLevels(req->html.debug_levels);
	_XmHTMLSetFullDebug(req->html.debug_full_output);

#ifdef DEBUG
	if(req->html.debug_disable_warnings)
		debug_disable_warnings = True;
	else
		debug_disable_warnings = False;
#endif

	_XmHTMLDebug(1, ("XmHTML.c: Initialize Start\n"));

	/* private widget resources */

	/* Initialize ToolkitAbstraction */
	HTML_ATTR(tka)         = XmHTMLTkaCreate();

	/* Store display */
	XmHTMLTkaSetDisplay(HTML_ATTR(tka), (Widget)html);

	HTML_ATTR(needs_vsb)    = False;
	HTML_ATTR(needs_hsb)    = False;
	HTML_ATTR(scroll_x)     = 0;
	HTML_ATTR(scroll_y)     = 0;

	CheckAnchorUnderlining(html, html);

	/* ScrollBarDisplayPolicy */
	if(!XmRepTypeValidValue(sb_policy_repid, HTML_ATTR(sb_policy), 
		(Widget)html))
		HTML_ATTR(sb_policy) = XmAS_NEEDED;
	else if(HTML_ATTR(sb_policy) == XmSTATIC)
		HTML_ATTR(needs_vsb) = True;

	/* ScrollBarPlacement */
	if(!XmRepTypeValidValue(sb_placement_repid, HTML_ATTR(sb_placement), 
		(Widget)html))
		HTML_ATTR(sb_placement) = XmBOTTOM_RIGHT;

	/* perfectColors */
	if(!XmRepTypeValidValue(enable_repid, HTML_ATTR(perfect_colors),
		(Widget)html))
		HTML_ATTR(perfect_colors) = XmAUTOMATIC;

	/* AlphaChannelProcessing */
	if(!XmRepTypeValidValue(enable_repid, HTML_ATTR(alpha_processing),
		(Widget)html))
		HTML_ATTR(alpha_processing) = XmALWAYS;

	/* ImageRGBConversion */
	if(!XmRepTypeValidValue(conv_repid, HTML_ATTR(rgb_conv_mode),
		(Widget)html) || HTML_ATTR(rgb_conv_mode) == XmDISABLED)
		HTML_ATTR(rgb_conv_mode) = XmBEST;

	/* ImageMapToPalette */
	if(!XmRepTypeValidValue(conv_repid, HTML_ATTR(map_to_palette),
		(Widget)html))
		HTML_ATTR(map_to_palette) = XmDISABLED;

	/* LoadType */
	if(!XmRepTypeValidValue(load_repid, HTML_ATTR(load_type),
		(Widget)html))
		HTML_ATTR(load_type) = XmLOAD_NORMAL;

	/* Vertical icon alignment */
	if(!XmRepTypeValidValue(icon_repid, HTML_ATTR(icon_valign),
		(Widget)html))
		HTML_ATTR(icon_valign) = XmALIGNMENT_CENTER;

	/* repeat delay. Must be positive */
	if(HTML_ATTR(repeat_delay) < 1)
	{
		_XmHTMLWarning(__WFUNC__(init, "Initialize"), XMHTML_MSG_9,
			"XmNrepeatDelay", HTML_ATTR(repeat_delay),
			XmHTML_DEFAULT_REPEAT_DELAY);
		HTML_ATTR(repeat_delay) = XmHTML_DEFAULT_REPEAT_DELAY;
	}

	/* tabwidth, must be a positive, non-zero number */
	if(HTML_ATTR(tabwidth) < 1)
	{
		_XmHTMLWarning(__WFUNC__(init, "Initialize"), XMHTML_MSG_9,
			"XmNtabWidth", HTML_ATTR(tabwidth),
			XmHTML_DEFAULT_TABWIDTH);
		HTML_ATTR(tabwidth) = XmHTML_DEFAULT_TABWIDTH;
	}

	/* Set default text alignment */
	CheckAlignment(html, html);

	/* source input is always complete */
	HTML_ATTR(input_complete) = True;

	/****
	* Initialize private resources.
	****/
	/* Formatted document resources */
	HTML_ATTR(formatted_width)   = 1;
	HTML_ATTR(formatted_height)  = 1;
	HTML_ATTR(elements)          = (XmHTMLObject*)NULL;
	HTML_ATTR(last_element)      = (XmHTMLObject*)NULL;
	HTML_ATTR(formatted)         = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(last_formatted)    = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(paint_start)       = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(paint_end)         = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(nlines)            = 0;
	HTML_ATTR(line_table)        = (XmHTMLLineTable*)NULL;

	/* body image */
	HTML_ATTR(body_image)        = (XmHTMLImage*)NULL;
	HTML_ATTR(body_image_url)    = (String)NULL;

	/* layout & paint engine resources */
	HTML_ATTR(in_layout)         = False;
	HTML_ATTR(paint_x)           = 0;
	HTML_ATTR(paint_y)           = 0;
	HTML_ATTR(paint_width)       = 0;
	HTML_ATTR(paint_height)      = 0;
	HTML_ATTR(body_fg)           = MGR_ATTR(foreground);
	HTML_ATTR(body_bg)           = CORE_ATTR(background_pixel);
	HTML_ATTR(images)            = (XmHTMLImage*)NULL;
	HTML_ATTR(image_maps)        = (XmHTMLImageMap*)NULL;
	HTML_ATTR(xcc)               = (XCC)NULL;
	HTML_ATTR(bg_gc)             = (GC)NULL;
	HTML_ATTR(form_data)         = (XmHTMLFormData*)NULL;
	HTML_ATTR(delayed_creation)  = False;	/* no delayed image creation */
	HTML_ATTR(embedded)          = (XmHTMLExtObj*)NULL;

	/***** 
	* Original colors must be stored. They can be altered by the
	* <BODY> element, so if we get a body without any or some of these
	* colors specified, we can use the proper default values for the 
	* unspecified elements.
	*****/
	HTML_ATTR(body_fg_save)             = HTML_ATTR(body_fg);
	HTML_ATTR(body_bg_save)             = HTML_ATTR(body_bg);
	HTML_ATTR(anchor_fg_save)           = HTML_ATTR(anchor_fg);
	HTML_ATTR(anchor_target_fg_save)    = HTML_ATTR(anchor_target_fg);
	HTML_ATTR(anchor_visited_fg_save)   = HTML_ATTR(anchor_visited_fg);
	HTML_ATTR(anchor_activated_fg_save) = HTML_ATTR(anchor_activated_fg);
	HTML_ATTR(anchor_activated_bg_save) = HTML_ATTR(anchor_activated_bg);

	/* anchor resources */
	HTML_ATTR(anchor_position_x)             = 0;
	HTML_ATTR(anchor_position_y)             = 0;
	HTML_ATTR(anchor_current_cursor_element) = (XmHTMLAnchor*)NULL;
	HTML_ATTR(armed_anchor)                  = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(current_anchor)                = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(num_named_anchors)             = 0;
	HTML_ATTR(anchors)                       = (XmHTMLWord*)NULL;
	HTML_ATTR(anchor_words)                  = 0;
	HTML_ATTR(named_anchors)                 = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(anchor_data)                   = (XmHTMLAnchor*)NULL;
	HTML_ATTR(press_x)                       = 0;
	HTML_ATTR(press_y)                       = 0;
	HTML_ATTR(pressed_time)                  = 0;
	HTML_ATTR(selected_time)                 = 0;
	HTML_ATTR(selected)                      = (XmHTMLAnchor*)NULL;

	/* Text selection resources */
	HTML_ATTR(selection)    = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(select_start) = 0;
	HTML_ATTR(select_end)   = 0;

	/* HTML Frame resources */
	HTML_ATTR(nframes)  = 0;
	HTML_ATTR(frames)   = NULL;
	HTML_ATTR(is_frame) = False;

	/* PLC resources */
	HTML_ATTR(plc_buffer)    = (PLCPtr)NULL;
	HTML_ATTR(num_plcs)      = 0;
	HTML_ATTR(plc_proc_id)   = None;
	HTML_ATTR(plc_suspended) = True;
	HTML_ATTR(plc_gc)        = (GC)NULL;

	/* Table resources */
	HTML_ATTR(tables)        = (XmHTMLTable*)NULL;

	/* HTML4.0 Event database */
	HTML_ATTR(events)      = (HTEvent**)NULL;
	HTML_ATTR(nevents)     = 0;
	HTML_ATTR(body_events) = (AllEvents*)NULL;
	HTML_ATTR(event_mask)  = 0L;

	/* initial mimetype */
	if(!(strcasecmp(HTML_ATTR(mime_type), "text/html")))
		HTML_ATTR(mime_id) = XmPLC_DOCUMENT;
	else if(!(strcasecmp(HTML_ATTR(mime_type), "text/html-perfect")))
		HTML_ATTR(mime_id) = XmPLC_DOCUMENT;
	else if(!(strcasecmp(HTML_ATTR(mime_type), "text/plain")))
		HTML_ATTR(mime_id) = XmHTML_NONE;
	else if(!(strncasecmp(HTML_ATTR(mime_type), "image/", 6)))
		HTML_ATTR(mime_id) = XmPLC_IMAGE;

	/* alpha channel stuff */
	HTML_ATTR(alpha_buffer)  = (AlphaPtr)NULL;

	if(!HTML_ATTR(anchor_track_callback) && !HTML_ATTR(anchor_cursor) &&
		!HTML_ATTR(highlight_on_enter) && !HTML_ATTR(motion_track_callback) &&
		!HTML_ATTR(focus_callback) && !HTML_ATTR(losing_focus_callback))
		HTML_ATTR(need_tracking) = False;
	else
		HTML_ATTR(need_tracking) = True;

	/* verify plc timing intervals */
	_XmHTMLPLCCheckIntervals(html);

	/* Misc. resources */
	HTML_ATTR(gc) = (GC)NULL;

	/* set maximum amount of colors for this display (also creates the XCC) */
	_XmHTMLCheckMaxColorSetting(html);

	/* Create the anchor cursor (if any) */
	if(HTML_ATTR(anchor_display_cursor) && !(HTML_ATTR(anchor_cursor)))
		CreateAnchorCursor(html);

	/* set cursor to None if we don't have to use or have a cursor */
	if(!HTML_ATTR(anchor_display_cursor) || !HTML_ATTR(anchor_cursor))
		HTML_ATTR(anchor_cursor) = None;

	/* Select & initialize appropriate font cache */
	HTML_ATTR(default_font) = _XmHTMLSelectFontCache(html, True);

	/*****
	* if no width or height was specified, default to the width of 20 em
	* (TeX measure for average character width) in the default font and the
	* height of a single line. We need to do this check since the Core
	* initialize() method doesn't do it.
	*****/
	if(CORE_ATTR(width) <= 0)
	{
		unsigned long value = 0;
		if(!(XGetFontProperty(HTML_ATTR(default_font)->xfont, XA_QUAD_WIDTH,
			&value)))
		{
			value = XTextWidth(HTML_ATTR(default_font)->xfont, "m", 1);

			/* sanity for non-ISO fonts */
			if(value <= 0)
				value = 16;
		}
		CORE_ATTR(width) = (Dimension)(20*(Dimension)value +
			2*HTML_ATTR(margin_width));
	}
	if(CORE_ATTR(height) <= 0)
		CORE_ATTR(height) = HTML_ATTR(default_font)->lineheight +
			2*HTML_ATTR(margin_height);

	/*****
	* Now create all private widgets: drawing area and scrollbars.
	* We couldn't do this until we knew for sure the widget dimensions were
	* set; creation of the work_area uses them.
	*****/
	CreateHTMLWidget(html);

	/* Parse the raw HTML text */
	if(ATTR_HTML(req, value))
	{
		HTML_ATTR(source)   = strdup(ATTR_HTML(req, value));
		HTML_ATTR(elements) = _XmHTMLparseHTML(req, NULL, ATTR_HTML(req, value),
								html);

		/* check for frames */
		HTML_ATTR(nframes) = _XmHTMLCheckForFrames(html, HTML_ATTR(elements));

		/* and create them */
		if(!_XmHTMLCreateFrames(NULL, html))
		{
			HTML_ATTR(frames) = NULL;
			HTML_ATTR(nframes) = 0;
		}
		/* Trigger link callback */
		if(HTML_ATTR(link_callback))
			_XmHTMLLinkCallback(html);

		/* do initial document markup */
		_XmHTMLformatObjects(html, html);

		/* check for possible delayed external imagemaps */
		_XmHTMLCheckImagemaps(html);
	}
	else
	{
		HTML_ATTR(source) = (String)NULL;
		HTML_ATTR(elements) = (XmHTMLObject*)NULL;
		HTML_ATTR(nframes) = 0;
		HTML_ATTR(formatted) = (XmHTMLObjectTable*)NULL;
	}

	/* reset scrollbars (this will also resize the work_area) */
	_XmHTMLCheckScrollBars(html);

	/* Final step: add a palette if we must dither */
	if(HTML_ATTR(map_to_palette) != XmDISABLED)
		_XmHTMLAddPalette(html);

	_XmHTMLDebug(1, ("XmHTML.c: Initialize End.\n"));
}

/*****
* Name: 		CreateAnchorCursor
* Return Type: 	void
* Description: 	creates the built-in anchor cursor
* In: 
*	html:		XmHTMLWidget for which to create a cursor
* Returns:
*	nothing.
*****/
static void 
CreateAnchorCursor(XmHTMLWidget html)
{
	_XmHTMLDebug(1, ("XmHTML.c: CreateAnchorCursor Start\n"));

	if(HTML_ATTR(anchor_cursor) == None)
	{
		Pixmap shape, mask;
		XColor white_def, black_def;
		Window window = XtWindow((Widget)html);
		Display *display = XtDisplay((Widget)html);
		Screen *screen = XtScreen((Widget)html);

		if(!window)
			window = RootWindowOfScreen(screen);

		shape = XCreatePixmapFromBitmapData(display, window,
			(char*)fingers_bits, fingers_width, fingers_height, 1, 0, 1);

		mask = XCreatePixmapFromBitmapData(display, window,
			(char*)fingers_m_bits, fingers_m_width, fingers_m_height, 1, 0, 1);

		(void)XParseColor(display, TkaGetColormap(html), "white", &white_def);

		(void)XParseColor(display, TkaGetColormap(html), "black", &black_def);

		HTML_ATTR(anchor_cursor) = XCreatePixmapCursor(display, shape, mask, 
			&white_def, &black_def, fingers_x_hot, fingers_y_hot);
	}
	_XmHTMLDebug(1, ("XmHTML.c: CreateAnchorCursor End\n"));
}

static void
LowerFormWidgets(XmHTMLWidget html)
{
	XmHTMLFormData *form;
	XmHTMLForm *entry;
	XmHTMLExtObj *uentry;

	for(form = HTML_ATTR(form_data); form != NULL; form = form->next)
	{
		for(entry = form->components; entry != NULL; entry = entry->next)
			if(entry->w && entry->mapped)
				XtUnmapWidget(entry->w);
	}
	for(uentry = HTML_ATTR(embedded); uentry != NULL; uentry = uentry->next)
	{
		if(uentry->w && uentry->mapped)
			XtUnmapWidget(uentry->w);
	}
}

void
_XmHTMLRaiseFormWidgets(XmHTMLWidget html)
{
	XmHTMLFormData *form;
	XmHTMLForm *entry;
	XmHTMLExtObj *uentry;

	for(form = HTML_ATTR(form_data); form != NULL; form = form->next)
	{
		for(entry = form->components; entry != NULL; entry = entry->next)
			if(entry->w && entry->mapped)
				XtMapWidget(entry->w);
	}
	for(uentry = HTML_ATTR(embedded); uentry != NULL; uentry = uentry->next)
	{
		if(uentry->w && uentry->mapped)
			XtMapWidget(uentry->w);
	}
}

/*****
* Name:			OverrideExposure
* Return Type: 	void
* Description: 	expose event filter when HTML form widgets are being scrolled.
* In: 
*	w:			unused;
*	client_..:	unused;
*	event:		XEvent info;
*	continu..:	flag to tell X whether or not to propagate this event; 
* Returns:
*	nothing.
* Note:
*	this routine is only activated when XmHTML is moving widgets on it's own
*	display area. It filters out any Exposure events that are generated by
*	moving these widgets around.
*****/
static void
OverrideExposure(Widget w, XtPointer client_data, XEvent *event,
	Boolean *continue_to_dispatch)
{
	if(event->xany.type == Expose || event->xany.type == GraphicsExpose)
	{
		XEvent expose;

#ifdef DEBUG
		_XmHTMLDebug(1, ("XmHTML.c: OverrideExposure, ignoring %s event\n"
			"\t(x = %i, y = %i, width = %i, height = %i)\n",
			(event->xany.type == Expose ? "Expose" : "GraphicsExpose"),
			event->xexpose.x, event->xexpose.y,
			event->xexpose.width, event->xexpose.height)); 
		if(event->xany.type == GraphicsExpose &&
			NULL != XtWindowToWidget(XtDisplay(w),
					event->xgraphicsexpose.drawable))
			_XmHTMLDebug(1, ("XmHTML.c: OverrideExposure, event was destined "
				"for %s (%i events still pending)\n",
				XtName(XtWindowToWidget(XtDisplay(w),
					event->xgraphicsexpose.drawable)),
					event->xgraphicsexpose.count));
#endif
		/*****
		* A GraphicsExpose event is generated for the entire area covered
		* by the form widgets when scrolling: the CopyArea call in
		* _XmHTMLMoveToPos fails 'cause the source is overlapped by
		* the form widgets. Therefore we must allow this event or text
		* will disappear.
		*****/
		if(event->xany.type == GraphicsExpose &&
			event->xgraphicsexpose.count == 0)
			return;
		
		/* Gather all other pending events and disallow continuation */
		while((XCheckWindowEvent(XtDisplay(w), XtWindow(w), ExposureMask, 
				&expose)) == True)
			;
		*continue_to_dispatch = False;
	}
#ifdef DEBUG
	else
		_XmHTMLDebug(1, ("XmHTML.c: OverrideExposure, wrong event %i\n",
			(int)(event->xany.type)));
#endif
}

/*****
* Name: 		_XmHTMLScrollForm
* Return Type: 	void
* Description: 	scrolls all widgets of all forms in the current document.
* In: 
*	html:		XmHTML widget id
* Returns:
*	nothing.
*****/
void
_XmHTMLScrollForm(XmHTMLWidget html)
{
	int x, y, xs, ys;
	XmHTMLFormData *form;
	XmHTMLForm *entry;
	Boolean did_anything = False;
	ToolkitAbstraction *tka = HTML_ATTR(tka);

#ifndef UNMAP_FORMS
	Widget *forms;
	Cardinal nforms;

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLScrollForm, Start\n"));

	XtVaGetValues(HTML_ATTR(work_area),
		XmNchildren, &forms,
		XmNnumChildren, &nforms,
		NULL);

	while(nforms--)
	{
		/* compute new widget position */
		xs = ATTR_CORE(forms[nforms],x) - HTML_ATTR(scroll_x);
		ys = ATTR_CORE(forms[nforms],y) - HTML_ATTR(scroll_y);
		tka->MoveWidget(forms[nforms],
			ATTR_CORE(forms[nforms],x) + xs,
			ATTR_CORE(forms[nforms],y) + ys);
	}

#else
	/*****
	* To prevent the X exposure handling from going haywire, we simply
	* override *any* exposure events generated by moving the widgets 
	* around.
	*****/
	XtInsertEventHandler(HTML_ATTR(work_area), ExposureMask, True,
		(XtEventHandler)OverrideExposure, NULL, XtListHead);

	for(form = HTML_ATTR(form_data); form != NULL; form = form->next)
	{
		for(entry = form->components; entry != NULL; entry = entry->next)
		{
			if(entry->w)
			{
				/* save current widget position */
				x = entry->x;
				y = entry->y;

				/* compute new widget position */
				xs = entry->data->x - HTML_ATTR(scroll_x);
				ys = entry->data->y - HTML_ATTR(scroll_y);

				/* check if we need to show this widget */
				if(xs + entry->width > 0 && xs < HTML_ATTR(work_width) &&
					ys + entry->height > 0 && ys < HTML_ATTR(work_height))
				{
					_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLScrollForm, moving "
						"widget %s from %ix%i to %ix%i (wxh = %ix%i)\n",
						entry->name, entry->x, entry->y, xs, ys,
						entry->width, entry->height));

					/* save new widget position */
					entry->x = xs;
					entry->y = ys;

					/* and move to it */
					tka->MoveWidget(entry->w, xs, ys);

					/* show it */
					if(!entry->mapped)
					{
						tka->SetMappedWhenManaged(entry->w, True);
						entry->mapped = True;
					}
/****
* FIXME!!
* This code is terribly slow for pages with a great deal of form widgets
* in them.
****/
#ifdef UNMAP_FORMS_AND_REFRESH
					/* restore background at previously obscured position */
					_XmHTMLRefresh(html, x, y, entry->width, entry->height);
#endif

					did_anything = True;
				}
				else
				{
					/* hide by unmapping it */
					if(entry->mapped)
					{
						_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLScrollForm, hiding "
							"widget %s\n", entry->name));

						tka->SetMappedWhenManaged(entry->w, False);
						entry->mapped = False;
/****
* FIXME!!
* This code is terribly slow for pages with a great deal of form widgets
* in them.
****/
#ifdef UNMAP_FORMS_AND_REFRESH
						/* restore background at previously obscured position */
						_XmHTMLRefresh(html, x, y, entry->width, entry->height);
#endif

						did_anything = True;
					}
				}
			}
		}
	}
	XtRemoveEventHandler(HTML_ATTR(work_area), ExposureMask, True,
		(XtEventHandler)OverrideExposure, NULL);
#endif
	/* only do this if we actually did something */
	if(did_anything)
	{
		tka->Sync(tka->dpy, False);
		XmUpdateDisplay((Widget)html);
	}

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLScrollForm, End\n"));
}

/*****
* Name: 		_XmHTMLScrollObjects
* Return Type: 	void
* Description: 	scrolls all external objects forms in the current document.
* In: 
*	html:		XmHTML widget id
* Returns:
*	nothing.
*****/
void
_XmHTMLScrollObjects(XmHTMLWidget html)
{
	int x, y, xs, ys;
	XmHTMLExtObj *entry;
	Boolean did_anything = False;
	ToolkitAbstraction *tka = HTML_ATTR(tka);

#ifndef UNMAP_FORMS
	Widget *forms;
	Cardinal nforms;

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLScrollObjects, Start\n"));

	XtVaGetValues(HTML_ATTR(work_area),
		XmNchildren, &forms,
		XmNnumChildren, &nforms,
		NULL);

	while(nforms--)
	{
		/* compute new widget position */
		xs = ATTR_CORE(forms[nforms],x) - HTML_ATTR(scroll_x);
		ys = ATTR_CORE(forms[nforms],y) - HTML_ATTR(scroll_y);
		tka->MoveWidget(forms[nforms],
			ATTR_CORE(forms[nforms],x) + xs,
			ATTR_CORE(forms[nforms],y) + ys);
	}

#else
	/*****
	* To prevent the X exposure handling from going haywire, we simply
	* override *any* exposure events generated by moving the widgets 
	* around.
	*****/
	XtInsertEventHandler(HTML_ATTR(work_area), ExposureMask, True,
		(XtEventHandler)OverrideExposure, NULL, XtListHead);

	for(entry = HTML_ATTR(embedded); entry != NULL; entry = entry->next)
	{
		if(entry->w)
		{
			/* save current widget position */
			x = entry->x;
			y = entry->y;

			/* compute new widget position */
			xs = entry->data->x - HTML_ATTR(scroll_x);
			ys = entry->data->y - HTML_ATTR(scroll_y);

			/* check if we need to show this widget */
			if(xs + entry->width > 0 && xs < HTML_ATTR(work_width) &&
				ys + entry->height > 0 && ys < HTML_ATTR(work_height))
			{
				_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLScrollForm, moving "
					"widget %s from %ix%i to %ix%i (wxh = %ix%i)\n",
					entry->name, entry->x, entry->y, xs, ys,
						entry->width, entry->height));

				/* save new widget position */
				entry->x = xs;
				entry->y = ys;

				/* and move to it */
				tka->MoveWidget(entry->w, xs, ys);

				/* show it */
				if(!entry->mapped)
				{
					tka->SetMappedWhenManaged(entry->w, True);
					entry->mapped = True;
				}
/****
* FIXME!!
* This code is terribly slow for pages with a great deal of form widgets
* in them.
****/
#ifdef UNMAP_FORMS_AND_REFRESH
				/* restore background at previously obscured position */
				_XmHTMLRefresh(html, x, y, entry->width, entry->height);
#endif

				did_anything = True;
			}
			else
			{
				/* hide by unmapping it */
				if(entry->mapped)
				{
					_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLScrollForm, hiding "
						"widget %s\n", entry->name));

					tka->SetMappedWhenManaged(entry->w, False);
					entry->mapped = False;
/****
* FIXME!!
* This code is terribly slow for pages with a great deal of form widgets
* in them.
****/
#ifdef UNMAP_FORMS_AND_REFRESH
					/* restore background at previously obscured position */
					_XmHTMLRefresh(html, x, y, entry->width, entry->height);
#endif

					did_anything = True;
				}
			}
		}
	}
	XtRemoveEventHandler(HTML_ATTR(work_area), ExposureMask, True,
		(XtEventHandler)OverrideExposure, NULL);
#endif
	/* only do this if we actually did something */
	if(did_anything)
	{
		tka->Sync(tka->dpy, False);
		XmUpdateDisplay((Widget)html);
	}

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLScrollForm, End\n"));
}

/*****
* Name: 		ScrollCB
* Return Type: 	void
* Description: 	callback procedure for scrollbar movement
* In: 
*	w:			originator
*	arg1:		client_data, in this case a XmHTMLWidget
*	arg2:		event specific callback structure.
* Returns:
*	nothing
*****/
static void
ScrollCB(Widget w, XtPointer arg1, XtPointer arg2)
{
	XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct *)arg2;
	XEvent *event = cbs->event;
	XmHTMLWidget html = (XmHTMLWidget)arg1;

	/* Fix 05/27/98-01, eb */
	if(event && event->type == MotionNotify && !HTML_ATTR(smooth_scroll))
	{
		/****
		* Try to collapse motion events.
		* In case there are some other motion events in the queue,
		* so ignore this one and let the latter one do the job.
		*****/
		XEvent    next_event ;
		Display  *dpy = HTML_ATTR(tka)->dpy ;

		/****
		* Look for a would-be MotionNotify event in the queue.
		* Use XCheckTypedWindowEvent() instead of XWindowEvent(), as
		* XWindowEvent() blocks if there is no event in the queue.
		* The only drawback is that the event is withdrawn from the queue,
		* and thus we have to put it back.
		*****/
		if(XCheckTypedWindowEvent(dpy, XtWindow(w), event->type, &next_event))
		{
			/* if found, put the event back in the queue and return */
			XPutBackEvent(dpy, &next_event);
			_XmHTMLDebug(1, ("XmHTML.c: ScrollCB, pushed back an outstanding "
				"MotionNotify event.\n"));
			return ;
		}
	}

	_XmHTMLDebug(1, ("XmHTML.c: ScrollCB, calling _XmHTMLMoveToPos\n"));
	_XmHTMLMoveToPos(w, (XmHTMLWidget)arg1, cbs->value);
}

/*****
* Name: 		CreateHTMLWidget
* Return Type: 	void
* Description: 	creates the HTML widget
*				The actual area we use to draw into is a drawingAreaWidget.
* In: 
*	html:		widget to be created.
* Returns:
*	nothing
*****/
static void
CreateHTMLWidget(XmHTMLWidget html)
{
	Arg args[15];
	Dimension argc = 0;
	int vsb_width, hsb_height;
	static XtTranslations trans = NULL;

	_XmHTMLDebug(1, ("XmHTML.c: CreateHTMLWidget Start\n"));

	/* Check if user provided a work area */
	if(HTML_ATTR(work_area) == NULL)
	{
		HTML_ATTR(work_area) = XtVaCreateWidget("workWindow",
			xmDrawingAreaWidgetClass, (Widget)html,
			XmNwidth, CORE_ATTR(width),
			XmNheight, CORE_ATTR(height),
			NULL);
	}
	/* catch all exposure events on the render window */
	XtAddEventHandler((Widget)HTML_ATTR(work_area), ExposureMask, True,
		(XtEventHandler)DrawRedisplay, (XtPointer)html);

	/* we want to know when to handle GraphicsExpose events */
	XtAddEventHandler((Widget)HTML_ATTR(work_area), VisibilityChangeMask, True,
		(XtEventHandler)VisibilityHandler, (XtPointer)html);

	XtAddEventHandler((Widget)html, SubstructureNotifyMask, 
		True, (XtEventHandler)Mapped, (XtPointer)html);

	/* 
	* For some reason, Motif fucks up the original action list, so we
	* need to use a fallback copy instead.
	* Crash happens in XrmStringToQuark().
	*/
	XtAppAddActions(XtWidgetToApplicationContext(HTML_ATTR(work_area)),
		spareActions, XtNumber(spareActions));

	/* add translations for the actions */
	if(trans == NULL)
		trans = XtParseTranslationTable(translations);
	XtSetArg(args[0], XtNtranslations, trans);
	XtSetValues(HTML_ATTR(work_area), args, 1);

	argc = 0;
	XtManageChild(HTML_ATTR(work_area));

	if(HTML_ATTR(vsb) == NULL)
	{
		argc = 0;
		XtSetArg(args[argc], XmNorientation, XmVERTICAL); argc++;
		XtSetArg(args[argc], XmNrepeatDelay, HTML_ATTR(repeat_delay)); argc++;
		/* make them a little bit more responsive */
		XtSetArg(args[argc], XmNinitialDelay, 100); argc++;
		HTML_ATTR(vsb) = XtCreateWidget("verticalScrollBar", 
			xmScrollBarWidgetClass, (Widget)html, args, argc);
	}
	XtManageChild(HTML_ATTR(vsb));
	/* Catch vertical scrollbar movement */
	XtAddCallback(HTML_ATTR(vsb), XmNvalueChangedCallback,
		(XtCallbackProc)ScrollCB, (XtPointer)html);
	XtAddCallback(HTML_ATTR(vsb), XmNdragCallback,
		(XtCallbackProc)ScrollCB, (XtPointer)html);

	if(HTML_ATTR(hsb) == NULL)
	{
		argc = 0;
		XtSetArg(args[argc], XmNorientation, XmHORIZONTAL); argc++;
		XtSetArg(args[argc], XmNrepeatDelay, HTML_ATTR(repeat_delay)); argc++;
		/* make them a little bit more responsive */
		XtSetArg(args[argc], XmNinitialDelay, 100); argc++;
		HTML_ATTR(hsb) = XtCreateWidget("horizontalScrollBar", 
			xmScrollBarWidgetClass, (Widget)html, args, argc);
	}
	XtManageChild(HTML_ATTR(hsb));
	/* Catch horizontal scrollbar movement */
	XtAddCallback(HTML_ATTR(hsb), XmNvalueChangedCallback,
		(XtCallbackProc)ScrollCB, (XtPointer)html);
	XtAddCallback(HTML_ATTR(hsb), XmNdragCallback,
		(XtCallbackProc)ScrollCB, (XtPointer)html);

	/* 
	* subtract margin_width once to minimize number of calcs in
	* the paint routines: every thing rendered starts at an x position
	* of margin_width.
	*/
	_XmHTMLGetScrollDim(html, &hsb_height, &vsb_width);

	HTML_ATTR(work_width) = CORE_ATTR(width) - HTML_ATTR(margin_width) -
								vsb_width;
	HTML_ATTR(work_height)= CORE_ATTR(height);

	/* store window of *drawingarea* in the toolkit abstraction */
	XmHTMLTkaSetDrawable(HTML_ATTR(tka),
		XtWindow(HTML_ATTR(work_area)));

	_XmHTMLDebug(1, ("XmHTML.c: CreateHTMLWidget End\n"));
	return;
}

/*****
* Name: 		_XmHTMLGetScrollDim
* Return Type: 	void
* Description: 	retrieves width & height of the scrollbars
* In: 
*	html:		XmHTMLWidget for which to retrieve these values
*	hsb_height: thickness of horizontal scrollbar, filled upon return
*	vsb_width:	thickness of vertical scrollbar, filled upon return
* Returns:
*	nothing
* Note:
*	I had a nicely working caching version of this routine under Linux & 
*	Motif 2.0.1, but under HPUX with 1.2.0 this never worked. This does.
*****/
void
_XmHTMLGetScrollDim(XmHTMLWidget html, int *hsb_height, int *vsb_width)
{
	Arg args[1];
	Dimension height = 0, width = 0;

	if(HTML_ATTR(hsb))
	{
#ifdef NO_XLIB_ILLEGAL_ACCESS
		XtSetArg(args[0], XmNheight, &height);
		XtGetValues(HTML_ATTR(hsb), args, 1);
#else
		height = ATTR_CORE(HTML_ATTR(hsb), height);
#endif

		/*
		* Sanity check if the scrollbar dimensions exceed the widget dimensions
		* Not doing this would lead to X Protocol errors whenever text is set
		* into the widget: the size of the workArea will be less than or equal
		* to zero when scrollbars are required.
		* We always need to do this check since it's possible that some
		* user has been playing with the dimensions of the scrollbars.
		*/
		if(height >= CORE_ATTR(height))
		{
			_XmHTMLWarning(__WFUNC__(HTML_ATTR(hsb), "_XmHTMLGetScrollDim"),
				XMHTML_MSG_11, "Height", "horizontal", height, "height",
				CORE_ATTR(height), 15);
			height = 15;
			XtSetArg(args[0], XmNheight, height);
			XtSetValues(HTML_ATTR(hsb), args, 1);
		}
	}

	if(HTML_ATTR(vsb))
	{
#ifdef NO_XLIB_ILLEGAL_ACCESS
		XtSetArg(args[0], XmNwidth, &width);
		XtGetValues(HTML_ATTR(vsb), args, 1);
#else
		width = ATTR_CORE(HTML_ATTR(vsb), width);
#endif

		if(width >= CORE_ATTR(width))
		{
			_XmHTMLWarning(__WFUNC__(HTML_ATTR(vsb), "_XmHTMLGetScrollDim"),
				XMHTML_MSG_11, "Width", "vertical", width, "width",
				CORE_ATTR(width), 15);
			width  = 15;
			XtSetArg(args[0], XmNwidth, width);
			XtSetValues(HTML_ATTR(vsb), args, 1);
		}
	}

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLGetScrollDim; height = %i, width = %i\n",
		height, width));

	*hsb_height = height;
	*vsb_width  = width;
}

/*****
* Name: 		_XmHTMLCheckScrollBars
* Return Type: 	void
* Description: 	(re)configures scrollbars
* In: 
*	html:		HTML widget to configure
* Returns:
*	nothing.
*****/
void
_XmHTMLCheckScrollBars(XmHTMLWidget html)
{
	int dx, dy, hsb_height, vsb_width, st;
	Boolean hsb_on_top, vsb_on_left;
	/* forced display of scrollbars: XmSTATIC or frames with scrolling = yes */
	Boolean force_vsb = False, force_hsb = False;
	Arg args[10];
	Dimension argc = 0;
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLCheckScrollBars, start\n"));

	/* don't do a thing if we aren't managed yet */
	if(!(tka->IsManaged((Widget)html)))
		return;

	/* Initial work area offset */
	st = dx = dy = MGR_ATTR(shadow_thickness);
	_XmHTMLGetScrollDim(html, &hsb_height, &vsb_width);

 	/* check if we need a vertical scrollbar */
	if(HTML_ATTR(formatted_height) < CORE_ATTR(height))
	{
		HTML_ATTR(needs_vsb) = False;
		/* don't forget! */
		HTML_ATTR(scroll_y) = 0;
		tka->UnmanageChild(HTML_ATTR(vsb));
	}
	else
		HTML_ATTR(needs_vsb) = True;

	/* add a scrollbar if we must and it isn't already here */
	if(!HTML_ATTR(needs_vsb) && HTML_ATTR(sb_policy) == XmSTATIC)
	{
		HTML_ATTR(needs_vsb) = True;
		force_vsb = True;
	}

	/*
	* check if we need a horizontal scrollbar. If we have a vertical
	* scrollbar, we must add it's width or text might be lost.
	*/
	if(HTML_ATTR(formatted_width) < CORE_ATTR(width)  -
		(HTML_ATTR(needs_vsb) ? vsb_width : 0)) /* fix 04/27/97-01, kdh */
	{
		HTML_ATTR(needs_hsb) = False;
		/* don't forget! */
		HTML_ATTR(scroll_x) = 0;
		tka->UnmanageChild(HTML_ATTR(hsb));
	}
	else
		HTML_ATTR(needs_hsb) = True;

	/* add a scrollbar if we must and it isn't already here */
	if(!HTML_ATTR(needs_hsb) && HTML_ATTR(sb_policy) == XmSTATIC)
	{
		HTML_ATTR(needs_hsb) = True;
		force_hsb = True;
	}

	/* if this is a frame, check what type of scrolling is requested */
	if(HTML_ATTR(is_frame))
	{
		if(HTML_ATTR(scroll_type) == FRAME_SCROLL_NONE)
		{
			HTML_ATTR(needs_hsb) = False;
			HTML_ATTR(needs_vsb) = False;
			HTML_ATTR(scroll_x) = 0;
			HTML_ATTR(scroll_y) = 0;
			tka->UnmanageChild(HTML_ATTR(hsb));
			tka->UnmanageChild(HTML_ATTR(vsb));
		}
		else if(HTML_ATTR(scroll_type) == FRAME_SCROLL_YES)
		{
			HTML_ATTR(needs_vsb) = True;
			HTML_ATTR(needs_hsb) = True;
			force_vsb = True;
			force_hsb = True;
		}
		/* else scrolling is auto, just proceed */
	}

	/* return if we don't need any scrollbars */
	if(!HTML_ATTR(needs_hsb) && !HTML_ATTR(needs_vsb))
	{
		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLCheckScrollBars, end, no bars "
			"needed.\n"));
		/* move work_area to it's correct position */
		tka->ConfigureWidget(HTML_ATTR(work_area), dx, dy, CORE_ATTR(width),
			CORE_ATTR(height), ATTR_CORE(HTML_ATTR(work_area), border_width));
		return;
	}

	/* see if we have to put hsb on top */
	hsb_on_top = (HTML_ATTR(sb_placement) == XmTOP_LEFT ||
		HTML_ATTR(sb_placement) == XmTOP_RIGHT);
	/* see if we have top put vsb on left */
	vsb_on_left = (HTML_ATTR(sb_placement) == XmTOP_LEFT ||
		HTML_ATTR(sb_placement) == XmBOTTOM_LEFT);

	/* horizontal sb on top */
	if(HTML_ATTR(needs_hsb) && hsb_on_top)
		dy += hsb_height;

	/* vertical sb on left */
	if(HTML_ATTR(needs_vsb) && vsb_on_left)
		dx += vsb_width;

	/* move work_area to it's correct position */
	tka->MoveWidget(HTML_ATTR(work_area), dx, dy);

	/* See what space we have to reserve for the scrollbars */
	if(HTML_ATTR(needs_hsb) && hsb_on_top == False)
		dy += hsb_height;
	if(HTML_ATTR(needs_vsb) && vsb_on_left == False)
		dx += vsb_width;

	tka->ResizeWidget(HTML_ATTR(work_area), 
		CORE_ATTR(width) - dx, CORE_ATTR(height) - dy, 
		ATTR_CORE(HTML_ATTR(work_area), border_width)); 

	if(HTML_ATTR(needs_hsb) == True)
	{
		int pinc;

		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLCheckScrollBars, setting hsb\n"));

		/* Set hsb size; adjust x-position if we have a vsb */
		dx = (HTML_ATTR(needs_vsb) ? vsb_width : 0);
		tka->ResizeWidget(HTML_ATTR(hsb),
			CORE_ATTR(width) - dx - 2*st,
			ATTR_CORE(HTML_ATTR(hsb), height),
			ATTR_CORE(HTML_ATTR(hsb), border_width));

		/* pageIncrement == sliderSize */
		pinc = HTML_ATTR(work_width) - 2*(HTML_ATTR(default_font) ? 
				HTML_ATTR(default_font)->m_width :
				XmHTML_HORIZONTAL_SCROLL_INCREMENT);
		/* sanity check */
		if(pinc < 1)
			pinc = XmHTML_HORIZONTAL_SCROLL_INCREMENT;

		/* adjust horizontal scroll if necessary */
		if(HTML_ATTR(scroll_x) > HTML_ATTR(formatted_width) - pinc)
			HTML_ATTR(scroll_x) = HTML_ATTR(formatted_width) - pinc;
		/* fix 01/23/97-02, kdh */

		/*
		* Adjust if a horizontal scrollbar has been forced
		* (can only happen for frames with scrolling = yes)
		*/
		if(force_hsb && pinc > HTML_ATTR(formatted_width))
		{
			pinc = HTML_ATTR(formatted_width);
			HTML_ATTR(scroll_x) = 0;
		}

		argc = 0;
		XtSetArg(args[argc], XmNminimum, 0); argc++;
		XtSetArg(args[argc], XmNmaximum, HTML_ATTR(formatted_width)); argc++;
		XtSetArg(args[argc], XmNvalue, HTML_ATTR(scroll_x)); argc++;
		XtSetArg(args[argc], XmNsliderSize, pinc); argc++;
		XtSetArg(args[argc], XmNincrement, (HTML_ATTR(default_font) ? 
					HTML_ATTR(default_font)->m_width :
					XmHTML_HORIZONTAL_SCROLL_INCREMENT));
		argc++;
		XtSetArg(args[argc], XmNpageIncrement, pinc); argc++;
		XtSetValues(HTML_ATTR(hsb), args, argc);

		/* adjust x-position if vsb is on left */
 		dx = (HTML_ATTR(needs_vsb) && vsb_on_left ? vsb_width : 0);

		/* place it */
		if(hsb_on_top)
			tka->MoveWidget(HTML_ATTR(hsb), dx, 0);
		else
			tka->MoveWidget(HTML_ATTR(hsb), dx,
				(CORE_ATTR(height) - hsb_height));
	}
	if(HTML_ATTR(needs_vsb) == True)
	{
		int pinc;
		
		_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLCheckScrollBars, setting vsb\n"));

		/* Set vsb size; adjust y-position if we have a hsb */
		dy = (HTML_ATTR(needs_hsb) ? hsb_height : 0);
		tka->ResizeWidget(HTML_ATTR(vsb), 
			ATTR_CORE(HTML_ATTR(vsb), width),
			CORE_ATTR(height) - dy - 2*st,
			ATTR_CORE(HTML_ATTR(vsb), border_width));

		/* pageIncrement == sliderSize */
		pinc = HTML_ATTR(work_height) - 2*(HTML_ATTR(default_font) ? 
			HTML_ATTR(default_font)->height : XmHTML_VERTICAL_SCROLL_INCREMENT);
		/* sanity check */
		if(pinc < 1)
			pinc = XmHTML_VERTICAL_SCROLL_INCREMENT;

		/* adjust vertical scroll if necessary */
		if(HTML_ATTR(scroll_y) > HTML_ATTR(formatted_height) - pinc)
			HTML_ATTR(scroll_y) = HTML_ATTR(formatted_height) - pinc;

		/*
		* Adjust if a vertical scrollbar has been forced
		* (can only happen if scrollBarDisplayPolicy == XmSTATIC)
		*/
		if(force_vsb && pinc > HTML_ATTR(formatted_height))
		{
			pinc = HTML_ATTR(formatted_height);
			HTML_ATTR(scroll_y) = 0;
		}

		argc = 0;
		XtSetArg(args[argc], XmNminimum, 0); argc++;
		XtSetArg(args[argc], XmNmaximum, HTML_ATTR(formatted_height)); argc++;
		XtSetArg(args[argc], XmNvalue, HTML_ATTR(scroll_y)); argc++;
		XtSetArg(args[argc], XmNsliderSize, pinc); argc++;
		XtSetArg(args[argc], XmNincrement, (HTML_ATTR(default_font) ? 
				HTML_ATTR(default_font)->height :
				XmHTML_VERTICAL_SCROLL_INCREMENT)); argc++;
		XtSetArg(args[argc], XmNpageIncrement, pinc); argc++;
		XtSetValues(HTML_ATTR(vsb), args, argc);

		/* adjust y-position if hsb is on top */
 		dy = (HTML_ATTR(needs_hsb) && hsb_on_top ? hsb_height : 0);

		/* place it */
		if(vsb_on_left)
			tka->MoveWidget(HTML_ATTR(vsb), 0, dy);
		else
			tka->MoveWidget(HTML_ATTR(vsb), (CORE_ATTR(width)- vsb_width), dy);
	}
	_XmHTMLDebug(1, ("XmHTML.c: _XmHTMLCheckScrollBars, end\n"));
}

/*****
* Name: 		Mapped
* Return Type: 	void
* Description: 	event handler for CreateNotify events.
* In: 
*	w:			owner of this eventhandler 
*	html:		client data, XmHTMLWidget to which w belongs
*	event:		CreateNotify event data
* Returns:
*	nothing
* Note:
*	We want to be notified when the window gets created. Motif seems to block
*	the CreateNotify event, so we work with the MapNotify event. This is
*	required to get the text rendered correctly when it has been
*	set inside the Xt[Va]Create[Managed]Widget and before XtRealizeWidget
*	has been called: we do not have a window yet and thus no valid gc. Bad 
*	things happen otherwise.
*****/
/*ARGSUSED*/
static void
Mapped(Widget w, XmHTMLWidget html, XEvent *event)
{
	/* wrong event, just return */
	if(event->type != MapNotify)
		return;

	_XmHTMLDebug(1, ("XmHTML.c: Mapped start\n"));

	_XmHTMLDebug(1, ("XmHTML.c: Mapped, work area dimensions: %ix%i\n",
		HTML_ATTR(work_width), HTML_ATTR(work_height)));

	/* store window of *drawingarea* in the toolkit abstraction */
	XmHTMLTkaSetDrawable(HTML_ATTR(tka), XtWindow(HTML_ATTR(work_area)));

	/* create a gc */
	_XmHTMLCheckGC(html);

	/* save new height */
	HTML_ATTR(work_height) = CORE_ATTR(height);
	/* and width as well, fix 10/26/97-01, kdh */
	HTML_ATTR(work_width) = CORE_ATTR(width) - HTML_ATTR(margin_width) -
							ATTR_CORE(HTML_ATTR(vsb), width); 

	_XmHTMLDebug(1, ("XmHTML.c: Mapped, new work area dimensions: %ix%i\n",
		HTML_ATTR(work_width), HTML_ATTR(work_height)));

	/* configure the scrollbars, will also resize work_area */
	_XmHTMLCheckScrollBars(html);

	_XmHTMLLayout(html);

	/* no longer needed now, so remove it */ 
	XtRemoveEventHandler(w, SubstructureNotifyMask, True,
		(XtEventHandler)Mapped, (XtPointer)html); 	

	/*****
	* IRIX seems to have problems with this. Trigger a Redisplay by
	* clearing the entire area.
	*****/
#ifdef __sgi
	_XmHTMLClearArea(html, 0, 0, CORE_ATTR(width), CORE_ATTR(height));
#endif

	_XmHTMLDebug(1, ("XmHTML.c: Mapped end.\n"));
}

/*****
* Name:			VisibilityHandler
* Return Type: 	void
* Description: 	VisibilityChangeMask event handler. Used to store the
*				visibility state of the work_area so we know when to
*				serve or ignore GraphicsExpose requests: if we're partially
*				obscured we need to respond to them, in all other cases we
*				can ignore them.
* In: 
*	w:			owner of this eventhandler 
*	html:		client data, XmHTMLWidget to which w belongs
*	event:		VisibilityNotify event data
* Returns:
*	nothing, but sets the visibility field in the widget's instance structure.
*****/
/*ARGSUSED*/
static void
VisibilityHandler(Widget w, XmHTMLWidget html, XEvent *event)
{
	if(event->type != VisibilityNotify)
		return;

	_XmHTMLDebug(1, ("XmHTML.c: VisibilityHandler start\n"));

	HTML_ATTR(visibility) = event->xvisibility.state;

	_XmHTMLDebug(1, ("XmHTML.c: VisibilityHandler end\n"));
}

/*****
* Name: 		_XmHTMLAutoSizeWidget
* Return Type: 	void
* Description: 	computes XmHTML's widget dimensions if we have to autosize
*				in either direction.
* In: 
*	html:		XmHTMLWidget id
* Returns:
*	nothing.
* Note:
*	This routine is fairly complicated due to the fact that the dimensions
*	of the work area are partly determined by the presence of possible
*	scrollbars.
*****/
void
_XmHTMLAutoSizeWidget(XmHTMLWidget html)
{
	int max_w, max_h, width, height, core_w, core_h;
	int hsb_height = 0, vsb_width = 0, h_reserved, w_reserved;
	Boolean done = False, granted = False, has_vsb = False, has_hsb = False;
	Dimension new_h, new_w, width_return, height_return;
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	/* get dimensions of the scrollbars */
	_XmHTMLGetScrollDim(html, &hsb_height, &vsb_width);

	/* maximum allowable widget height: 80% of screen height */
	max_h = (int)(0.8*tka->height);

	/* make a guess at the initial widget width */
	max_w = _XmHTMLGetMaxLineLength(html) + 2*HTML_ATTR(margin_width);

	/* save original widget dimensions in case our resize request is denied */
	core_w = CORE_ATTR(width);
	core_h = CORE_ATTR(height);

	/* set initial dimensions */
	height = (core_h > max_h ? max_h : core_h);
	width  = max_w;

	/*
	* Since we are making geometry requests, we need to compute the total
	* width and height required to make all text visible.
	* If we succeed, we don't require any scrollbars to be present.
	* This does complicate things considerably.
	* The dimensions of the work area are given by the widget dimensions
	* minus possible margins and possible scrollbars.
	*/
	h_reserved = HTML_ATTR(margin_height) + hsb_height;
	w_reserved = 2*HTML_ATTR(margin_width)  + vsb_width;

	do
	{
		/* work_width *always* includes room for a vertical scrollbar */
		HTML_ATTR(work_width) = width - w_reserved;

		/* Check if we need to add a vertical scrollbar. */
		if(height - h_reserved > max_h)
			has_vsb = True;
		else /* no vertical scrollbar needed */
			has_vsb = False;
	
		_XmHTMLDebug(1, ("XmHTML.c: autoSizeWidget, initial dimension: "
			"%ix%i. has_vsb: %s\n", width, height,
			has_vsb ? "yes" : "no"));

		/* Compute new screen layout. */
		_XmHTMLComputeLayout(html);

		/*
		* We have made a pass on the document, so we know now the formatted 
		* dimensions. If the formatted width exceeds the maximum allowable
		* width, we need to add a horizontal scrollbar, and if the formatted
		* height exceeds the maximum allowable height we need to add a
		* vertical scrollbar. Order of these checks is important: if a vertical
		* scrollbar is present, the width of the vertical scrollbar must be
		* added as well.
		* formatted_height includes the vertical margin twice.
		* formatted_width includes the horizontal margin once.
		*/

		/* higher than available height, need a vertical scrollbar */
		if(HTML_ATTR(formatted_height) > max_h)
		{
			has_vsb    = True;
			height     = max_h;
		}
		else
		{
			has_vsb    = False;
			height     = HTML_ATTR(formatted_height);
		}

		/* wider than available width, need a horizontal scrollbar */
		if(HTML_ATTR(formatted_width) + HTML_ATTR(margin_width) > max_w)
		{
			has_hsb    = True;
			width      = max_w;
		}
		else
		{
			has_hsb    = False;
			width      = HTML_ATTR(formatted_width) + HTML_ATTR(margin_width);
		}

		/* add width of vertical scrollbar if we are to have one */
		if(has_vsb)
			width += vsb_width;

		/*
		* With the above checks we *know* width and height are positive
		* integers smaller than 2^16 (max value of an unsigned short), so we
		* don't have to check for a possible wraparound of the new dimensions.
		*/
		new_h = (Dimension)height;
		new_w = (Dimension)width;
		width_return  = 0;
		height_return = 0;

		_XmHTMLDebug(1, ("XmHTML.c: autoSizeWidget, geometry request with "
			"dimensions: %hix%hi. has_vsb = %s, has_hsb = %s\n", new_w, new_h,
			has_vsb ? "yes" : "no", has_hsb ? "yes" : "no"));

		/* make the resize request and check return value */
		switch(XtMakeResizeRequest((Widget)html, new_w, new_h,
			&width_return, &height_return))
		{
			case XtGeometryAlmost:
				/*
				* partially granted. Set the returned width and height
				* as the new widget dimensions and recompute the
				* widget layout. The next time the resizeRequest is made
				* it *will* be granted.
				*/
				width = (int)width_return;
				height= (int)height_return;
				break;
			case XtGeometryNo:
				/* revert to original widget dimensions */
				new_h = core_h;
				new_w = core_w;
				granted = False;
				done    = True;
				break;
			case XtGeometryYes:
				/* Resize request was granted. */
				granted = True;
				done    = True;
				break;
			default:	/* not reached, XtGeometryDone is never returned */
				done = True;
				break;
		}
	}
	while(!done);

	CORE_ATTR(width)  = new_w;
	CORE_ATTR(height) = HTML_ATTR(work_height) = new_h;
	/* work_width *always* includes room for a vertical scrollbar */
	HTML_ATTR(work_width) = new_w - w_reserved;

	/* Make sure scrollbars don't appear when they are not needed. */
	if(!has_hsb && granted)
		HTML_ATTR(formatted_height) = new_h - HTML_ATTR(margin_height) -
			hsb_height - 1;
	if(!has_vsb && granted)
		HTML_ATTR(formatted_width) = new_w - 1;

	/*
	* If a vertical scrollbar is present, CheckScrollBars will add a horizontal
	* scrollbar if the formatted_width + vsb_width exceeds the widget width.
	* To make sure a horizontal scrollbar does not appear when one is not
	* needed, we need to adjust the formatted width accordingly.
	*/
	if(has_vsb && granted)
		HTML_ATTR(formatted_width) -= vsb_width; 

	/* 
	* If our resize request was denied we need to recompute the text
	* layout using the original widget dimensions. The previous layout is
	* invalid since it used guessed widget dimensions instead of the previous
	* dimensions and thus it will look bad if we don't recompute it.
	*/
	if(!granted)
		_XmHTMLComputeLayout(html);

	_XmHTMLDebug(1, ("XmHTML.c: autoSizeWidget, results:\n"
		"\tRequest granted: %s\n"
		"\tcore height = %i, core width = %i, work_width = %i\n"
		"\tformatted_width = %i, formatted_height = %i.\n"
		"\thas_vsb = %s, has_hsb = %s\n",
		granted ? "yes" : "no",
		CORE_ATTR(height), CORE_ATTR(width), HTML_ATTR(work_width),
		HTML_ATTR(formatted_width), HTML_ATTR(formatted_height),
		has_vsb ? "yes" : "no", has_hsb ? "yes" : "no"));
}

/*****
* Name: 		DrawRedisplay
* Return Type: 	void
* Description: 	Eventhandler for exposure events on the work_area
* In: 
*	w:			owner of this eventhandler 
*	html:		client data, XmHTMLWidget to which w belongs
*	event:		expose event data.
* Returns:
*	nothing
* Note:
*	This routine makes a rough guess on which ObjectTable elements
*	should be used as vertical start and end points for the paint engine.
*	Finetuning is done by the DrawText routine in paint.c, which uses
*	the paint_x, paint_y, paint_width and paint_height fields in the
*	htmlRec to determine what should be painted exactly.
*****/
static void
DrawRedisplay(Widget w, XmHTMLWidget html, XEvent *event)
{
	/* 
	* must use int for y-positions. The Position and Dimension typedefs
	* are shorts, which may produce bad results if the scrolled position
	* exceeds the size of a short
	*/
	int y1, y2, height, x1, x2, width;
	XEvent expose;

	_XmHTMLDebug(1, ("XmHTML.c: DrawRedisplay Start\n"));

	/*****
	* No needless exposures. Kick out graphics exposures, I don't know
	* who invented these, sure as hell don't know what to do with them...
	*
	* Update August 26: I do know now what to do with these suckers:
	* they are generated whenever a XCopyArea or XCopyPlane request couldn't
	* be completed 'cause the destination area is (partially) obscured.
	* This happens when some other window is placed over our display area.
	* So when we get a GraphicsExpose event, we check our visibility state
	* and only draw something when we are partially obscured: when we are
	* fully visibile we won't get any GraphicsExpose events, and when we
	* are fully obscured we won't even get Expose Events.
	* The reason behind all of this are the images & anchor drawing: sometimes
	* they overlap an already painted area, and drawing will then generate
	* a GraphicsExpose, which in turn will trigger a redisplay of these anchors
	* and then it starts all over again. Ergo: bad screen flickering. And we
	* DO NOT want this.
	*****/
	if(((event->xany.type != Expose) && (event->xany.type != GraphicsExpose))
		|| HTML_ATTR(formatted) == NULL || HTML_ATTR(nframes))
	{
		/* display scrollbars if we are in a frame */
		if(HTML_ATTR(is_frame))
			_XmHTMLSetScrollBars(html);
		_XmHTMLDebug(1, ("XmHTML.c: DrawRedisplay End: wrong event "
			"(%i).\n", event->xany.type));
		return;
	}
	if(event->xany.type == GraphicsExpose &&
		HTML_ATTR(visibility) != VisibilityPartiallyObscured &&
		!HTML_ATTR(form_data))
	{
		_XmHTMLDebug(1, ("XmHTML.c: DrawRedisplay End: bad GraphicsExpose, "
			"window not partially obscured.\n"));
		return;
	}

	x1 = event->xexpose.x;
	y1 = event->xexpose.y;
	width = event->xexpose.width;
	height = event->xexpose.height;
	x2 = x1 + width;

	_XmHTMLDebug(1, ("XmHTML.c: DrawRedisplay, y-position of region: %i, "
		"height of region: %i\n", y1, height));

	_XmHTMLDebug(1, ("XmHTML.c: DrawRedisplay, event type: %s\n",
		event->xany.type == Expose ? "Expose" : "GraphicsExpose"));

	_XmHTMLDebug(1, ("XmHTML.c: DrawRedisplay %i Expose events waiting.\n",
		event->xexpose.count));

	/*
	* coalesce multiple expose events into one.
	*/
	while((XCheckWindowEvent(XtDisplay(w), XtWindow(w), ExposureMask, 
			&expose)) == True)
	{
		int dx, dy, dh, dw;

		/*****
		* GraphicsExpose events are only honored when we are partially
		* obscured.
		*****/
		if(expose.xany.type == NoExpose ||
			(event->xany.type == GraphicsExpose &&
			HTML_ATTR(visibility) != VisibilityPartiallyObscured))
			continue;

		dx = expose.xexpose.x;
		dy = expose.xexpose.y;
		dw = expose.xexpose.width;
		dh = expose.xexpose.height;

		_XmHTMLDebug(1, ("XmHTML.c: DrawRedisplay, next event, geometry of "
			"exposed region: %ix%i:%i,%i\n", dx, dy, dw, dh));

		/* right side of region */
		x2 = x1 + width;

		/* leftmost x-position of exposure region */
		if(x1 > dx)
			x1 = dx;

		/* rightmost x-position of exposure region */
		if(x2 < (dx + dw))
			x2 = dx + dw;

		/* width of exposure region */
		width = x2 - x1;

		/* bottom of region */
		y2 = y1 + height;

		/* topmost y-position of exposure region */
		if(y1 > dy)
			y1 = dy;

		/* bottommost y-position of exposure region */
		if(y2 < (dy + dh))
			y2 = dy + dh;

		/* height of exposure region */
		height = y2 - y1;
	}

	_XmHTMLDebug(1, ("XmHTML.c: DrawRedisplay, total region geometry: "
		"%ix%i:%i,%i.\n", x1, y1, width, height));

	_XmHTMLRefresh(html, x1, y1, width, height);

	_XmHTMLDebug(1, ("XmHTML.c: DrawRedisplay End\n"));
}

/*****
* Name: 		Redisplay
* Return Type: 	void
* Description: 	xmHTMLWidgetClass expose method.
* In: 
*	w:			widget to expose
*	event:		description of event that triggered an expose
*	region:		region to display.
* Returns:
*	nothing
*****/
static void 
Redisplay(Widget w, XEvent *event, Region region)
{
	_XmHTMLDebug(1, ("XmHTML.c: Redisplay Start\n"));

	/* Pass exposure events down to the children */
	_XmRedisplayGadgets(w, (XEvent*)event, region);

	_XmHTMLDebug(1, ("XmHTML.c: Redisplay End\n"));
	return;
}

/*****
* Name: 		SetValues
* Return Type: 	Boolean
* Description: 	xmHTMLWidgetClass SetValues method.
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
SetValues(Widget current, Widget request, Widget set,
	ArgList args, Cardinal *num_args)
{
	XmHTMLWidget w_curr = (XmHTMLWidget)current;
	XmHTMLWidget w_req  = (XmHTMLWidget)request;
	XmHTMLWidget w_new  = (XmHTMLWidget)set;

	Boolean redraw = False, parse = False;
	Boolean need_reformat = False;
	Boolean need_layout = False;
	Boolean free_images = False;

	unsigned long event_to_check = 0L;

	/* fix 06/17/97-01, aj */
	int i;	
	int valueReq = False;

	_XmHTMLDebug(1, ("XmHTML.c: SetValues Start\n"));

	/*****
	* Note on processing of the onLoad/onUnload events.
	* For the onLoad event, the spec states the following:
	* ``The onload event occurs when the user agent finished loading a
	* window or all frames within a frameset''.
	* Therefore the onLoad event will be the last action to be carried
	* out when a new source has been supplied.
	*
	* For the onUnload event: the spec doesn't state when this event
	* should be executed. We take the easy way out and execute this event
	* right before the document is unloaded.
	*****/


#ifdef DEBUG
	if(ATTR_HTML(w_req, debug_levels) != ATTR_HTML(w_curr, debug_levels))
	{
		_XmHTMLSelectDebugLevels(ATTR_HTML(w_req, debug_levels));
		ATTR_HTML(w_new, debug_levels) = ATTR_HTML(w_req, debug_levels);
	}
	_XmHTMLSetFullDebug(ATTR_HTML(w_req, debug_full_output));

	if(ATTR_HTML(w_req, debug_disable_warnings))
		debug_disable_warnings = True;
	else
		debug_disable_warnings = False;
#endif

	/*****
	* We always use a copy of the HTML source text that is set into
	* the widget to prevent a crash when the user has freed it before we
	* had a chance of parsing it, and we ensure new text will get set
	* properly.
	*
	* fix 06/17/97-01, aj
	* Patch to fix clearing if doing setvalues without XmNvalue  
	* Determine if we have a set value request and only check new source
	* if it has been supplied explicitly.
	*
	* Addition 10/10/97, kdh: changing the palette at run-time is *never*
	* allowed.
	*****/
	for(i = 0; i < *num_args; i++)
	{
		if(!strcmp(XmNvalue, args[i].name))
			valueReq = True;

		/* Check for read-only resources */
		if(!strcmp(XmNimagePalette, args[i].name) ||
			!strcmp(XmNhorizontalScrollBar, args[i].name) ||
			!strcmp(XmNverticalScrollBar, args[i].name) ||
			!strcmp(XmNworkWindow, args[i].name))
		{
			_XmHTMLWarning(__WFUNC__(w_curr, "SetValues"), XMHTML_MSG_12,
				args[i].name);
			return(False);
		}
	}

	/* we have a new source request */
	if(valueReq)
	{
		/* we had a previous source */
		if(ATTR_HTML(w_curr, source))
		{
			/* new text has been supplied */
			if(ATTR_HTML(w_req, value))
			{
				/* see if it differs */
				if(strcmp(ATTR_HTML(w_req, value), ATTR_HTML(w_curr, source)))
				{
					parse = True;	/* it does */

					/* free previous source text */
					free(ATTR_HTML(w_curr, source));

					/* copy new source text */
					ATTR_HTML(w_new, source) = strdup(ATTR_HTML(w_req, value));

					/* Required to do both onLoad and onUnload events */
					event_to_check = EVENT_LOAD | EVENT_UNLOAD;
				}
				else
					parse = False;	/* it doesn't */
			}
			else	/* have to clear current text */
			{
				parse = True;

				/* free previous source text */
				free(ATTR_HTML(w_curr, source));

				/* reset to NULL */
				ATTR_HTML(w_new, source) = NULL;

				/* Required to check onUnLoad event */
				event_to_check = EVENT_UNLOAD;
			}
		}
		else	/* we didn't have any source */
		{
			if(ATTR_HTML(w_req, value))
			{
				/* new text */
				parse = True;

				/* copy new source text */
				ATTR_HTML(w_new, source) = strdup(ATTR_HTML(w_req, value));

				/* Required to check onLoad event */
				event_to_check = EVENT_LOAD;
			}
			else
				parse = False;	/* still empty */
		}
		/* input is always complete */
		ATTR_HTML(w_new, input_complete) = True;
	}

	/*****
	* Whoa!! String direction changed!!! All text will be reversed
	* and default alignment changes to the other margin as well.
	* Needs full reformat as this changes things profoundly...
	* This requires a full reparsing of the document data as string reversal
	* is done at the lowest possible level: in the parser.
	*****/
	if(ATTR_HTML(w_req,string_direction) != ATTR_HTML(w_curr,string_direction))
	{
		parse = True;

		/* check for alignment */
		CheckAlignment(w_new, w_req);
	}

	/*****
	* Enabling/disabling icon entity support requires a complete reparsing
	* of the input as the icons are translated at parse-time.
	*****/
	if(ATTR_HTML(w_req, icon_entities_enabled) !=
		ATTR_HTML(w_curr, icon_entities_enabled))
	{
		parse = True;
	}

	/*****
	* Check if vertical icon alignment has changed. Same note as with
	* enabling/disabling icon entities applies here as well.
	*****/
	if(ATTR_HTML(w_req, icon_valign) != ATTR_HTML(w_curr, icon_valign))
	{
		/* verify new value */
		if(!XmRepTypeValidValue(icon_repid, ATTR_HTML(w_req, icon_valign),
				(Widget)w_req))
			ATTR_HTML(w_new, icon_valign) = ATTR_HTML(w_curr, icon_valign);
		else
			/*****
			* Reparsing only required if support for icon entities is
			* enabled.
			*****/
			parse = ATTR_HTML(w_req, icon_entities_enabled);
	}

	if(parse)
	{
		_XmHTMLDebug(1, ("XmHTML.c: SetValues, parsing new text\n"));

		/* Check if we have to execute the onUnload event */
		if((event_to_check & EVENT_UNLOAD) &&
			ATTR_HTML(w_curr, event_mask) & EVENT_UNLOAD)
		{
			/*****
			* Event processing of the onUnload event can *never*
			* modify the document content. EventProcess explicitly
			* *exits* if it does happen.
			*****/
			_XmHTMLEventProcess(w_curr, NULL,
				ATTR_HTML(w_curr, body_events)->onUnload);
		} 
		/* clear the unload bit */
		event_to_check &= ~EVENT_UNLOAD;

		/* new text has been set, kill of any existing PLC's */
		_XmHTMLKillPLCCycler(w_curr);

		/* release event database */
		_XmHTMLEventFreeDatabase(w_curr, w_new);

		/* destroy any form data */
		_XmHTMLFreeForm(w_curr, ATTR_HTML(w_curr, form_data));
		ATTR_HTML(w_curr, form_data) = (XmHTMLFormData*)NULL;
		ATTR_HTML(w_new, form_data) = (XmHTMLFormData*)NULL;

		/*****
		* Destroy any external object data inside frames before new
		* ones are created.
		*****/
#ifdef HAVE_EXTERNAL_OBJECTS
		_XmHTMLFrameDestroyExternalObjects(w_curr);
		_XmHTMLDestroyExternalObjects(w_curr);
#endif

		/* Parse the raw HTML text */
		ATTR_HTML(w_new, elements) = _XmHTMLparseHTML(w_req,
				ATTR_HTML(w_curr, elements), ATTR_HTML(w_req, value), w_new);

		/* reset topline */
		ATTR_HTML(w_new, top_line) = 0;

		/* keep current frame setting and check if new frames are allowed */
		ATTR_HTML(w_new, is_frame) = ATTR_HTML(w_curr, is_frame);
		ATTR_HTML(w_new, nframes) = _XmHTMLCheckForFrames(w_new,
							ATTR_HTML(w_new, elements));

		/* Trigger link callback */
		if(ATTR_HTML(w_new, link_callback))
			_XmHTMLLinkCallback(w_new);

		/* needs layout, a redraw and current images must be freed */
		need_reformat = True;
		redraw      = True;
		free_images = True;

		_XmHTMLDebug(1, ("XmHTML.c: SetValues, done parsing\n"));
	}

	if((ATTR_HTML(w_req, enable_outlining)
		!= ATTR_HTML(w_curr, enable_outlining)) ||
		(ATTR_HTML(w_req, alignment) != ATTR_HTML(w_curr, alignment)))
	{
		/* Needs full reformat, default alignment is a text property */
		CheckAlignment(w_new, w_req);
		need_reformat = True;
	}

	/*****
	* Check if tabwidth has been changed. Always needs a recomputation
	* of the layout since we don't know if the current text contains
	* preformatted text or not.
	*****/
	if(ATTR_HTML(w_req, tabwidth) != ATTR_HTML(w_curr, tabwidth))
	{
		/*****
		* tabwidth must be a positive, non-zero number.
		* If not, issue a warning and keep the current tabwidth.
		*****/
		if(ATTR_HTML(w_req, tabwidth) < 1)
		{
			_XmHTMLWarning(__WFUNC__(w_req, "SetValues"), XMHTML_MSG_9,
				"XmNtabWidth", ATTR_HTML(w_req, tabwidth),
				ATTR_HTML(w_curr, tabwidth));
			ATTR_HTML(w_new, tabwidth) = ATTR_HTML(w_curr, tabwidth);
		}
		else
			need_reformat = True;
	}

	/*****
	* see if fonts have changed. The bloody problem with resources of type
	* String is that it's very well possible that a user is using some
	* static space to store these things. In these cases, the simple
	* comparisons are bound to be True every time, even though the content
	* might have changed (which we won't see cause it's all in static user
	* space!!), so to catch changes to this type of resources, we *MUST*
	* scan the array of provided args to check if it's specified. Sigh.
	*****/
	valueReq = False;
	for(i = 0; i < *num_args; i++)
	{
		if(!strcmp(XmNcharset, args[i].name) ||
			!strcmp(XmNfontFamily, args[i].name) ||
			!strcmp(XmNfontFamilyFixed, args[i].name) ||
			!strcmp(XmNfontSizeFixedList, args[i].name) ||
			!strcmp(XmNfontSizeList, args[i].name))
			valueReq = True;
	}
	if(valueReq ||
	ATTR_HTML(w_req, font_sizes)       != ATTR_HTML(w_curr, font_sizes)  ||
	ATTR_HTML(w_req, font_family)      != ATTR_HTML(w_curr, font_family) ||
	ATTR_HTML(w_req, font_sizes_fixed) != ATTR_HTML(w_curr, font_sizes_fixed) ||
	ATTR_HTML(w_req, font_family_fixed)!= ATTR_HTML(w_curr, font_family_fixed)||
	ATTR_HTML(w_req, charset)          != ATTR_HTML(w_curr, charset))
	{
		/* reset font cache */
		ATTR_HTML(w_new, default_font) = _XmHTMLSelectFontCache(w_new, True);
		need_reformat = True;
	}

	/*
	* Body colors. Original body colors are restored when body colors are
	* disabled.
	*/
	if(ATTR_HTML(w_req, body_colors_enabled)
		!= ATTR_HTML(w_curr, body_colors_enabled))
	{
		/* restore original body colors */
		if(!ATTR_HTML(w_req, body_colors_enabled))
		{
			ATTR_HTML(w_new, body_fg)   = ATTR_HTML(w_req, body_fg_save);
			ATTR_HTML(w_new, body_bg)   = ATTR_HTML(w_req, body_bg_save);
			ATTR_HTML(w_new, anchor_fg) = ATTR_HTML(w_req, anchor_fg_save);
			ATTR_HTML(w_new, anchor_visited_fg)   =
				ATTR_HTML(w_req, anchor_visited_fg_save);
			ATTR_HTML(w_new, anchor_activated_fg) =
				ATTR_HTML(w_req, anchor_activated_fg_save);

			/* recompute top, bottom & highlight colors */
			XmHTMLTkaRecomputeColors(w_new, ATTR_HTML(w_new, body_bg));
		}
		need_reformat = True;
	}

	/* 
	* Colors. For now we redo the layout since all colors are stored
	* in the ObjectTable data.
	* Not that effective, perhaps use multiple GC's, but thats a lot of
	* resource consuming going on then...
	*/
	if((ATTR_MGR(w_req,foreground)                  !=
			ATTR_MGR(w_curr, foreground))           ||
		(ATTR_CORE(w_req, background_pixel)         !=
			ATTR_CORE(w_curr, background_pixel))    ||
		(ATTR_HTML(w_req, anchor_fg)                !=
			ATTR_HTML(w_curr, anchor_fg))           ||
		(ATTR_HTML(w_req, anchor_target_fg)         !=
			ATTR_HTML(w_curr, anchor_target_fg))    ||
		(ATTR_HTML(w_req, anchor_visited_fg)        !=
			ATTR_HTML(w_curr, anchor_visited_fg))   ||
		(ATTR_HTML(w_req, anchor_activated_fg)      !=
			ATTR_HTML(w_curr, anchor_activated_fg)) ||
		(ATTR_HTML(w_req, anchor_activated_bg)      !=
			ATTR_HTML(w_curr, anchor_activated_bg)))
	{
		/* back and foreground pixels */
		ATTR_MGR(w_new, foreground)           =
			ATTR_MGR(w_req, foreground);
		ATTR_CORE(w_new, background_pixel)    =
			ATTR_CORE(w_req, background_pixel);
		ATTR_HTML(w_new, body_fg)             =
			ATTR_MGR(w_new, foreground);
		ATTR_HTML(w_new, body_bg)             =
			ATTR_CORE(w_new, background_pixel);
		ATTR_HTML(w_new, anchor_fg)           =
			ATTR_HTML(w_req, anchor_fg);
		ATTR_HTML(w_new, anchor_target_fg)    =
			ATTR_HTML(w_req, anchor_target_fg);
		ATTR_HTML(w_new, anchor_visited_fg)   =
			ATTR_HTML(w_req, anchor_visited_fg);
		ATTR_HTML(w_new, anchor_activated_fg) =
			ATTR_HTML(w_req, anchor_activated_fg);
		ATTR_HTML(w_new, anchor_activated_bg) =
			ATTR_HTML(w_req, anchor_activated_bg);

		/* save as new default colors */
		ATTR_HTML(w_new, body_fg_save)             =
			ATTR_HTML(w_new, body_fg);
		ATTR_HTML(w_new, body_bg_save)             =
			ATTR_HTML(w_new, body_bg);
		ATTR_HTML(w_new, anchor_fg_save)           =
			ATTR_HTML(w_new, anchor_fg);
		ATTR_HTML(w_new, anchor_target_fg_save)    =
			ATTR_HTML(w_new, anchor_target_fg);
		ATTR_HTML(w_new, anchor_visited_fg_save)   =
			ATTR_HTML(w_new, anchor_visited_fg);
		ATTR_HTML(w_new, anchor_activated_fg_save) =
			ATTR_HTML(w_new, anchor_activated_fg);
		ATTR_HTML(w_new, anchor_activated_bg_save) =
			ATTR_HTML(w_new, anchor_activated_bg);

		/* get new values for top, bottom & highlight colors */
		XmHTMLTkaRecomputeColors(w_new, ATTR_HTML(w_new, body_bg));
		need_reformat = True;
	}

	/*
	* anchor highlighting, must invalidate any current selection
	* No need to do a redraw if the highlightcolor changes: since the
	* SetValues method is chained, Manager's SetValues takes care of that.
	*/
	if(ATTR_HTML(w_req, highlight_on_enter) !=
		ATTR_HTML(w_curr, highlight_on_enter))
		ATTR_HTML(w_new, armed_anchor) = (XmHTMLObjectTable*)NULL;

	/* 
	* anchor underlining. Also needs a full layout computation as
	* underlining data is stored in the ObjectTable data
	*/
	if( (ATTR_HTML(w_req, anchor_underline_type)         != 
			ATTR_HTML(w_curr, anchor_underline_type))         ||
		(ATTR_HTML(w_req, anchor_visited_underline_type) != 
			ATTR_HTML(w_curr, anchor_visited_underline_type)) ||
		(ATTR_HTML(w_req, anchor_target_underline_type)  != 
			ATTR_HTML(w_curr, anchor_target_underline_type)))
	{
		CheckAnchorUnderlining(w_new, w_req);
		need_reformat = True;
	}
	else
	{
		/*
		* Support for color & font attributes. Needs a redo of the layout
		* if changed. We only need to check for this if the above test 
		* failed as that will also trigger a redo of the layout.
		*/
		if(ATTR_HTML(w_req, allow_color_switching) !=
				ATTR_HTML(w_curr, allow_color_switching) ||
			ATTR_HTML(w_req, allow_font_switching) !=
				ATTR_HTML(w_curr, allow_font_switching))
		need_reformat = True;
	}

	/*
	* on-the-fly enable/disable of dithering.
	*/
	if(ATTR_HTML(w_req, map_to_palette) != ATTR_HTML(w_curr, map_to_palette))
	{
		/* from on to off or off to on */
		if(ATTR_HTML(w_curr, map_to_palette) == XmDISABLED ||
			ATTR_HTML(w_req, map_to_palette) == XmDISABLED)
		{
			/* free current stuff */
			XCCFree(ATTR_HTML(w_curr, xcc));

			/* and create a new one */
			ATTR_HTML(w_new, xcc) = NULL;
			_XmHTMLCheckXCC(w_new);

			/* add palette if necessary */
			if(ATTR_HTML(w_req, map_to_palette) != XmDISABLED)
				_XmHTMLAddPalette(w_new);
		}
		else
		{
			/* fast & best methods require precomputed error matrices */
			if(ATTR_HTML(w_req, map_to_palette) == XmBEST ||
				ATTR_HTML(w_req, map_to_palette) == XmFAST)
			{
				XCCInitDither(ATTR_HTML(w_new, xcc));
			}
			else
				XCCFreeDither(ATTR_HTML(w_new, xcc));
		}
		/* and in *all* cases we need a full reformat */
		need_reformat = True;
	}

	/*
	* maximum amount of allowable image colors. Needs a full redo
	* of the layout if the current doc has got images with more colors
	* than allowed or it has images which have been dithered to fit
	* the previous setting.
	*/
	if((ATTR_HTML(w_req, max_image_colors) !=
		ATTR_HTML(w_curr, max_image_colors)))
	{
		_XmHTMLCheckMaxColorSetting(w_new);

		/*
		* check if we have any images with more colors than allowed or
		* we had images that were dithered. If so we need to redo the layout
		*/
		if(!need_reformat)
		{
			XmHTMLImage *image;
			int prev_max = ATTR_HTML(w_curr, max_image_colors);
			int new_max  = ATTR_HTML(w_req, max_image_colors);

			for(image = ATTR_HTML(w_new, images); image != NULL && !free_images;
				image = image->next)
			{
				/* ImageInfo is still available. Compare against it */
				if(!ImageInfoFreed(image))
				{
					/*
					* redo image composition if any of the following
					* conditions is True:
					* - current image has more colors than allowed;
					* - current image has less colors than allowed but the
					*	original image had more colors than allowed previously.
					*/
					if(image->html_image->ncolors > new_max ||
						(image->html_image->scolors < new_max &&
						image->html_image->scolors > prev_max))
						free_images = True;
				}
				/* info no longer available. Check against allocated colors */
				else
					if(image->npixels > new_max)
						free_images = True;
			}
			/* need to redo the layout if we are to redo the images */
			need_reformat = free_images;
		}
	}

	/* Are images enabled? */
	if(ATTR_HTML(w_req, images_enabled) != ATTR_HTML(w_curr, images_enabled))
	{
		/*****
		* we always need to free the images if this changes. A full
		* layout recomputation will load all images.
		*****/
		free_images = True;
		need_reformat = True;
	}

	/* PLC timing intervals */
	if(ATTR_HTML(w_req, plc_min_delay) != ATTR_HTML(w_curr, plc_min_delay)  ||
		ATTR_HTML(w_req, plc_max_delay) != ATTR_HTML(w_curr, plc_max_delay) ||
		ATTR_HTML(w_req, plc_delay) != ATTR_HTML(w_curr, plc_def_delay))
		_XmHTMLPLCCheckIntervals(w_new);

	/*****
	* Now format the list of parsed objects.
	* Don't do a bloody thing if we are already in layout as this will
	* cause unnecessary reloading and screen flickering.
	*****/
	if(need_reformat && !ATTR_HTML(w_curr, in_layout))
	{
		_XmHTMLDebug(1, ("XmHTML.c: SetValues, need layout\n"));

		/*****
		* It the current document makes heavy use of images we first need
		* to clear it. Not doing this would cause a shift in the colors of 
		* the current document (as they are being released) which does not 
		* look nice. Therefore first clear the entire display* area *before* 
		* freeing anything at all.
		*****/
		if(ATTR_HTML(w_new, gc) != NULL)
		{
			ATTR_HTML(w_curr, tka)->ClearArea(ATTR_HTML(w_new, tka)->dpy,
				ATTR_HTML(w_new, tka)->win, 0, 0, 
				ATTR_CORE(w_new, width), ATTR_CORE(w_new, height), False);
		}

		/* destroy any form data */
		_XmHTMLFreeForm(w_curr, ATTR_HTML(w_curr, form_data));
		ATTR_HTML(w_new, form_data) = (XmHTMLFormData*)NULL;

		/*****
		* Destroy any external object data inside frames before new
		* ones are created.
		*****/
#ifdef HAVE_EXTERNAL_OBJECTS
		_XmHTMLFrameDestroyExternalObjects(w_curr);
		_XmHTMLDestroyExternalObjects(w_curr);
#endif

		/* Free all non-persistent resources */
		_XmHTMLFreeExpendableResources(w_curr, free_images);

		/* reset some important vars */
		_XmHTMLReset(w_new, free_images);

		/* check for an xcc, it might have been destroyed by now */
		if(free_images)
			_XmHTMLCheckXCC(w_new);

		/* get new values for top, bottom & highlight */
		XmHTMLTkaRecomputeColors(w_new, ATTR_HTML(w_new, body_bg));

		/* go and format the parsed HTML data */
		if(!_XmHTMLCreateFrames(w_curr, w_new))
		{
			ATTR_HTML(w_new, frames) = NULL;
			ATTR_HTML(w_new, nframes) = 0;
			/* keep current frame setting */
			ATTR_HTML(w_new, is_frame) = ATTR_HTML(w_curr, is_frame);
		}

		_XmHTMLformatObjects(w_curr, w_new);

		/* Check for the onload event */
		if(ATTR_HTML(w_new, event_mask) & EVENT_LOAD)
		{
			/*****
			* Check event return value. If it returns True the document
			* has changed from under us!
			*****/
			if(_XmHTMLEventProcess(w_new, NULL,
				ATTR_HTML(w_new, body_events)->onLoad))
				return(False);
		}

		/* and check for possible external imagemaps */
		_XmHTMLCheckImagemaps(w_new);

		_XmHTMLDebug(1, ("XmHTML.c: SetValues, computing new layout.\n"));

		/* compute new screen layout */
		_XmHTMLLayout(w_new);

		_XmHTMLDebug(1, ("XmHTML.c: SetValues, done with layout.\n"));

		/* if new text has been set, fire up the PLCCycler */
		if(parse)
		{
			ATTR_HTML(w_new, plc_suspended) = False;
			_XmHTMLPLCCycler((XtPointer)w_new , NULL);
		}

		free_images = False;
		redraw = True;
		need_layout = False;
	}
	/*****
	* Default background image changed. We don't need to do this when a
	* layout recomputation was required as it will have been taken care
	* of already.
	*****/
	else if
		(ATTR_HTML(w_req, body_images_enabled) !=
			ATTR_HTML(w_curr, body_images_enabled) ||
		 ATTR_HTML(w_req, def_body_image_url)  !=
			ATTR_HTML(w_curr, def_body_image_url))
	{

		/* check if body images display status is changed */
		if(ATTR_HTML(w_req, body_images_enabled) !=
			ATTR_HTML(w_curr, body_images_enabled))
		{
			if(!free_images && ATTR_HTML(w_curr, body_image))
				ATTR_HTML(w_curr, body_image)->options |= IMG_ORPHANED;
			ATTR_HTML(w_new, body_image) = NULL;
		}

		/* a new body image has been specified, check it */
		if(ATTR_HTML(w_req, def_body_image_url) !=
			ATTR_HTML(w_curr, def_body_image_url))
		{
			/* do we have a new image? */
			if(ATTR_HTML(w_req, def_body_image_url))
			{
				/* yes we do */
				ATTR_HTML(w_new, def_body_image_url) =
					strdup(ATTR_HTML(w_req, def_body_image_url));
			}
			else /* no we don't */
				ATTR_HTML(w_new, def_body_image_url) = NULL;

			/* did we have a previous image? */
			if(ATTR_HTML(w_curr, def_body_image_url))
			{
				/* we did, free it */
				free(ATTR_HTML(w_curr, def_body_image_url));

				/* make it an orphan */
				if(!free_images && ATTR_HTML(w_curr, body_image))
					ATTR_HTML(w_curr, body_image)->options |= IMG_ORPHANED;
			}
		}

		/*
		* only load background image if image support is enabled and if
		* we are instructed to show a background image.
		*/
		if(ATTR_HTML(w_req, images_enabled) &&
			ATTR_HTML(w_req, body_images_enabled))
		{
			/* current document has a background image of it's own. */
			if(ATTR_HTML(w_new, body_image_url))
				_XmHTMLLoadBodyImage(w_new, ATTR_HTML(w_new, body_image_url));
			/*
			* Only load the default background image if the doc didn't have
			* it's colors changed.
			*/
			else if(ATTR_HTML(w_new, def_body_image_url)     &&
				ATTR_HTML(w_new, body_fg)                    ==
					ATTR_HTML(w_new, body_fg_save)           &&
				ATTR_HTML(w_new, body_bg)                    ==
					ATTR_HTML(w_new, body_bg_save)           &&
				ATTR_HTML(w_new, anchor_fg)                  ==
					ATTR_HTML(w_new, anchor_fg_save)         &&
				ATTR_HTML(w_new, anchor_visited_fg)          ==
					ATTR_HTML(w_new, anchor_visited_fg_save) &&
				ATTR_HTML(w_new, anchor_activated_fg)        ==
					ATTR_HTML(w_new, anchor_activated_fg_save))
				_XmHTMLLoadBodyImage(w_new,
					ATTR_HTML(w_new, def_body_image_url));
		}
		/*****
		* When a body image is present it is very likely that a highlight
		* color based upon the current background actually makes an anchor
		* invisible when highlighting is selected. Therefore we base the
		* highlight color on the activated anchor background when we have a 
		* body image, and on the document background when no body image is
		* present.
		*****/
		if(ATTR_HTML(w_new, body_image))
			XmHTMLTkaRecomputeHighlightColor(w_new,
				ATTR_HTML(w_new, anchor_activated_fg));
		else
			XmHTMLTkaRecomputeHighlightColor(w_new, ATTR_HTML(w_new, body_bg));

		/* only redraw if the new body image differs from the old one */
		if(ATTR_HTML(w_new, body_image) != ATTR_HTML(w_curr, body_image))
		{
			/* set alpha channel processing if not yet done */
			free_images = !parse && !need_reformat;
			redraw = True;
		}
	}

	/* anchor button state */
	if((ATTR_HTML(w_req, anchor_buttons) != ATTR_HTML(w_curr, anchor_buttons)))
		redraw = True;

	/*****
	* cursor state changes. Note that we always free the current cursor,
	* even if it's created by the user.
	*****/
	if((ATTR_HTML(w_req, anchor_cursor) != ATTR_HTML(w_curr, anchor_cursor)) ||
		(ATTR_HTML(w_req, anchor_display_cursor) !=
			ATTR_HTML(w_curr, anchor_display_cursor)))
	{
		/* set cursor to None if we don't have to use or have a cursor */
		if(!ATTR_HTML(w_new, anchor_display_cursor) ||
			!ATTR_HTML(w_new, anchor_cursor))
		{
			if(ATTR_HTML(w_curr, anchor_cursor) != None)
				XFreeCursor(XtDisplay((Widget)w_curr),
					ATTR_HTML(w_curr, anchor_cursor));
			ATTR_HTML(w_new, anchor_cursor) = None;
		}
		/* no redraw required */
	}

	/*
	* Scroll to the requested line or restore previous line if it has been
	* messed up as the result of a resource change requiring a recompuation
	* of the layout. 
	*/
	if(ATTR_HTML(w_req, top_line) != ATTR_HTML(w_curr, top_line))
	{
		_XmHTMLScrollToLine(w_new, ATTR_HTML(w_req, top_line));
		redraw = True;
	}
	else if(need_reformat && !parse &&
			ATTR_HTML(w_new, top_line) != ATTR_HTML(w_curr, top_line))
	{
		_XmHTMLScrollToLine(w_new, ATTR_HTML(w_curr, top_line));
		redraw = True;
	}

	/* check and set scrolling delay */
	if(ATTR_HTML(w_req, repeat_delay) != ATTR_HTML(w_curr, repeat_delay))
	{
		if(ATTR_HTML(w_new, vsb) && XtIsManaged(ATTR_HTML(w_new, vsb)))
			XtVaSetValues(ATTR_HTML(w_new, vsb), 
				XmNrepeatDelay, ATTR_HTML(w_new, repeat_delay), NULL);
		if(ATTR_HTML(w_new, hsb) && XtIsManaged(ATTR_HTML(w_new, hsb)))
			XtVaSetValues(ATTR_HTML(w_new, hsb), 
				XmNrepeatDelay, ATTR_HTML(w_new, repeat_delay), NULL);
	}
	/* see if we have to restart the animations if they were frozen */
	if(!ATTR_HTML(w_req, freeze_animations) &&
		ATTR_HTML(w_curr, freeze_animations))
		_XmHTMLRestartAnimations(w_new);

	/* do we still need pointer tracking? */
	if(!ATTR_HTML(w_new, anchor_track_callback)  &&
		!ATTR_HTML(w_new, anchor_cursor)         &&
		!ATTR_HTML(w_new, highlight_on_enter)    &&
		!ATTR_HTML(w_new, motion_track_callback) &&
		!ATTR_HTML(w_new, focus_callback)        &&
		!ATTR_HTML(w_new, losing_focus_callback))
		ATTR_HTML(w_new, need_tracking) = False;
	else
		ATTR_HTML(w_new, need_tracking) = True;

	/* only recompute new layout if we haven't done so already */
	if(need_layout && !ATTR_HTML(w_curr, in_layout) && !need_reformat)
	{
		_XmHTMLLayout(w_new);
		redraw = True;
	}

	if(redraw)
	{
		/*
		* If free_images is still set when we get here, check if some
		* images need their delayed_creation bit set.
		*/
		if(free_images)
		{
			XmHTMLImage *img;
			for(img = ATTR_HTML(w_new, images); img != NULL; img = img->next)
			{
				if(!ImageInfoFreed(img) &&
					ImageInfoDelayedCreation(img->html_image))
				{
					img->options |= IMG_DELAYED_CREATION;
					ATTR_HTML(w_new, delayed_creation) = True;
				}
			}
			if(ATTR_HTML(w_new, delayed_creation))
				_XmHTMLImageCheckDelayedCreation(w_new);
		}

		_XmHTMLDebug(1, ("XmHTML.c: SetValues, calling _XmHTMLClearArea.\n"));
		/*****
		* To make sure the new text is displayed, we need to clear
		* the current contents and generate an expose event to render
		* the new text.
		* We can only do this when we have been realized. If we don't have
		* a gc, it means we haven't been realized yet. (fix 01/26/97-01, kdh)
		*****/
		if(ATTR_HTML(w_new, gc) != NULL)
			_XmHTMLClearArea(w_new, 0, 0, ATTR_CORE(w_new, width),
				ATTR_CORE(w_new, height));
	}

	/* Check for the onload event */
	if((event_to_check & EVENT_LOAD) &&
		ATTR_HTML(w_new, event_mask) & EVENT_LOAD)
	{
		/*****
		* Don't bother to check return value. It's forbidden to modify
		* the document content during processing of the onLoad event.
		* (EventProcess exits if it does happen)
		*****/
		_XmHTMLEventProcess(w_new, NULL,
			ATTR_HTML(w_new, body_events)->onLoad);
	}

	_XmHTMLDebug(1, ("XmHTML.c: SetValues End\n"));

	return(redraw);
}

/*****
* Name: 		GetValues
* Return Type: 	void
* Description: 	XmHTMLWidgetClass get_values_hook method.
* In: 
*
* Returns:
*	nothing
*****/
static void 
GetValues(Widget w, ArgList args, Cardinal *num_args)
{
	register int i;

	_XmHTMLDebug(1, ("XmHTML.c: GetValues Start\n"));

	/*****
	* Note on args filling:
	* the value field (which is of type long int) represents a pointer to
	* another pointer. Thus, updating a value requires updating the
	* location the second pointer points to. That's why a double pointer
	* cast is required.
	*****/

	for(i = 0; i < *num_args; i++)
	{
		_XmHTMLDebug(1, ("XmHTML.c: GetValues, requested for %s.\n",
			args[i].name));

		/*
		* We return a pointer to the source text instead of letting X do it
		* since the user might have freed the original text by now.
		*/
		if(!(strcmp(args[i].name, XmNvalue)))
		{
			*((char**)args[i].value) = XmHTMLTextGetSource(w);
		}
		else if(!(strcmp(args[i].name, XmNtopLine)))
		{
			XmHTMLObjectTableElement tmp;
			XmHTMLWidget html = (XmHTMLWidget)w;

			/* get current linenumber */
			tmp = _XmHTMLGetLineObject(html, HTML_ATTR(scroll_y));

			if(tmp != NULL)
				*((int*)args[i].value) = tmp->line;
			else
				*((int*)args[i].value) = 0;
		}
	}
	_XmHTMLDebug(1, ("XmHTML.c: GetValues End\n"));
	return;
}

/*****
* Name: 		GeometryManager
* Return Type: 	XtGeometryResult
* Description:	XmHTMLWidgetClass geometry_manager method
* In: 
*
* Returns:
*	Don't care. Just pass everything on.
*****/
static XtGeometryResult 
GeometryManager(Widget w, XtWidgetGeometry *request,
	XtWidgetGeometry *geometry_return)
{
	_XmHTMLDebug(1, ("XmHTML.c: GeometryManager Start\n"));

	if(request->request_mode & CWX)
		geometry_return->x = request->x;
	if(request->request_mode & CWY)
		geometry_return->y = request->y;
	if(request->request_mode & CWWidth)
		geometry_return->width = request->width;
	if(request->request_mode & CWHeight)
		geometry_return->height = request->height;
	if(request->request_mode & CWBorderWidth)
		geometry_return->border_width = request->border_width;
	geometry_return->request_mode = request->request_mode;

	_XmHTMLDebug(1, ("XmHTML.c: GeometryManager End\n"));

	return(XtGeometryYes);
}

/*****
* Name: 		Destroy
* Return Type: 	void
* Description: 	XmHTMLWidgetClass destroy method. Frees up allocated resources.
* In: 
*	w:			widget to destroy
* Returns:
*	nothing
*****/
static void 
Destroy(Widget w)
{
	XmHTMLWidget html = (XmHTMLWidget)w;

	_XmHTMLDebug(1, ("XmHTML.c: Destroy Start\n"));

	/* destroy common resources */
	_XmHTMLDestroyPhaseZero(html);

	/* remove all callbacks */
	XtRemoveAllCallbacks(w, XmNactivateCallback);
	XtRemoveAllCallbacks(w, XmNarmCallback);
	XtRemoveAllCallbacks(w, XmNanchorTrackCallback);
	XtRemoveAllCallbacks(w, XmNframeCallback);
	XtRemoveAllCallbacks(w, XmNformCallback);
	XtRemoveAllCallbacks(w, XmNinputCallback);
	XtRemoveAllCallbacks(w, XmNlinkCallback);
	XtRemoveAllCallbacks(w, XmNmotionTrackCallback);
	XtRemoveAllCallbacks(w, XmNimagemapCallback);
	XtRemoveAllCallbacks(w, XmNdocumentCallback);
	XtRemoveAllCallbacks(w, XmNfocusCallback);
	XtRemoveAllCallbacks(w, XmNlosingFocusCallback);
	XtRemoveAllCallbacks(w, XmNeventCallback);

	/* Destroy our ToolkitAbstraction */
	XmHTMLTkaDestroy(HTML_ATTR(tka));

	/* invalidate this widget */
	w = NULL;

	_XmHTMLDebug(1, ("XmHTML.c: Destroy End\n"));
	return;
}

/*****
* Name: 		HTMLProcessInput
* Return Type: 	void
* Description: 	handles keyboard input for the HTML widget.
* In: 
*	w:			XmHTMLWidget
*	event:		ButtonEvent structure
*	params:		additional args, unused
*	num_params:	no of addition args, unused
* Returns:
*	nothing
* Note:
*	This routine calls any installed XmNinputCallback callback resources.
*****/
static void 
HTMLProcessInput(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	XmHTMLWidget html;
	/*
	* If this action proc is called directly from within application code,
	* w is a html widget. In all other cases this action proc is called 
	* for the translations installed on the work_area, and thus we need to
	* use XtParent to get our html widget.
	*/
	if(XmIsHTML(w))
		html = (XmHTMLWidget)w;
	else
		html = (XmHTMLWidget)XtParent(w);

	/* pass down if callback is installed */
	if(HTML_ATTR(input_callback))
		_XmHTMLInputCallback(html, event);

	_XmHTMLDebug(1, ("XmHTML.c: ProcessInput End\n"));
}

/*****
* Name: 		HTMLPageUpOrLeft
* Return Type: 	void
* Description: 	keyboard navigation action routine
* In: 
*	w:			widget id; XmHTMLWidget id if called from within application
*				code, work_area if handled by XmHTML itself;
*	event:		key event;
*	params:		0 for pageUp, 1 for pageLeft
*	num_params:	always 1
* Returns:
*	nothing
* Note:
*	This routine also honors the repeatDelay resource.
*****/
static void
HTMLPageUpOrLeft(Widget w, XEvent *event, String *params, 
		Cardinal *num_params)
{
	int which;
	XmHTMLWidget html;
	static Time prev_time = 0;

	if(XmIsHTML(w))
		html = (XmHTMLWidget)w;
	else
		html = (XmHTMLWidget)XtParent(w);

	if(*num_params != 1 || !XtIsRealized(w))
	{
		if(*num_params != 1)
			_XmHTMLWarning(__WFUNC__(w, "HTMLPageUpOrLeft"), XMHTML_MSG_13,
				"page-up-or-left");
		return;
	}

	/* check repeat delay */
	if(event->xkey.time - prev_time < HTML_ATTR(repeat_delay))
		return;
	prev_time = event->xkey.time;

	which = atoi(params[0]);

	_XmHTMLDebug(1, ("XmHTML.c: HTMLPageUpOrLeft, which = %i\n", which));

	if(which == 0 && XtIsManaged(HTML_ATTR(vsb)))
		XtCallActionProc(HTML_ATTR(vsb), "PageUpOrLeft", event, params, 1);
	else if(which == 1 && XtIsManaged(HTML_ATTR(hsb)))
		XtCallActionProc(HTML_ATTR(hsb), "PageUpOrLeft", event, params, 1);
}

/*****
* Name: 		HTMLDownOrRight
* Return Type: 	void
* Description: 	keyboard navigation action routine
* In: 
*	w:			widget id; XmHTMLWidget id if called from within application
*				code, work_area if handled by XmHTML itself;
*	event:		key event;
*	params:		0 for pageDown, 1 for pageRight
*	num_params:	always 1
* Returns:
*	nothing
* Note:
*	This routine also honors the repeatDelay resource.
*****/
static void
HTMLPageDownOrRight(Widget w, XEvent *event, String *params, 
		Cardinal *num_params)
{
	int which;
	XmHTMLWidget html;
	static Time prev_time = 0;

	if(XmIsHTML(w))
		html = (XmHTMLWidget)w;
	else
		html = (XmHTMLWidget)XtParent(w);

	if(*num_params != 1 || !XtIsRealized(w))
	{
		if(*num_params != 1)
			_XmHTMLWarning(__WFUNC__(w, "HTMLPageDownOrRight"), XMHTML_MSG_13,
				"page-down-or-right");
		return;
	}

	/* check repeat delay */
	if(event->xkey.time - prev_time < HTML_ATTR(repeat_delay))
		return;
	prev_time = event->xkey.time;

	which = atoi(params[0]);

	_XmHTMLDebug(1, ("XmHTML.c: HTMLPageDownOrRight, which = %i\n", which));

	if(which == 0 && XtIsManaged(HTML_ATTR(vsb)))
		XtCallActionProc(HTML_ATTR(vsb), "PageDownOrRight", event, params, 1);
	else if(which == 1 && XtIsManaged(HTML_ATTR(hsb)))
		XtCallActionProc(HTML_ATTR(hsb), "PageDownOrRight", event, params, 1);
}

/*****
* Name: 		HTMLIncrementUpOrLeft
* Return Type: 	void
* Description: 	keyboard navigation action routine
* In: 
*	w:			widget id; XmHTMLWidget id if called from within application
*				code, work_area if handled by XmHTML itself;
*	event:		key event;
*	params:		0 for IncrementUp, 1 for IncrementLeft
*	num_params:	always 1
* Returns:
*	nothing
* Note:
*	This routine also honors the repeatDelay resource.
*****/
static void
HTMLIncrementUpOrLeft(Widget w, XEvent *event, String *params, 
		Cardinal *num_params)
{
	int which;
	XmHTMLWidget html;
	static Time prev_time = 0;

	if(XmIsHTML(w))
		html = (XmHTMLWidget)w;
	else
		html = (XmHTMLWidget)XtParent(w);

	if(*num_params != 1 || !XtIsRealized(w))
	{
		if(*num_params != 1)
			_XmHTMLWarning(__WFUNC__(w, "HTMLIncrementUpOrLeft"), XMHTML_MSG_13,
				"increment-up-or-left");
		return;
	}

	/* check repeat delay */
	if(event->xkey.time - prev_time < HTML_ATTR(repeat_delay))
		return;
	prev_time = event->xkey.time;

	which = atoi(params[0]);

	_XmHTMLDebug(1, ("XmHTML.c: HTMLIncrementUpOrLeft, which = %i\n", which));

	if(which == 0 && XtIsManaged(HTML_ATTR(vsb)))
		XtCallActionProc(HTML_ATTR(vsb), "IncrementUpOrLeft", event,
			params, 1);
	else if(which == 1 && XtIsManaged(HTML_ATTR(hsb)))
		XtCallActionProc(HTML_ATTR(hsb), "IncrementUpOrLeft", event,
			params, 1);
}

/*****
* Name: 		HTMLIncrementDownOrRight
* Return Type: 	void
* Description: 	keyboard navigation action routine
* In: 
*	w:			widget id; XmHTMLWidget id if called from within application
*				code, work_area if handled by XmHTML itself;
*	event:		key event;
*	params:		0 for IncrementDown, 1 for IncrementRight
*	num_params:	always 1
* Returns:
*	nothing
* Note:
*	This routine also honors the repeatDelay resource.
*****/
static void
HTMLIncrementDownOrRight(Widget w, XEvent *event, String *params, 
		Cardinal *num_params)
{
	int which;
	XmHTMLWidget html;
	static Time prev_time = 0;

	if(XmIsHTML(w))
		html = (XmHTMLWidget)w;
	else
		html = (XmHTMLWidget)XtParent(w);

	if(*num_params != 1 || !XtIsRealized(w))
	{
		if(*num_params != 1)
			_XmHTMLWarning(__WFUNC__(w, "HTMLIncrementDownOrRight"),
				XMHTML_MSG_13, "increment-down-or-right");
		return;
	}

	/* check repeat delay */
	if(event->xkey.time - prev_time < HTML_ATTR(repeat_delay))
		return;
	prev_time = event->xkey.time;

	which = atoi(params[0]);

	_XmHTMLDebug(1, ("XmHTML.c: HTMLIncrementDownOrRight, which = %i\n",
		which));

	if(which == 0 && XtIsManaged(HTML_ATTR(vsb)))
		XtCallActionProc(HTML_ATTR(vsb), "IncrementDownOrRight", event, 
			params, 1);
	else if(which == 1 && XtIsManaged(HTML_ATTR(hsb)))
		XtCallActionProc(HTML_ATTR(hsb), "IncrementDownOrRight", event, 
			params, 1);
}

/*****
* Name: 		HTMLTopOrBottom
* Return Type: 	void
* Description: 	keyboard navigation action routine
* In: 
*	w:			widget id; XmHTMLWidget id if called from within application
*				code, work_area if handled by XmHTML itself;
*	event:		key event;
*	params:		0 for top, 1 for bottom
*	num_params:	always 1
* Returns:
*	nothing
* Note:
*	no repeatDelay by this action routine, it only moves from top to bottom
*	or vice-versa
*****/
static void
HTMLTopOrBottom(Widget w, XEvent *event, String *params, 
		Cardinal *num_params)
{
	int which;
	XmHTMLWidget html;

	if(XmIsHTML(w))
		html = (XmHTMLWidget)w;
	else
		html = (XmHTMLWidget)XtParent(w);

	if(*num_params != 1 || !XtIsRealized(w))
	{
		if(*num_params != 1)
			_XmHTMLWarning(__WFUNC__(w, "HTMLTopOrBottom"), XMHTML_MSG_13,
				"top-or-bottom");
		return;
	}
	which = atoi(params[0]);

	_XmHTMLDebug(1, ("XmHTML.c: HTMLTopOrBottom, which = %i\n", which));

	if(which == 0 && XtIsManaged(HTML_ATTR(vsb)))
	{
		/* no move if already on top */
		if(HTML_ATTR(top_line) == 0)
			return;

		HTML_ATTR(top_line) = 0;
		_XmHTMLMoveToPos(HTML_ATTR(vsb), html, 0);
	}
	else if(which == 1 && XtIsManaged(HTML_ATTR(vsb)))
	{
		int value;

		/* no move if already on bottom */
		if(HTML_ATTR(top_line) == HTML_ATTR(nlines))
			return;

		HTML_ATTR(top_line) = HTML_ATTR(nlines);
		value = HTML_ATTR(formatted_height);

		/* fix 01/30/97-04, kdh */
		_XmHTMLAdjustVerticalScrollValue(HTML_ATTR(vsb), &value);

		_XmHTMLMoveToPos(HTML_ATTR(vsb), html, value);
	}
}

static void
HTMLTraverseCurrent(Widget w, XEvent *event, String *params,
	Cardinal *num_params)
{
	if(!XtIsRealized(w))
		return;
	_XmHTMLProcessTraversal(w, XmTRAVERSE_CURRENT);
}

static void
HTMLTraverseNext(Widget w, XEvent *event, String *params,
	Cardinal *num_params)
{
	if(!XtIsRealized(w))
		return;
	_XmHTMLProcessTraversal(w, XmTRAVERSE_NEXT);
}

static void
HTMLTraversePrev(Widget w, XEvent *event, String *params,
	Cardinal *num_params)
{
	if(!XtIsRealized(w))
		return;

	_XmHTMLProcessTraversal(w, XmTRAVERSE_PREV);
}

static void
HTMLTraverseNextOrPrev(Widget w, XEvent *event, String *params,
	Cardinal *num_params)
{
	int which;

	if(*num_params != 1 || !XtIsRealized(w))
	{
		if(*num_params != 1)
			_XmHTMLWarning(__WFUNC__(w, "HTMLTraverseNextOrPrev"),
				XMHTML_MSG_13, "traverse-next-or-prev");
		return;
	}
	which = atoi(params[0]);
	if(which == 0)
		_XmHTMLProcessTraversal(w, XmTRAVERSE_NEXT_TAB_GROUP);
	else
		_XmHTMLProcessTraversal(w, XmTRAVERSE_PREV_TAB_GROUP);
}

/*****
* Name: 		ExtendStart
* Return Type: 	void
* Description: 	buttonPress action routine. Initializes a selection when
*				not over an anchor, else paints the anchor as being selected.
* In: 
*
* Returns:
*	nothing.
*****/
static void	
ExtendStart(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	/* need to use XtParent since we only get button events from work_area */
	XmHTMLWidget html = (XmHTMLWidget)XtParent(w);
	XButtonPressedEvent *pressed = (XButtonPressedEvent*)event;
	XmHTMLAnchor *anchor = NULL;
	XmHTMLWord *anchor_word = NULL;
	XmHTMLImage anchor_img;
	int x,y;

	/* no needless lingering in this routine */
	if(XtClass(XtParent(w)) != xmHTMLWidgetClass)
		return;

	/* we don't do a thing with events generated by button3 */
	if(pressed->button == Button3 && HTML_ATTR(arm_callback) == NULL)
		return;

	_XmHTMLFullDebug(1, ("XmHTML.c: ExtendStart Start\n"));

	/* Get coordinates of button event and add core offsets */
	x = pressed->x;
	y = pressed->y;

	/* try to get current anchor element */
	if(pressed->button != Button3 &&
		(((anchor_word = _XmHTMLGetAnchor(html, x, y, &anchor_img)) != NULL) ||
		((anchor = _XmHTMLGetImageAnchor(html, x, y, &anchor_img)) != NULL)))
	{
		/*****
		* User has selected an anchor. Get the text for this anchor.
		* Note: if anchor is NULL it means the user was over a real anchor
		* (regular anchor, anchored image or a form image button) and
		* anchor_word is non-NULL (this object the referenced URL). If it
		* is non-NULL the mouse was over an imagemap, in which case we
		* may not show visual feedback to the user.
		* I admit, the naming of the variables is rather confusing.
		******/
		if(anchor == NULL)
		{
			/* store anchor & paint as selected */
			anchor = anchor_word->owner->anchor;
			/*****
			* uncheck currently selected anchor if it's not the same as
			* the current anchor (mouse dragging)
			*****/
			if(HTML_ATTR(current_anchor) != NULL &&
					HTML_ATTR(current_anchor) != anchor_word->owner)
				_XmHTMLPaintAnchorUnSelected(html);
			_XmHTMLPaintAnchorSelected(html, anchor_word);
		}
		else if(HTML_ATTR(selected) != NULL &&
			HTML_ATTR(selected) != anchor)
			_XmHTMLPaintAnchorUnSelected(html);

		/* check for the onMouseDown event */
		if(anchor->event_mask & EVENT_MOUSEDOWN)
		{
			/*****
			* Check return value, document might have changed from under
			* us upon return.
			*****/
			if(_XmHTMLEventProcess(html, event, anchor->events->onMouseDown))
				return;
		}

		HTML_ATTR(selected) = anchor;

		_XmHTMLFullDebug(1, ("XmHTML.c: ExtendStart, anchor selected is %s\n",
			anchor->href));
	}
	else if(HTML_ATTR(current_anchor) != NULL)
	{
		ToolkitAbstraction *tka = HTML_ATTR(tka);
		/* not over an anchor, unselect current anchor and reset cursor */
		_XmHTMLPaintAnchorUnSelected(html);
		tka->UndefineCursor(tka->dpy, tka->win);
	}

	/* remember pointer position and time */
	HTML_ATTR(press_x) = pressed->x;
	HTML_ATTR(press_y) = pressed->y;
	HTML_ATTR(pressed_time) = pressed->time;

	if(anchor_word == NULL && anchor == NULL && HTML_ATTR(arm_callback))
		_XmHTMLArmCallback(html, event);

	_XmHTMLFullDebug(1, ("XmHTML.c: ExtendStart End\n"));
}

/*****
* Name: 		ExtendAdjust
* Return Type: 	void
* Description: 	buttondrag action routine. Adjusts the selection initiated
*				by ExtendStart.
* In: 
*
* Returns:
*	nothing.
*****/
static void	
ExtendAdjust(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	XmHTMLWidget html;

	/* need to use XtParent since we only get motion events from work_area */
	if(XtClass(XtParent(w)) != xmHTMLWidgetClass)
		return;

	html = (XmHTMLWidget)XtParent(w);

	_XmHTMLFullDebug(1, ("XmHTML.c: ExtendAdjust Start\n"));

	_XmHTMLFullDebug(1, ("XmHTML.c: ExtendAdjust End\n"));

	return;
}

/*****
* Name: 		ExtendEnd
* Return Type: 	void
* Description: 	buttonrelease tracking action routine. Terminates the selection
*				initiated by ExtendStart. When over an anchor, paints the 
*				anchor as being deselected. XmNactivateCallback  or
*				XmNarmCallback callback resources are only called if the
*				buttonpress and release occur within a certain time limit 
*				(XmHTML_MAX_BUTTON_RELEASE_TIME, defined XmHTMLfuncs.h)
* In: 
*
* Returns:
*	nothing
*****/
static void	
ExtendEnd(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	/* need to use XtParent since we only get button events from work_area */
	XmHTMLWidget html = (XmHTMLWidget)XtParent(w);
	XButtonReleasedEvent *release = (XButtonReleasedEvent*)event;
	XmHTMLAnchor *anchor = NULL;
	XmHTMLWord *anchor_word = NULL;
	XmHTMLImage anchor_img;
	int x,y;

	/* no needless lingering in this routine */
	if(XtClass(XtParent(w)) != xmHTMLWidgetClass)
		return;

	/* we don't do a thing with events generated by button3 */
	if(release->button == Button3)
		return;

	_XmHTMLFullDebug(1, ("XmHTML.c: ExtendEnd Start\n"));

	/* Get coordinates of button event */
	x = release->x;
	y = release->y;

	/* try to get current anchor element */
	if(((anchor_word = _XmHTMLGetAnchor(html, x, y, &anchor_img)) != NULL) ||
		((anchor = _XmHTMLGetImageAnchor(html, x, y, &anchor_img)) != NULL))
	{
		/* 
		* OK, release took place over an anchor, see if it falls within the 
		* allowable time limit and we are still over the anchor selected by
		* ExtendStart.
		*/
		if(anchor == NULL)
			anchor = anchor_word->owner->anchor;

		/*
		* If we already have an active anchor and it's different from the
		* current anchor, deselect it.
		*/
		if(HTML_ATTR(current_anchor) &&
			HTML_ATTR(current_anchor) != anchor_word->owner)
			_XmHTMLPaintAnchorUnSelected(html);

		/* see if we need to serve the mouseUp event */
		if(anchor->event_mask & EVENT_MOUSEUP)
		{
			/*****
			* Process this event. If we get a non-zero return value it
			* means that the document has been changed from under us
			* so we won't perform any other processing as we most
			* likely will be referencing invalid data.
			*****/
			if(_XmHTMLEventProcess(html, event, anchor->events->onMouseUp))
				return;
		}

		/* this anchor is still in selection */
		if(anchor_word)
			if(HTML_ATTR(highlight_on_enter))
				_XmHTMLPaintAnchorEntry(html, anchor_word->owner);
			else
				_XmHTMLPaintAnchorUnSelected(html);

		_XmHTMLFullDebug(1, ("XmHTML.c: ExtendEnd, anchor selected is %s\n",
			anchor->href));
		/* 
		* if we had a selected anchor and it's equal to the current anchor
		* and the button was released in time, trigger the activation callback.
		*/
		if(HTML_ATTR(selected) != NULL && anchor == HTML_ATTR(selected) &&
		(release->time - HTML_ATTR(pressed_time)) < XmHTML_BUTTON_RELEASE_TIME)
		{
			/* check for the onClick event */
			if(anchor->event_mask & EVENT_CLICK)
			{
				_XmHTMLDebug(1, ("XmHTML.c:ExtendEnd, onClick: anchor=%x ->\n"
					"events=%x -> onClick = %x -> (type=%d, data=%x)\n",
					anchor, anchor->events, anchor->events->onClick,
					anchor->events->onClick->type,
					anchor->events->onClick->data));

				/*****
				* Process this event. If we get a non-zero return value it
				* means that the document has been changed from under us
				* so we won't perform any other processing as we most
				* likely will be referencing invalid data.
				*****/
				if(_XmHTMLEventProcess(html, event, anchor->events->onClick))
					return;
			}

			/*****
			* Trigger form callback
			* No need to check return value, we return after this call.
			*****/
			if(anchor->url_type == ANCHOR_FORM_IMAGE)
				_XmHTMLFormActivate(html, event, anchor_word->form);
			else if(HTML_ATTR(activate_callback))
			{
				/*****
				* Trigger activation callback
				* No need to check return value, we return after this call.
				*****/
				_XmHTMLActivateCallback(html, event, anchor);

				_XmHTMLFullDebug(1, ("XmHTML.c: ExtendEnd End\n"));
			}
			return;
		}
	}

	/* unset any previously selected anchor */
	if(HTML_ATTR(current_anchor) != NULL)
	{
		/* keep current anchor selection or unset it */
		if(anchor_word)
			_XmHTMLPaintAnchorEntry(html, anchor_word->owner);
		else
			_XmHTMLPaintAnchorUnSelected(html);
	}
	_XmHTMLFullDebug(1, ("XmHTML.c: ExtendEnd End\n"));

	return;
}

/*****
* Name: 		TrackMotion
* Return Type: 	void
* Description: 	mouse tracker; calls XmNanchorTrackCallback and/or HTML4.0
*				event processing if entering/leaving an anchor.
*				Also calls XmNmotionTrackCallback when installed.
* In: 
*	w:			XmHTMLWidget
*	event:		MotionEvent structure
*	params:		additional args, unused
*	num_parmas:	no of additional args, unused
* Returns:
*	nothing
*****/
static void 
TrackMotion(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	/* need to use XtParent since we only get motion events from work_area */
	XmHTMLWidget html = (XmHTMLWidget)XtParent(w);
	XMotionEvent *motion = (XMotionEvent*)event;
	XmHTMLAnchor *anchor = NULL;
	XmHTMLWord *anchor_word = NULL;
	XmHTMLImage anchor_img;
	int x = 0, y = 0;
	ToolkitAbstraction *tka = NULL;

	/* no needless lingering in this routine */
	if(XtClass(XtParent(w)) != xmHTMLWidgetClass)
		return;

	/* ignore if we don't have to make any more feedback to the user */
	if(!HTML_ATTR(need_tracking) || event->xany.type != MotionNotify)
		return;

	/* get handle to the ToolkitAbstraction */
	tka = HTML_ATTR(tka);

	/* we are already on the correct anchor, just return */
	_XmHTMLFullDebug(1, ("XmHTML.c: TrackMotion Start.\n"));

	/* save x and y position, we need it to get anchor data */
	/* pass down to motion tracker callback if installed */
	if(HTML_ATTR(motion_track_callback))
		_XmHTMLMotionCallback(html, event);

	x = motion->x;
	y = motion->y;

	/* try to get current anchor element (if any) */
	if(((anchor_word = _XmHTMLGetAnchor(html, x, y, &anchor_img)) == NULL) &&
		((anchor = _XmHTMLGetImageAnchor(html, x, y, &anchor_img)) == NULL))
	{
		_XmHTMLFullDebug(1, ("XmHTML.c: TrackMotion, no current anchor.\n"));

		/* invalidate current selection or check event mask if there is one */
		if(HTML_ATTR(anchor_current_cursor_element))
		{
			XmHTMLAnchor *pa = HTML_ATTR(anchor_current_cursor_element);
			/*****
			* First check for onMouseOut event. Return if document has
			* been modified during event processing.
			*****/
			if(pa->event_mask & EVENT_MOUSEOUT)
			{
				if(_XmHTMLEventProcess(html, event, pa->events->onMouseOut))
					return;
			}
			if(HTML_ATTR(anchor_track_callback))
				_XmHTMLTrackCallback(html, event, NULL);
		}

		if(HTML_ATTR(highlight_on_enter) && HTML_ATTR(armed_anchor))
			_XmHTMLPaintAnchorLeave(html);

		HTML_ATTR(armed_anchor) = NULL;
		HTML_ATTR(anchor_current_cursor_element) = NULL;
		tka->UndefineCursor(tka->dpy, tka->win);
		return;
	}

	if(anchor == NULL)
		anchor = anchor_word->owner->anchor;

	/* Trigger callback and set cursor if we are entering a new element */
	if(anchor != HTML_ATTR(anchor_current_cursor_element)) 
	{
		_XmHTMLFullDebug(1, ("XmHTML.c: TrackMotion, new anchor.\n"));

		/*****
		* When anchors are painted next to each other or the user is
		* moving the mouse very fast from one anchor to another, the previous
		* check (anchor_word and anchor == NULL) will fail, thereby leaving
		* a few loose ends (possible HTML4.0 events and anchor tracking).
		* So if we had a current anchor, we need to check if it a mouseOut
		* event was present and if so, we need to process this event.
		* After that we call the TrackCallback so the application will be
		* notified of the fact that the current anchor is no longer valid
		* and that the user is moving towards a new anchor.
		*****/
		if(HTML_ATTR(anchor_current_cursor_element))
		{
			XmHTMLAnchor *pa = HTML_ATTR(anchor_current_cursor_element);
			/*****
			* First check for onMouseOut event. Return if document has
			* been modified during event processing.
			*****/
			if(pa->event_mask & EVENT_MOUSEOUT)
			{
				if(_XmHTMLEventProcess(html, event, pa->events->onMouseOut))
					return;
			}
			if(HTML_ATTR(anchor_track_callback))
				_XmHTMLTrackCallback(html, event, NULL);
		}

		/* Check for onMouseOver event */
		if(anchor->event_mask & EVENT_MOUSEOVER)
		{
			/* Check return value of event processing */
			if(_XmHTMLEventProcess(html, event, anchor->events->onMouseOver))
				return;
		}

		/* remove highlight of previous anchor */
		if(HTML_ATTR(highlight_on_enter))
		{
			if(anchor_word)
			{
				/* unarm previous selection */
				if(HTML_ATTR(armed_anchor) &&
					HTML_ATTR(armed_anchor) != anchor_word->owner)
					_XmHTMLPaintAnchorLeave(html);
				/* highlight new selection */
				_XmHTMLPaintAnchorEntry(html, anchor_word->owner);
			}
			else /* unarm previous selection */
				if(HTML_ATTR(armed_anchor))
				_XmHTMLPaintAnchorLeave(html);
		}

		HTML_ATTR(anchor_current_cursor_element) = anchor;
		_XmHTMLTrackCallback(html, event, anchor);
		tka->DefineCursor(tka->dpy, tka->win, HTML_ATTR(anchor_cursor));
		return;
	}
	else if(anchor->event_mask & EVENT_MOUSEMOVE)
	{
		/*****
		* Current anchor. Need to call onMouseMove event if defined.
		* No need to check return value as we return immediatly after
		* event processing.
		*****/
		_XmHTMLEventProcess(html, event, anchor->events->onMouseMove);
	}
	/* we are already on the correct anchor, just return */
	_XmHTMLFullDebug(1, ("XmHTML.c: TrackMotion End, over current anchor\n"));
}

/*****
* Name: 		TrackFocus
* Return Type: 	void
* Description: 	focus tracker; calls XmNanchorTrackCallback if 
*				entering/leaving an anchor.
*				Also calls XmNfocusCallback when installed.
* In: 
*	w:			XmHTMLWidget
*	event:		FocusIn/FocusOut/Leave/Event structure
*	params:		additional args, unused
*	num_parmas:	no of additional args, unused
* Returns:
*	nothing
*****/
static void 
TrackFocus(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	/* need to use XtParent since we only get motion events from work_area */
	XmHTMLWidget html = (XmHTMLWidget)XtParent(w);
	ToolkitAbstraction *tka = NULL;

	/* no needless lingering in this routine */
	if(XtClass(XtParent(w)) != xmHTMLWidgetClass)
		return;

	/* ignore if we don't have to make any feedback to the user */
	if(!HTML_ATTR(need_tracking))
		return;

	/* get handle to the ToolkitAbstraction */
	tka = HTML_ATTR(tka);

	/* we are already on the correct anchor, just return */
	_XmHTMLFullDebug(1, ("XmHTML.c: TrackFocus Start.\n"));

	/* check event and act accordingly */
	switch(event->type)
	{
		case FocusIn:
			_XmHTMLFullDebug(1, ("XmHTML.c: TrackFocus, FocusIn.\n"));

			_XmHTMLFocusInCallback(html, event);

			/*****
			* Might seem silly to test for the presence of a window in the
			* current tka, but in some very rare cases it can happen that a
			* FocusIn event is generated *before* a SubStructure event is
			* generated. This leads to the weird possibility that we have an
			* event on a window which has yet to be mapped to the screen!
			* This leads to a BadValue error and so we need to check if we have
			* a valid window id before doing anything with it.
			*****/
			if(tka->win)
				tka->UndefineCursor(tka->dpy, tka->win);
			break;
		case LeaveNotify:
			/*****
			* LeaveNotify Events occur when the pointer focus is transferred
			* from the DrawingArea child to another window. This can occur
			* when the pointer is moved outside the Widget *OR* when a
			* ButtonPress event occurs ON the drawingArea. When that happens,
			* the pointer focus is transferred from the drawingArea to it's
			* parent, being the Widget itself. In this case the detail
			* detail member of the XEnterWindowEvent will be NotifyAncestor,
			* and we would want to ignore this event (as it will cause a
			* flicker of the screen or an unnecessary call to any installed
			* callbacks).
			*****/
			if(((XEnterWindowEvent*)event)->detail == NotifyAncestor)
				return;
			/* else fall through */
		case FocusOut:
			_XmHTMLFullDebug(1, ("XmHTML.c: TrackFocus, "
				"%s.\n", event->type == LeaveNotify ?
					"LeaveNotify" : "FocusOut"));

			/* invalidate current selection if there is one */
			if(HTML_ATTR(anchor_track_callback) && 
				HTML_ATTR(anchor_current_cursor_element))
				_XmHTMLTrackCallback(html, event, NULL);

			/* loses focus, remove anchor highlight */
			if(HTML_ATTR(highlight_on_enter) && HTML_ATTR(armed_anchor))
				_XmHTMLPaintAnchorLeave(html);

			HTML_ATTR(armed_anchor) = NULL;
			HTML_ATTR(anchor_current_cursor_element) = NULL;
			tka->UndefineCursor(tka->dpy, tka->win);

			/* final step: call focusOut callback */
			if(event->type == FocusOut)
				_XmHTMLFocusOutCallback(html, event);
			/* fall through */
		default:
			/* uninteresting event, throw away */
			break;
	}
}

/*****
* Name: 		CheckAnchorUnderlining
* Return Type: 	void
* Description: 	validate anchor underlining enumeration values.
* In: 
*	html:		target widget
*	req:		requester widget
* Returns:
*	nothing.
*****/
static void
CheckAnchorUnderlining(XmHTMLWidget html, XmHTMLWidget req)
{
	/* Anchor Underlining values */
	if(!XmRepTypeValidValue(underline_repid, req->html.anchor_underline_type, 
		(Widget)html))
		HTML_ATTR(anchor_underline_type) = XmSINGLE_LINE;
	else
		HTML_ATTR(anchor_underline_type) = ATTR_HTML(req,anchor_underline_type);

	/* Set corresponding private resources */
	switch(HTML_ATTR(anchor_underline_type))
	{
		case XmNO_LINE:
			HTML_ATTR(anchor_line) = NO_LINE;
			break;
		case XmSINGLE_DASHED_LINE:
			HTML_ATTR(anchor_line) = LINE_DASHED|LINE_UNDER|LINE_SINGLE;
			break;
		case XmDOUBLE_LINE:
			HTML_ATTR(anchor_line) = LINE_SOLID|LINE_UNDER|LINE_DOUBLE;;
			break;
		case XmDOUBLE_DASHED_LINE:
			HTML_ATTR(anchor_line) = LINE_DASHED|LINE_UNDER|LINE_DOUBLE;;
			break;
		case XmSINGLE_LINE:		/* default */
		default:
			HTML_ATTR(anchor_line) = LINE_SOLID | LINE_UNDER | LINE_SINGLE;
			break;
	}

	/* Visited Anchor Underlining values */
	if(!XmRepTypeValidValue(underline_repid, 
		ATTR_HTML(req, anchor_visited_underline_type), (Widget)html))
		HTML_ATTR(anchor_visited_underline_type) = XmSINGLE_LINE;
	else
		HTML_ATTR(anchor_visited_underline_type) = 
			ATTR_HTML(req, anchor_visited_underline_type);

	/* Set corresponding private resources */
	switch(HTML_ATTR(anchor_visited_underline_type))
	{
		case XmNO_LINE:
			HTML_ATTR(anchor_visited_line) = NO_LINE;
			break;
		case XmSINGLE_DASHED_LINE:
			HTML_ATTR(anchor_visited_line) = LINE_DASHED|LINE_UNDER|LINE_SINGLE;
			break;
		case XmDOUBLE_LINE:
			HTML_ATTR(anchor_visited_line) = LINE_SOLID|LINE_UNDER|LINE_DOUBLE;
			break;
		case XmDOUBLE_DASHED_LINE:
			HTML_ATTR(anchor_visited_line) = LINE_DASHED|LINE_UNDER|LINE_DOUBLE;
			break;
		case XmSINGLE_LINE:		/* default */
		default:
			HTML_ATTR(anchor_visited_line) = LINE_SOLID|LINE_UNDER|LINE_SINGLE;
			break;
	}

	/* Target Anchor Underlining values */
	if(!XmRepTypeValidValue(underline_repid, 
		HTML_ATTR(anchor_target_underline_type), (Widget)html))
		ATTR_HTML(req, anchor_target_underline_type) = XmSINGLE_DASHED_LINE;
	else
		HTML_ATTR(anchor_target_underline_type) = 
			ATTR_HTML(req, anchor_target_underline_type);

	/* Set corresponding private resources */
	switch(HTML_ATTR(anchor_target_underline_type))
	{
		case XmNO_LINE:
			HTML_ATTR(anchor_target_line) = NO_LINE;
			break;
		case XmSINGLE_LINE:
			HTML_ATTR(anchor_target_line) = LINE_SOLID|LINE_UNDER|LINE_SINGLE;
			break;
		case XmDOUBLE_LINE:
			HTML_ATTR(anchor_target_line) = LINE_SOLID|LINE_UNDER|LINE_DOUBLE;
			break;
		case XmDOUBLE_DASHED_LINE:
			HTML_ATTR(anchor_target_line) = LINE_DASHED|LINE_UNDER|LINE_DOUBLE;
			break;
		case XmSINGLE_DASHED_LINE:	/* default */
		default:
			HTML_ATTR(anchor_target_line) = LINE_DASHED|LINE_UNDER|LINE_SINGLE;
			break;
	}
}

/*****
* Name: 		CheckAlignment
* Return Type: 	void
* Description: 	checks and sets the alignment resources
* In: 
*	html:		target widget
*	req:		requestor widget
* Returns:
*	nothing.
*****/
static void
CheckAlignment(XmHTMLWidget html, XmHTMLWidget req)
{
	/* Set default alignment */
	if(ATTR_HTML(req, enable_outlining))
		HTML_ATTR(default_halign) = XmHALIGN_JUSTIFY;
	else
	{
		/* default alignment depends on string direction */
		if(HTML_ATTR(string_direction) == XmSTRING_DIRECTION_R_TO_L)
			HTML_ATTR(default_halign) = XmHALIGN_RIGHT;
		else
			HTML_ATTR(default_halign) = XmHALIGN_LEFT;

		/* verify alignment */
		if(XmRepTypeValidValue(string_repid, ATTR_HTML(req, alignment),
			(Widget)html))
		{
			if(HTML_ATTR(alignment) == XmALIGNMENT_BEGINNING)
				HTML_ATTR(default_halign) = XmHALIGN_LEFT;
			if(HTML_ATTR(alignment) == XmALIGNMENT_END)
				HTML_ATTR(default_halign) = XmHALIGN_RIGHT;
			else if(HTML_ATTR(alignment) == XmALIGNMENT_CENTER)
				HTML_ATTR(default_halign) = XmHALIGN_CENTER;
		}
	}
}

/*****
* Name:			_XmHTMLCvtStringToWarning
* Return Type: 	Boolean
* Description: 	converts a XmHTML XmCHTMLWarningType to it's internal value.
* In: 
*	dpy:		display with which this conversion is associated;
*	args:		any XrmValue arguments to this converter. Always NULL;
*	num_args:	no of args. Always 0;
*	from_val:	address and size of value to be converted;
*	to_val:		address where the converted value must be stored;
*	convert..:	data to be passed to the destructor routine. Since this
*				converter doesn't allocate any data, this argument is ignored.
* Returns:
*	True when the conversion was successfull, False if not.
*****/
Boolean
_XmHTMLCvtStringToWarning(Display *dpy, XrmValuePtr args, Cardinal *num_args,
	XrmValuePtr from_val, XrmValuePtr to_val, XtPointer *converter_data)
{
	static String warn_styles[] = {"unknown_element", "bad", "open_block",
					"close_block", "open_element", "nested", "violation"};
	Byte warn_values[] = {XmHTML_UNKNOWN_ELEMENT, XmHTML_BAD,
					XmHTML_OPEN_BLOCK, XmHTML_CLOSE_BLOCK, XmHTML_OPEN_ELEMENT,
					XmHTML_NESTED, XmHTML_VIOLATION};

	String warning = NULL;
	int i;
	String ptr = (String)from_val->addr;
	Byte ret_val = XmHTML_NONE;

	if(*num_args != 0)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "_XmHTMLCvtStringToWarning"),
			XMHTML_MSG_19);
		return(False);
	}

	/* hmm, shouldn't happen */
	if(ptr == NULL || *ptr == '\0' || from_val->size < 3)
		goto end;

	/* copy so we scan it safely */
	warning = my_strndup(ptr, from_val->size);

	/* check if we have NONE */
	if(my_strcasestr(warning, "none"))
		goto end;

	/* check if we have HTML_ALL */
	if(my_strcasestr(warning, "all"))
	{
		ret_val = XmHTML_ALL;
		goto end;
	}

#define NUM_WARNINGS 7
	/* now scan the string for the possible warning types */
	for(i = 0; i < NUM_WARNINGS; i++)
	{
		if(my_strcasestr(warning, warn_styles[i]))
			ret_val |= warn_values[i];
	}
#undef NUM_WARNINGS

	/* this is an error */
	if(ret_val == XmHTML_NONE)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "_XmHTMLCvtStringToWarning"),
			XMHTML_MSG_20, warning);
		free(warning);
		return(False);
	}

end:
	/* no longer needed, free it */
	if(warning != NULL)
		free(warning);

	if(to_val->addr != NULL)
	{
		if(to_val->size < sizeof(Byte))
		{
			to_val->size = sizeof(Byte);
			return(False);
		}
		*(Byte*)to_val->addr = ret_val;
		return(True);
	}
	else
	{
		static Byte static_val;
		static_val = ret_val;
		to_val->addr = (char *)&static_val;
		to_val->size = sizeof(Byte);
		return(True);
	}
}

/*****
* Name:			_XmHTMLAdjustVerticalScrollValue
* Return Type: 	void
* Description: 	adjust slider value so it does not exceed the maximum
*				slider value.
* In: 
*	vsb:		vertical scrollbar widget id;
*	value:		current slider value.
* Returns:
*	nothing, but value can be updated upon return.
*****/
void
_XmHTMLAdjustVerticalScrollValue(Widget vsb, int *value)
{
	int max = 0, size = 0;
	XtVaGetValues(vsb,
		XmNmaximum, &max,
		XmNsliderSize, &size,
		NULL);
	if(*value > (max - size))
		*value = (max - size);
}

/********
****** Public XmHTML Functions
********/

/*****
* Name: 		XmCreateHTML
* Return Type: 	Widget
* Description: 	creates a XmHTML widget
* In: 
*	parent:		widget to act as parent for this new XmHTMLWidget
*	name:		name for the new widget
*	arglist:	arguments for this new XmHTMLWidget
*	argcount:	no of arguments
* Returns:
*	a newly created widget. This routine exits if parent is NULL or a subclass
*	of XmGadget.
*****/
Widget
XmCreateHTML(Widget parent, String name, ArgList arglist, Cardinal argcount)
{
#ifdef MOTIF_IN_DLL
	/*****
	* See comment above (search for other instance of this ifdef)
	* We must do this BEFORE creating the widget or we'd have serious
	* problems...
	*****/
	xmHTMLClassRec.core_class.superclass = (WidgetClass)&xmManagerClassRec;
#endif

	if(parent && !XmIsGadget(parent))
		return(XtCreateWidget(name, xmHTMLWidgetClass, parent, 
			arglist, argcount));

	/* invalid parent, needs a widget, not a gadget */
	__XmHTMLWarning(__WFUNC__(parent, "XmCreateHTML"),
		XMHTML_MSG_34, (parent ? "Invalid" : "NULL"), "XmCreateHTML");

	/* keep compiler happy */
	return(NULL);
}
