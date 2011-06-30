#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* debug_menu.c : XmHTML debug menu, allows runtime selection of debug levels.
*
* This file Version	$Revision$
*
* Creation date:		Sun Aug 31 14:23:40 GMT+0100 1997
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
* Revision 1.1  2011/06/30 16:10:30  rwcox
* Cadd
*
* Revision 1.4  1998/04/27 06:59:05  newt
* tka stuff
*
* Revision 1.3  1998/04/04 06:28:04  newt
* XmHTML Beta 1.1.3
*
* Revision 1.2  1997/10/23 00:24:54  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.1  1997/08/31 17:45:23  newt
* Initial Revision
*
*****/ 
#ifndef NDEBUG
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>

#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleBG.h>
#include <Xm/SeparatoG.h>

#include "toolkit.h"
#include XmHTMLPrivateHeader

#include "debug.h"				/* debug arrays */
#include "debug_menu.h"			/* proto's */

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/
typedef struct{
	String file;
	int file_id;
}debugRec;

/*** Private Function Prototype Declarations ****/
static void debugCB(Widget w, XtPointer client_data,
	XmToggleButtonCallbackStruct *cbs);
static void debugResCB(Widget w, XtPointer client_data,
	XmToggleButtonCallbackStruct *cbs);

/*** Private Variable Declarations ***/
#define MAX_ENTRIES		15
static debugRec debugLevels[MAX_ENTRIES] = {
	{ "XmHTML.c", 1},
	{ "format.c", 2},
	{ "callbacks.c", 3},
	{ "parse.c", 4},
	{ "paint.c", 5},
	{ "images.c/XmImage.c", 6},
	{ "colors.c", 7},
	{ "fonts.c", 8},
	{ "XCC.c", 9},
	{ "map.c", 10},
	{ "frames.c", 11},
	{ "forms.c", 12},
	{ "quantize.c", 13},
	{ "plc.c", 14},
	{ "Image readers", 15}
};

/*****
* Name: 		debugCB
* Return Type: 	void
* Description: 	debug level callback. Sets/Unsets the selected debug level.
* In: 
*	w:			widget id of toggleButton triggering this callback;
*	client_..:	selected debug level;
*	cbs:		toggleButton callback data;
* Returns:
*	nothing.
*****/
static void
debugCB(Widget w, XtPointer client_data, XmToggleButtonCallbackStruct *cbs)
{
	__rsd__debug_levels_defined[(int)client_data] = cbs->set;
}

/*****
* Name: 		debugResCB
* Return Type: 	void
* Description: 	callback for XmHTML debug resources.
* In: 
*	w:			widget id of toggleButton triggering this callback;
*	client_..:	id of selected button;
*	cbs:		toggleButton callback data;
* Returns:
*	nothing.
*****/
static void
debugResCB(Widget w, XtPointer client_data, XmToggleButtonCallbackStruct *cbs)
{
	Widget html = NULL;

	XtVaGetValues(w, XmNuserData, &html, NULL);

	if(html == NULL)
		return;

	switch((int)client_data)
	{
		case MAX_ENTRIES+1:
			XtVaSetValues(html,
				XmNdebugDisableWarnings, (Boolean)cbs->set, NULL);
			break;
		case MAX_ENTRIES+2:
			XtVaSetValues(html,
				XmNdebugEnableFullOutput, (Boolean)cbs->set, NULL);
			break;
		case MAX_ENTRIES+3:
			XtVaSetValues(html,
				XmNdebugSaveClipmasks, (Boolean)cbs->set, NULL);
			break;
		case MAX_ENTRIES+4:
			XtVaSetValues(html,
				XmNdebugNoAnimationLoopCount, (Boolean)cbs->set, NULL);
			break;
		default:
			fprintf(stderr, "debugResCB: impossible selection (id = %i)\n",
				(int)client_data);
			break;
	}
}

/*****
* Name:			_XmHTMLAddDebugMenu
* Return Type: 	void
* Description: 	adds a "Debug" menu to the menubar of an application.
*				Allows one to select XmHTML debug output when an application
*				is running.
* In: 
*	html:		XmHTMLWidget id;
*	menubar:	menubar Widget id to which a debug menu should be added;
*	label:		the label to use for this menu. When NULL, "Debug" is used;
* Returns:
*	nothing.
*****/
void
_XmHTMLAddDebugMenu(Widget html, Widget menubar, String label)
{
	Widget tb, debug_menu;
	int i;
	XmString xms;
	char accel;
	Boolean full = False, nowarn = False, savemasks = False, noloop = False;

	/* sanity */
	if(html == NULL || !XmIsHTML(html) || menubar == NULL)
		return;

	XtVaGetValues(html,
		XmNdebugDisableWarnings, &nowarn,
		XmNdebugEnableFullOutput, &full,
		XmNdebugSaveClipmasks, &savemasks,
		XmNdebugNoAnimationLoopCount, &noloop,
		NULL);

	if(label == NULL)
		label = "Debug";

	accel = label[0];

	/* create debug menu */
	debug_menu = XmCreatePulldownMenu(menubar, "debugPulldown", NULL, 0);

	/* create options button */
	xms = XmStringCreateLocalized(label);
	tb = XtVaCreateManagedWidget(label,
		xmCascadeButtonWidgetClass, menubar,
		XmNlabelString, xms,
		XmNmnemonic, accel,
		XmNsubMenuId, debug_menu,
		NULL);
	XmStringFree(xms);	/* don't need it anymore */

	for(i = 0; i < MAX_ENTRIES; i++)
	{
		tb = XtVaCreateManagedWidget(debugLevels[i].file,
			xmToggleButtonGadgetClass, debug_menu,
			XmNset, __rsd__debug_levels_defined[debugLevels[i].file_id],
			NULL);
		XtAddCallback(tb, XmNvalueChangedCallback, 
			(XtCallbackProc)debugCB, (XtPointer)debugLevels[i].file_id);
	}

	XtVaCreateManagedWidget("separator",
		xmSeparatorGadgetClass, debug_menu, NULL);

	tb = XtVaCreateManagedWidget("Disable Warnings",
		xmToggleButtonGadgetClass, debug_menu,
		XmNset, nowarn,
		XmNuserData, html,		/* need to have this */
		NULL);
	XtAddCallback(tb, XmNvalueChangedCallback, 
		(XtCallbackProc)debugResCB, (XtPointer)(MAX_ENTRIES+1));

	tb = XtVaCreateManagedWidget("Save Clipmasks",
		xmToggleButtonGadgetClass, debug_menu,
		XmNset, savemasks,
		XmNuserData, html,		/* need to have this */
		NULL);
	XtAddCallback(tb, XmNvalueChangedCallback, 
		(XtCallbackProc)debugResCB, (XtPointer)(MAX_ENTRIES+2));

	tb = XtVaCreateManagedWidget("No Animation loopcount",
		xmToggleButtonGadgetClass, debug_menu,
		XmNset, noloop,
		XmNuserData, html,		/* need to have this */
		NULL);
	XtAddCallback(tb, XmNvalueChangedCallback, 
		(XtCallbackProc)debugResCB, (XtPointer)(MAX_ENTRIES+3));

	tb = XtVaCreateManagedWidget("Full Output",
		xmToggleButtonGadgetClass, debug_menu,
		XmNset, full,
		XmNuserData, html,		/* need to have this */
		NULL);
	XtAddCallback(tb, XmNvalueChangedCallback, 
		(XtCallbackProc)debugResCB, (XtPointer)(MAX_ENTRIES+4));

	/* show it */
	XtManageChild(debug_menu);
}
#endif	/* !NDEBUG */
