/*****
* Balloon.h : XmBalloon Widget public header file
*
* This file Version	$Revision$
*
* Creation date:		Sun Nov  2 19:18:40 GMT+0100 1997
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
* $Source$
*****/
/*****
* ChangeLog
* $Log$
* Revision 1.1  2011/06/30 16:08:40  rwcox
* Cadd
*
* Revision 1.1  1998/04/04 06:27:16  newt
* Initial Revision
*
*****/

#ifndef _Balloon_h_
#define _Balloon_h_

/* required includes */
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <XmHTML/HTML.h>

_XFUNCPROTOBEGIN

/* XmBalloon type defines */
typedef struct _XmBalloonClassRec *XmBalloonWidgetClass;
typedef struct _XmBalloonRec *XmBalloonWidget;

externalref WidgetClass xmBalloonWidgetClass;

/* XmBalloonWidget subclassing macro */
#ifndef XmIsBalloon
#define XmIsBalloon(w) XtIsSubclass(w, xmBalloonWidgetClass)
#endif /* XmIsBalloon */

/*****
* Possible values for XmNcornerStyle
* The TFolder widget also has this so we check against it.
*****/
#ifndef _XmTabFolder_h
enum{
	XmCORNER_STRAIGHT = 0,
	XmCORNER_BEVELED,
	XmCORNER_SLANT,
	XmCORNER_ROUND
};
#endif

/*****
* Possible values for XmNballoonStyle
*****/
enum{
	XmBALLOON_SQUARE = 0,
	XmBALLOON_SHAPED
};

/******* Public Function Declarations ********/
/* create an XmBalloon Widget */
Widget XmCreateBalloon(Widget parent, String name, ArgList arglist,
	Cardinal argcount);

/*****
* Popup an XmBalloon Widget. x and y specify the position where the balloon
* should popup. They are relative to the upper-left corner of the Balloon's
* parent. If both are 0, the balloon is popped up right under the current
* pointer's position.
*****/
void XmBalloonPopup(Widget w, Position x, Position y, String label);

/* popdown an XmBalloon Widget */
void XmBalloonPopdown(Widget w);

_XFUNCPROTOEND

/* Don't add anything after this endif! */
#endif /* _Balloon_h_ */
