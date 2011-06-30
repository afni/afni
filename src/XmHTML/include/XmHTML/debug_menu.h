/*****
* debug_menu.h : XmHTML debug menu for runtime selection of debug options.
*
* This file Version	$Revision$
*
* Creation date:		Sun Aug 31 14:23:37 GMT+0100 1997
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
* $Source$
*****/
/*****
* ChangeLog 
* $Log$
* Revision 1.1  2011/06/30 16:08:41  rwcox
* Cadd
*
* Revision 1.2  1997/10/23 00:24:55  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.1  1997/08/31 17:45:27  newt
* Initial Revision
*
*****/ 

#ifndef _debug_menu_h_
#define _debug_menu_h_

/*****
* Add a XmHTML "Debug" menu to your menubar. This menu allows you to select
* XmHTML debug output while an application is running. The menu itself consists
* of togglebuttons, one for each source file (or group of source files) that
* allow selectable debug output.
*
* html:
*	XmHTMLWidget id. Used to get & set the values of the XmHTML debug
*	resources (XmHTML resources starting with XmNdebug);
* menubar:
*	Widget id of the menubar to which the debug menu should be added;
* label:
*	title to use for this menu. When NULL "Debug" is used.
*	The first character of this string is used as the menu accelerator.
*****/
extern void _XmHTMLAddDebugMenu(Widget html, Widget menubar, String label);

/* Don't add anything after this endif! */
#endif /* _debug_menu_h_ */
