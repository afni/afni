/*****
* toolkit.h : XmHTML Motif function & data type wrappers
*
* This file Version	$Revision$
*
* Creation date:		Thu Jan  8 04:32:19 GMT+0100 1998
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
* About this file:
*
*	XmHTML is originally an Xt/Motif based Widget. To make porting to
*	other toolkits a bit easier, XmHTML uses a toolkit abstraction and a 
*	set of defines that allow you to replace all X/Xt/Xm functions used by
*	XmHTML.
*
*	All Xt/Xm functions are wrapped together in what I call a
*	ToolkitAbstraction (see XmHTMLP.h for the definition and motif.c for
*	the Motif ToolkitAbstraction).
*
*	There is one assumption though: that you never include a header containing
*	Xt or Xm specifics. If you do this however, you will need to override
*	a whole bunch of routines, typedefs and constants (don't worry, they
*	are all listed).
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
* Revision 1.2  1998/04/27 07:03:52  newt
* more tka stuff
*
* Revision 1.1  1998/04/04 06:27:29  newt
* Initial Revision
*
*****/ 

#ifndef _toolkit_h_
#define _toolkit_h_

#define XmHTMLPrivateHeader "XmHTML/XmHTMLP.h"
#define HAVE_XCCP_H

/* Every XmHTML source file requires these two X11 includes */
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>

/*****
* X types & constants
* We need defines for these as typedefs would lead to a tremendous amount
* of warnings (with most compilers that is). Lint would go absolutely crazy...
*****/

/* common types */
#define XCOLOR					XColor
#define COLORMAP				Colormap
#define PIXMAP					Pixmap
#define WINDOW					Window
#define DRAWABLE				Drawable
#define XIMAGE					XImage
#define XFONTSTRUCT				XFontStruct
#define XFONTSET				XFontSet
#define VISUAL					Visual
#define XGC						GC
#define XCHARSTRUCT				XCharStruct

#define XEVENT					XEvent
#define XBUTTONPRESSEDEVENT		XButtonPressedEvent
#define XBUTTONRELEASEDEVENT	XButtonReleasedEvent

/* byte ordering for this host */
#define LSBFIRST					LSBFirst
#define MSBFIRST					MSBFirst

/* Color access macros */
#define GETP(c)	(c).pixel
#define GETR(c)	(c).red
#define GETG(c)	(c).green
#define GETB(c)	(c).blue
#define GETF(c)	(c).flags

#define GETPP(c)	(c)->pixel
#define GETPR(c)	(c)->red
#define GETPG(c)	(c)->green
#define GETPB(c)	(c)->blue
#define GETPF(c)	(c)->flags

/*****
* This macro should return a XFontStruct
*****/
#define FontIsXFont(font)			font

#define XTEVENTPROC(f,a,b,c,d) \
	f(a,b,c,d)

/*****
* Xlib Function Wrappers
*****/

#define TkaCurrentTime CurrentTime

/*****
* XFontStruct access macro's
*****/
#define TkaFont(XF)				((XF)->xfont)
#define TkaFontLeftBearing(XF)	((XF)->max_bounds.lbearing)
#define TkaFontRightBearing(XF)	((XF)->max_bounds.rbearing)
#define TkaFontWidth(XF)		((XF)->max_bounds.width)
#define TkaFontAscent(XF)		((XF)->ascent)
#define TkaFontDescent(XF)		((XF)->descent)
#define TkaFontMaxAscent(XF)	((XF)->max_bounds.ascent)
#define TkaFontMaxDescent(XF)	((XF)->max_bounds.descent)
#define TkaFontLineheight(XF)	((XF)->ascent + (XF)->descent)

/*****
* XImage wrappers & access macros.
*****/

#define TkaImageData(image) \
	(image->data)

#define TkaImageBitsPerPixel(image) \
	(image->bits_per_pixel)

#define TkaImageBytesPerLine(image) \
	(image->bytes_per_line)

#define TkaImageByteOrder(image) \
	(image->byte_order)

#define TkaImageBitmapBitOrder(image) \
	(image->bitmap_bit_order)

/* check support for various combinations of bits per pixel */
#define TkaImageCheck2bpp(image) \
	(image->bits_per_pixel == 2)

#define TkaImageCheck4bpp(image) \
	(image->bits_per_pixel == 4)

#define TkaImageCheck24bpp(image) \
	(image->bits_per_pixel == 24)

#define TkaImageCheck32bpp(image) \
	(image->bits_per_pixel == 32)

/*****
* Xt Function wrappers
*****/

#define TkaWidgetName(w)		XtName((Widget)w)
#define TkaWidgetClassName(w)	(XtClass((Widget)w)->core_class.class_name)

/* Check for the presence of a callback function */
#define TkaHasCallback(W,D) ATTR_HTML(W, D)

/* Activate a callback function */
#define TkaCallCallbackList(W,C,D) \
	XtCallCallbackList((Widget)W, ATTR_HTML(W,C##_callback), D)

/* Set the position of a scrollbar slider */
#define TkaScrollbarSliderSetPosition(W,V) \
	XtVaSetValues(W, XmNvalue, V, NULL)

/*****
* Motif wrappers
*****/

/* none */

/*****
* Widget internal access wrappers
*****/

/* Main access method. *REQUIRES* a variable of html to be known */
#define HTML_ATTR(field)	((XmHTMLWidget)html)->html.field
#define CORE_ATTR(field)	((Widget)html)->core.field
#define MGR_ATTR(field)		((XmHTMLWidget)html)->manager.field

/* subclass access methods */
#define ATTR_CORE(widget,field) \
	(widget)->core.field

#define ATTR_MGR(widget,field) \
	(widget)->manager.field

#define ATTR_HTML(widget,field) \
	((XmHTMLWidget)widget)->html.field

/* widely used subclass properties */

/* get XFont used by the default XmHTMLFont */
#define GetDefaultXFont(widget) \
	ATTR_HTML(widget,default_font->xfont)

#define TkaGetBackground(widget) \
	ATTR_CORE(widget,background_pixel)

#define TkaGetColormap(widget) ATTR_CORE(widget,colormap)

#define TkaVisualGetDepth(widget) \
	ATTR_HTML(widget,xcc->visualInfo->depth)

#define TkaVisualGetMapEntries(visual) \
	visual->map_entries

/* Don't add anything after this endif! */
#endif /* _toolkit_h_ */
