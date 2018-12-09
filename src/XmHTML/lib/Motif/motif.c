#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* tka-motif.c : XmHTML/Motif ToolkitAbstraction
*
* This file Version	$Revision$
*
* Creation date:		Thu Feb 26 22:33:21 GMT+0100 1998
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
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
* Revision 1.1  2011/06/30 16:10:30  rwcox
* Cadd
*
* Revision 1.2  1998/04/27 07:01:23  newt
* Added some more functions for proper tka handling
*
* Revision 1.1  1998/04/04 06:27:24  newt
* Initial Revision
*
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#include <Xm/XmP.h>
#include <Xm/DrawP.h>		/* Private render functions */
#include <Xm/XmStrDefs.h>	/* For motif XmN macros */
#include <Xm/DrawingA.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/RepType.h>

/* Our private header files */
#include "toolkit.h"
#include XmHTMLPrivateHeader

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/

/*** Private Variable Declarations ***/

/*****
* I18N support.
*
* For performance reasons, there are two versions of the TextWidth and
* DrawString routines: a set that fully supports I18N and a set that
* doesn't.
*****/

#ifdef I18N
/*****
* Name:			I18NTextWidth
* Return Type: 	int
* Description: 	full I18N version for computing pixel-width of the
*				given string.
* In:
*	font:		font to be used when computing pixel width;
*	string:		string for which to compute pixel width;
*	count:		no of characters in string.
* Returns:
*	pixel width of given string.
*****/
static int
I18NTextWidth(XmHTMLfont *font, const char* string, int count)
{
	switch(font->type)
	{
		case XmHTML_FONT:
			{
				XFontStruct *xfont = (XFontStruct*)font->xfont;

				/*****
				* If this is a two-byte font, assume that the given
				* string consists of two-byte characters.
				*****/
				if((xfont->min_byte1 == 0) && (xfont->max_byte1 == 0))
					return(XTextWidth(xfont, string, count));
				else
					return(XTextWidth16(xfont, (XChar2b*)string, count/2));
			}
			break;
		case XmHTML_FONTSET:
			/*****
			* A Fontset indicates the string to be rendered is a multibyte
			* string. Act accordingly.
			*****/
			return(XmbTextEscapement((XFontSet)font->xfont, string, count));
			break;
	}
	_XmHTMLError(__WFUNC__(NULL, "I18NTextWidth"),
		"Unknown fontset type %i", font->type);

	/* not reached */
	return(0);
}

/*****
* Name:			I18NDrawString
* Return Type: 	int
* Description: 	Full I18N XDrawString.
* In:
*	display:	display to be used;
*	drawable:	area on which to render text;
*	font:		font to be used;
*	gc:			gc to be used for rendering;
*	x:			baseline left x-coordinate;
*	y:			baseline y-coordinate;
*	string:		string to be rendered;
*	length:		number of characters string to be rendered;
* Returns:
*	nothing.
*****/
static int
I18NDrawString(Display *display, Drawable drawable, XmHTMLfont *font,
	GC gc, int x, int y, const char *string, int length)
{
	switch(font->type)
	{
		case XmHTML_FONT:
			{
				XFontStruct *xfont = (XFontStruct*)font->xfont;
				static Font fid;

				/*****
				* check if the last font is equal to the new font. If it isn't,
				* set it into the gc and save the font id for later reference.
				*****/
				if(xfont->fid != fid)
				{
					fid = xfont->fid;
					XSetFont(display, gc, fid);
				}

				if((xfont->min_byte1 == 0) && (xfont->max_byte1 == 0))
					XDrawString(display, drawable, gc, x, y, string, length);
				else
					XDrawString16(display, drawable, gc, x, y, (XChar2b*)string,
						length / 2);
			}
			return(1);
		case XmHTML_FONTSET:
			XmbDrawString(display, drawable, (XFontSet)font->xfont, gc, x, y,
				string, length);
			return(1);
	}
	_XmHTMLError(__WFUNC__(NULL, "I18NDrawString"),
		"Unknown fontset type %i", font->type);

	/* not reached */
	return(0);
}

#else /* I18N */

/*****
* Name:			I18NTextWidth
* Return Type: 	int
* Description: 	XTextWidth without I18N support.
* In:
*	font:		font to be used when computing pixel width;
*	string:		string for which to compute pixel width;
*	count:		no of characters in string.
* Returns:
*	pixel width of given string.
*****/
static int
I18NTextWidth(XmHTMLfont *font, const char* string, int count)
{
	return(XTextWidth((XFontStruct*)font->xfont, string, count));
}

/*****
* Name:			I18NDrawString
* Return Type: 	int
* Description: 	XDrawString without I18N support.
* In:
*	display:	display to be used;
*	drawable:	area on which to render text;
*	font:		font to be used;
*	gc:			gc to be used for rendering;
*	x:			baseline left x-coordinate;
*	y:			baseline y-coordinate;
*	string:		string to be rendered;
*	length:		size of string to be rendered;
* Returns:
*	nothing.
* Note:
*	For performance reasons, this routine does *NOT* set the font in the gc.
*	It is up to the caller to do that *before* calling this routine.
*****/
static int
I18NDrawString(Display *display, Drawable drawable, XmHTMLfont *font,
	GC gc, int x, int y, const char *string, int length)
{
	static Font fid;

	/*****
	* check if the last font is equal to the new font. If it isn't,
	* set it into the gc and save the font id for later reference.
	*****/
	if(((XFontStruct*)font->xfont)->fid != fid)
	{
		fid = ((XFontStruct*)font->xfont)->fid;
		XSetFont(display, gc, fid);
	}
	return(XDrawString(display, drawable, gc, x, y, string, length));
}
#endif /* I18 N */

/*****
* Name:			XSetFont_wrapper
* Return Type: 	nothing
* Description: 	sets a font into the given GC.
* In:
*	display:	display being used;
*	gc:			gc to be modified;
*	font:		font to be set.
* Returns:
*	return value from XSetFont (ignored by caller).
* Note:
*	This is a wrapper function as XmHTML gives this routine a XmHTMLfont
*	instead of a Font XID. This is done to increase portability.
*****/
static int
XSetFont_wrapper(Display *display, GC gc, XmHTMLfont *font)
{
	return(XSetFont(display, gc, ((XFontStruct*)font->xfont)->fid));
}

/*****
* Name:			DestroyImage
* Return Type: 	void
* Description: 	XDestroyImage is only defined as a macro, which we
*				obviously can't call directly as a function. Instead
*				we define a function that calls the real macro.
* In:
*	image:		ptr to XImage to be destroyed;
* Returns:
*	nothing.
*****/
static void
DestroyImage(XImage *image)
{
	XDestroyImage(image);
}

static unsigned long
GetPixelWrapper(XImage *ximage, Dimension x, Dimension y)
{
	return(XGetPixel(ximage, x, y));
}

static void
DrawImage(XmHTMLWidget html, XmHTMLImage *image, GC gc,
	int src_x, int src_y, unsigned int width, unsigned int height,
	int dest_x, int dest_y)
{
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	tka->CopyArea(tka->dpy, image->pixmap, tka->win, gc, src_x, src_y,
		width, height, dest_x, dest_y);
}

#ifdef NO_XM_ILLEGAL_ACCESS
static void
_XmHTMLDrawShadows(Display *display, Drawable drawable, GC top_shadow_GC,
	GC bottom_shadow_GC, Position x, Position y, Dimension width,
	Dimension height, Dimension shadow_thick, Byte shadow_type)
{
	switch(shadow_type)
	{
		case XmSHADOW_IN:
			/* top & left border */
			XFillRectangle(display, drawable, bottom_shadow_GC, x, y, width, 1);
			XFillRectangle(display, drawable, bottom_shadow_GC, x, y, 1,
				height-1);

			/* bottom & right border */
			XFillRectangle(display, drawable, top_shadow_GC, x + 1,
				 y + height - 1, width - 1, 1);
			XFillRectangle(display, drawable, top_shadow_GC, x - 1, y + 1, 1,
				height - 2);
			break;
		case XmSHADOW_OUT:
			/* top & left border */
			XFillRectangle(display, drawable, top_shadow_GC, x, y, width, 1);
			XFillRectangle(display, drawable, top_shadow_GC, x, y, 1, height-1);

			/* bottom & right border */
			XFillRectangle(display, drawable, bottom_shadow_GC, x + 1,
				y + height - 1, width - 1, 1);
			XFillRectangle(display, drawable, bottom_shadow_GC, x - 1,
				y + 1, 1, height - 2);
			break;
		default:
			break;
	}
}
#endif

static ToolkitAbstraction*
_CreateMotifTka(void)
{
	static ToolkitAbstraction *tka;

	tka = (ToolkitAbstraction*)malloc(sizeof(ToolkitAbstraction));

	tka->dpy = NULL;
	tka->win = None;
	tka->defaultRoot = None;

	/* GC properties */
	tka->fill_style[GC_FILL_SOLID]           = FillSolid;
	tka->fill_style[GC_FILL_TILED]           = FillTiled;
	tka->fill_style[GC_FILL_STIPPLED]        = FillStippled;
	tka->fill_style[GC_FILL_OPAQUE_STIPPLED] = FillOpaqueStippled;

	tka->cap_style[GC_CAP_NOT_LAST]   = CapNotLast;
	tka->cap_style[GC_CAP_BUTT]       = CapButt;
	tka->cap_style[GC_CAP_ROUND]      = CapRound;
	tka->cap_style[GC_CAP_PROJECTING] = CapProjecting;

	tka->line_style[GC_LINE_SOLID]       = LineSolid;
	tka->line_style[GC_LINE_ON_OFF_DASH] = LineOnOffDash;
	tka->line_style[GC_LINE_DOUBLE_DASH] = LineDoubleDash;

	tka->join_style[GC_JOIN_MITER] = JoinMiter;
	tka->join_style[GC_JOIN_ROUND] = JoinRound;
	tka->join_style[GC_JOIN_BEVEL] = JoinBevel;

	tka->coord_mode[GC_COORDMODE_ORIGIN] = CoordModeOrigin;
	tka->coord_mode[GC_COORDMODE_PREVIOUS] = CoordModePrevious;

	/* GC functions */
	tka->gc_func[GC_GXcopy] = GXcopy;

	/* GC functions */
	tka->CreateGC      = XCreateGC;
	tka->FreeGC        = XFreeGC;
	tka->CopyGC        = XCopyGC;
	tka->SetFunction   = XSetFunction;
	tka->SetClipMask   = XSetClipMask;
	tka->SetClipOrigin = XSetClipOrigin;
	tka->SetTile       = XSetTile;
	tka->SetTSOrigin   = XSetTSOrigin;
	tka->SetFillStyle  = XSetFillStyle;
	tka->SetFont       = XSetFont_wrapper;
	tka->SetForeground = XSetForeground;
	tka->SetBackground = XSetBackground;
	tka->SetLineAttributes = XSetLineAttributes;

	/* Font Allocation functions */
	tka->LoadQueryFont   = XLoadQueryFont;
	tka->FreeFont        = XFreeFont;
	tka->GetFontProperty = XGetFontProperty;

	/* Cursor & pointer functions */
	tka->UngrabPointer = XUngrabPointer;
	tka->DefineCursor  = XDefineCursor;
	tka->UndefineCursor= XUndefineCursor;
	tka->FreeCursor    = XFreeCursor;

	/* Color functions */
	tka->ParseColor    = XParseColor;
	tka->AllocColor    = XAllocColor;
	tka->QueryColor    = XQueryColor;
	tka->QueryColors   = XQueryColors;
	tka->FreeColors    = XFreeColors;

	/* Pixmap functions */
	tka->CreatePixmap  = XCreatePixmap;
	tka->FreePixmap    = XFreePixmap;
	tka->CreatePixmapFromBitmapData = XCreatePixmapFromBitmapData;

	/* XImage functions */
	tka->CreateImage   = XCreateImage;
	tka->DestroyImage  = DestroyImage;
	tka->PutImage      = XPutImage;
	tka->GetImage      = XGetImage;
	tka->GetPixel      = GetPixelWrapper;	/* XGetPixel is a macro */
	tka->DrawImage     = DrawImage;
	tka->DrawAnchorData= NULL;

	/* string/text functions */
	tka->TextWidth     = I18NTextWidth;
	tka->TextExtents   = XTextExtents;

	/* Render functions */
	tka->DrawString    = I18NDrawString;
	tka->DrawLine      = XDrawLine;
	tka->DrawLines     = XDrawLines;
	tka->DrawRectangle = XDrawRectangle;
	tka->FillRectangle = XFillRectangle;
	tka->DrawArc       = XDrawArc;
	tka->FillArc       = XFillArc;

	/* misc. functions */
	tka->CopyArea      = XCopyArea;
	tka->ClearArea     = XClearArea;
	tka->Sync          = XSync;

	/****
	* X Intrinsic wrappers
	* First undefine any macro versions of these functions so the real
	* functions will be used. The macro versions will be present 'cause
	* we have include <X11/IntrinsicP.h> (via XmHTMLPrivateHeader)
	*****/
#ifdef XtIsRealized
#undef XtIsRealized
#endif
#ifdef XtIsManaged		/* I've only seen this on SunOs as a macro */
#undef XtIsManaged
#endif

	tka->IsRealized      = XtIsRealized;
	tka->IsManaged       = XtIsManaged;
	tka->ManageChild     = XtManageChild;
	tka->UnmanageChild   = XtUnmanageChild;
	tka->MoveWidget      = XtMoveWidget;
	tka->ResizeWidget    = XtResizeWidget;
	tka->ConfigureWidget = XtConfigureWidget;
	tka->DestroyWidget   = XtDestroyWidget;
	tka->SetMappedWhenManaged = XtSetMappedWhenManaged;
	tka->RemoveTimeOut   = XtRemoveTimeOut;
	tka->AddTimeOut      = XtAppAddTimeOut;

	/* Motif Wrappers */
#ifndef NO_XM_ILLEGAL_ACCESS
	tka->DrawShadows     = _XmDrawShadows;
#else
	tka->DrawShadows     = _XmHTMLDrawShadows;
#endif

	tka->data = NULL;
	tka->FreeData = NULL;

	return(tka);
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
ToolkitAbstraction*
XmHTMLTkaCreate(void)
{
	return(_CreateMotifTka());
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
XmHTMLTkaDestroy(ToolkitAbstraction *tka)
{
	/* call application release function if external data was stored */
	if(tka->data != NULL && tka->FreeData != NULL)
		tka->FreeData(tka->data);

	free(tka);
}

/*****
* Name:			XmHTMLTkaCopy
* Return Type: 	ToolkitAbstraction*
* Description:
* In:
*
* Returns:
*
*****/
ToolkitAbstraction*
XmHTMLTkaCopy(ToolkitAbstraction *tka)
{
	static ToolkitAbstraction *tka_ret;

	tka_ret = (ToolkitAbstraction*)malloc(sizeof(ToolkitAbstraction));

	memcpy((void*)tka_ret, tka, sizeof(ToolkitAbstraction));

	return(tka_ret);
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
XmHTMLTkaSetDrawable(ToolkitAbstraction *tka, Drawable drawable)
{
	tka->win = drawable;
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
XmHTMLTkaSetDisplay(ToolkitAbstraction *tka, Widget w)
{
	tka->dpy = XtDisplay(w);

	/* and set display dimensions (we use it determine the dpi of the screen)*/
	tka->width    = DisplayWidth(tka->dpy, DefaultScreen(tka->dpy));
	tka->height   = DisplayHeight(tka->dpy, DefaultScreen(tka->dpy));
	tka->widthMM  = DisplayWidthMM(tka->dpy, DefaultScreen(tka->dpy));
	tka->heightMM = DisplayHeightMM(tka->dpy, DefaultScreen(tka->dpy));

	/* fallback window */
	tka->defaultRoot = DefaultRootWindow(tka->dpy);
}

/*****
* Name: 		XmHTMLTkaRecomputeColors
* Return Type: 	void
* Description: 	computes new values for top and bottom shadows and the
*				highlight color based on the current background color.
* In:
*	html:		XmHTMLWidget id;
*	bg_pix:		background color to be used (set as background for the
*				display area).
* Returns:
*	nothing.
*****/
void
XmHTMLTkaRecomputeColors(XmHTMLWidget html, Pixel bg_pixel)
{
	/*
	* We can only compute the colors when we have a GC. If we don't
	* have a GC, the widget is not yet realized. Use managers defaults
	* then.
	*/
	if(html->html.gc != NULL)
	{
		Pixel top = None, bottom = None, highlight = None;
		Arg args[3];

		XtVaSetValues(HTML_ATTR(work_area),
			XmNbackground, bg_pixel,
			NULL);

		XmGetColors(XtScreen((Widget)html), html->core.colormap,
			html->html.body_bg, NULL, &top, &bottom, &highlight);
		XtSetArg(args[0], XmNtopShadowColor, top);
		XtSetArg(args[1], XmNbottomShadowColor, bottom);
		XtSetArg(args[2], XmNhighlightColor, highlight);
		XtSetValues((Widget)html, args, 3);
	}
}

/*****
* Name: 		XmHTMLTkaRecomputeHighlightColor
* Return Type: 	void
* Description: 	computes the select color based upon the given color.
* In:
*	html:		XmHTMLWidget id;
* Returns:
*	nothing.
*****/
void
XmHTMLTkaRecomputeHighlightColor(XmHTMLWidget html, Pixel bg_pixel)
{
	/*
	* We can only compute the colors when we have a GC. If we don't
	* have a GC, the widget is not yet realized. Use managers defaults
	* then.
	*/
	if(html->html.gc != NULL)
	{
		Pixel highlight = None;
		Arg args[1];

		XmGetColors(XtScreen((Widget)html), html->core.colormap,
			bg_pixel, NULL, NULL, NULL, &highlight);
		XtSetArg(args[0], XmNhighlightColor, highlight);
		XtSetValues((Widget)html, args, 1);
	}
}

/*****
* Name:			XmHTMLTkaRecomputeShadowColors
* Return Type: 	void
* Description: 	recomputes the top and bottom shadow colors based on the
*				given *foreground* color
* In:
*	html:		XmHTMLWidget id;
*	base:		base color to base computation on.
* Returns:
*	Nothing, but the GC's from Manager are updated to reflect the change.
*****/
void
XmHTMLTkaRecomputeShadowColors(XmHTMLWidget html, Pixel base)
{
	/*
	* We can only compute the colors when we have a GC. If we don't
	* have a GC, the widget is not yet realized. Use managers defaults
	* then.
	*/
	if(html->html.gc != NULL)
	{
		Pixel top = None, bottom = None;
		Arg args[2];

		XmGetColors(XtScreen((Widget)html), html->core.colormap,
			base, NULL, &top, &bottom, NULL);
		XtSetArg(args[0], XmNtopShadowColor, top);
		XtSetArg(args[1], XmNbottomShadowColor, bottom);
		XtSetValues((Widget)html, args, 2);
	}
}
