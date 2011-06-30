/*****
* BalloonP.h : XmBalloon private header file
*
* This file Version	$Revision$
*
* Creation date:		Sun Nov  2 19:18:42 GMT+0100 1997
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
* Revision 1.1  1998/04/04 06:27:17  newt
* Initial Revision
*
*****/ 

#ifndef _BalloonP_h_
#define _BalloonP_h_

#include <X11/ShellP.h>		/* we're subclassing from overrideShell */

/* Required includes */
#include <XmHTML/Balloon.h>

_XFUNCPROTOBEGIN

/*****
* Class Pointer
*****/
typedef struct
{
	XtPointer		extensions;		/* Pointer to extension record */
}XmBalloonClassPart;

typedef struct _XmBalloonClassRec
{
	CoreClassPart			core_class;
	CompositeClassPart		composite_class;
	ShellClassPart			shell_class;
	OverrideShellClassPart	override_shell_class;
	XmBalloonClassPart		balloon_class;
}XmBalloonClassRec;

/*****
* Supporting structures
*****/
typedef struct _transform
{
	double  mx, bx;
	double  my, by;
}Transform;

typedef struct _TPoint
{
	double  x, y;
}TPoint;

/*****
* XmBalloon instance definition
*****/
typedef struct _XmBalloonPart
{
	/* public widget data */
	String		label;				/* original label as set by user	*/
	String		source;				/* privatly owner copy				*/
	int			source_len;			/* size of label					*/
	int			popup_delay;		/* delay before popping up			*/
	int			popdown_delay;		/* delay before popping down		*/
	Dimension	margin_width;		/* horizontal margin spacing		*/
	Dimension	margin_height;		/* vertical margin spacing			*/
	int			left_offset;		/* left cursor offset				*/
	int			top_offset;			/* top cursor offset				*/
	float		border_size;		/* thickness of the border			*/
	Pixel		foreground;			/* foreground pixel to be used		*/
#if XtSpecificationRelease < 5
	XFontStruct *font;				/* X11R4 font to be used			*/
#else
	XFontSet fontset;				/* X11R5 or above uses fontSets		*/
#endif
	unsigned char corner_style;		/* how corners should be drawn		*/
	unsigned char balloon_style;	/* balloon style					*/
	Boolean		transparent;		/* make balloon fully transparent	*/
	int			backing_store;		/*
									 * select appropriate amount of
									 * backing store
									 */

	/* private widget data */
	Position	pop_x;				/* relative x-popup position		*/
	Position	pop_y;				/* relative y-popup position		*/
	Dimension	font_width;			/* width of common (1 TeX em)		*/
	Dimension	font_height;		/* font height (row spacing)		*/
	Dimension	baseline;			/* baseline offset					*/
	GC			gc;					/* text rendering gc 				*/
	GC			top_gc;				/* top shadow gc					*/
	GC			bottom_gc;			/* bottom shadow gc					*/
	GC			shape_gc;			/* shaped window gc					*/
	XtIntervalId popup_id;			/* popup timeout id					*/
	XtIntervalId popdown_id;		/* popdown timout id				*/
	XtAppContext context;			/* application context for timeouts	*/
	Boolean	popped;					/* True when we are being displayed	*/

	/* Shaped Window data */
	Boolean		shape_window;		/* use shaped window extension?		*/
	Pixmap		shape_mask;			/* window shape						*/
	int			shape_width;		/* last shaped window width 		*/
	int			shape_height;		/* last shaped window height 		*/

	Transform	t;
	Transform	maskt;
}XmBalloonPart;

typedef struct _XmBalloonRec
{
	CorePart			core;
	CompositePart 		composite;
	ShellPart			shell;
	OverrideShellPart	override;
	XmBalloonPart		balloon;
}XmBalloonRec;

externalref XmBalloonClassRec xmBalloonClassRec;

/* Never define _LIBRARY yourself */
#ifdef VERSION
# ifndef _LIBRARY
#  define _LIBRARY
# endif
#endif

_XFUNCPROTOEND

/* Don't add anything after this endif! */
#endif /* _BalloonP_h_ */
