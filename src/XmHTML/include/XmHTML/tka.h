/*****
* tka.h : XmHTML Toolkit Abstraction Public Interface
*
* This file Version	$Revision$
*
* Creation date:		Mon Sep 28 08:49:25 CEST 1998
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				XmHTML Developers Account
*
* Copyright (C) 1994-1998 by Ripley Software Development 
* All Rights Reserved
*
* This file is part of no particular project.
*
* This program is free software; you can redistribute it and/or
* modify it under the terms of the GNU General Public
* License as published by the Free Software Foundation; either
* version 2 of the License, or (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* General Public License for more details.
*
* You should have received a copy of the GNU General Public
* License along with this program; if not, write to the Free
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
*
*****/ 

#ifndef _tka_h_
#define _tka_h_

/*****
* Toolkit independent rendering functions. This enables us to use the
* same engine for rendering to a display, text or postscript.
* 
* This abstraction makes it a *lot* easier when porting XmHTML to other
* toolkits, provided the display functions know how to deal/convert the
* X-specific types. See also toolkit.h for other toolkit-dependent
* definitions.
*****/
#define GC_FILL_SOLID				0
#define GC_FILL_TILED				1
#define GC_FILL_STIPPLED			2
#define GC_FILL_OPAQUE_STIPPLED		3

#define GC_CAP_NOT_LAST				0
#define GC_CAP_BUTT					1
#define GC_CAP_ROUND				2
#define GC_CAP_PROJECTING			3

#define GC_LINE_SOLID				0
#define GC_LINE_ON_OFF_DASH			1
#define GC_LINE_DOUBLE_DASH			2

#define GC_JOIN_MITER				0
#define GC_JOIN_ROUND				1
#define GC_JOIN_BEVEL				2

#define GC_GXcopy					0

#define GC_COORDMODE_ORIGIN			0
#define GC_COORDMODE_PREVIOUS		1

typedef struct _ToolkitAbstraction{
	Display *dpy;			/* display being used		*/
	Drawable win;			/* render area				*/
	Drawable defaultRoot;	/* fallback window			*/

	/*****
	* Screen definitions
	*****/
	int width;				/* width in pixels			*/
	int height;				/* height in pixels			*/
	int widthMM;			/* width in millimeters		*/
	int heightMM;			/* height in millimeters	*/

	/**********
	* Xlib function wrappers
	**********/

	/*****
	* GC properties
	*****/

	int fill_style[4];
	int cap_style[4];
	int line_style[3];
	int join_style[3];
	int gc_func[2];
	int coord_mode[2];

	/*****
	* GC functions
	*****/
	GC	(*CreateGC)( 
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Drawable			/* d */,
		unsigned long		/* valuemask */,
		XGCValues*			/* values */
#endif
	);

	int (*FreeGC)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		GC					/* gc */
#endif
	);

	int (*CopyGC)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		GC					/* src */,
		unsigned long		/* valuemask */,
		GC					/* dest */
#endif
	);

	int (*SetFunction)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		GC					/* gc */,
		int					/* function */
#endif
	);

	int (*SetClipMask)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		GC					/* gc */,
		Pixmap				/* pixmap */
#endif
	);

	int (*SetClipOrigin)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		GC					/* gc */,
		int					/* clip_x_origin */,
		int					/* clip_y_origin */
#endif
	);

	int (*SetTile)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		GC					/* gc */,
		Pixmap				/* tile */
#endif
	);

	int (*SetTSOrigin)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		GC					/* gc */,
		int					/* ts_x_origin */,
		int					/* ts_y_origin */
#endif
	);

	int (*SetFillStyle)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		GC					/* gc */,
		int					/* fill_style */
#endif
	);

	int (*SetFont)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		GC					/* gc */,
		XmHTMLfont*			/* font */
#endif
	);

	int (*SetForeground)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		GC					/* gc */,
		unsigned long		/* foreground */
#endif
	);

	int (*SetBackground)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		GC					/* gc */,
		unsigned long		/* background */
#endif
	);

	int (*SetLineAttributes)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		GC					/* gc */,
		unsigned int		/* line_width */,
		int					/* line_style */,
		int					/* cap_style */,
		int					/* join_style */
#endif
	);

	/*****
	* Font functions
	*****/

	XFontStruct* (*LoadQueryFont)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		_Xconst char*		/* name */
#endif
	);

	int (*FreeFont)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		XFontStruct*		/* font_struct */
#endif
	);

	int (*GetFontProperty)(
#ifdef NeedFunctionPrototypes
		XFontStruct*		/* font_struct */,
		Atom				/* atom */,
		unsigned long*		/* value_return */
#endif
	);

	/*****
	* Cursor & pointer functions
	*****/
	int (*UngrabPointer)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Time				/* time */
#endif
	);

	int (*DefineCursor)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Window				/* w */,
		Cursor				/* cursor */
#endif
	);

	int (*UndefineCursor)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Window				/* w */
#endif
	);

	int (*FreeCursor)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Cursor				/* cursor */
#endif
	);

	/*****
	* Color functions
	*****/

	int (*ParseColor)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Colormap			/* colormap */,
		_Xconst char*		/* spec */,
		XColor*				/* exact_def_return */
#endif
	);

	int (*AllocColor)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Colormap			/* colormap */,
		XColor*				/* screen_in_out */
#endif
	);

	int (*QueryColor)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Colormap			/* colormap */,
		XColor*				/* def_in_out */
#endif
	);

	int (*QueryColors)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Colormap			/* colormap */,
		XColor*				/* defs_in_out */,
		int					/* ncolors */
#endif
	);

	int (*FreeColors)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Colormap			/* colormap */,
		unsigned long*		/* pixels */,
		int					/* npixels */,
		unsigned long		/* planes */
#endif
	);

	/*****
	* Pixmap functions
	*****/

	Pixmap (*CreatePixmap)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Drawable			/* d */,
		unsigned int		/* width */,
		unsigned int		/* height */,
		unsigned int		/* depth */
#endif
	);

	Pixmap (*CreatePixmapFromBitmapData)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Drawable			/* d */,
		char*				/* data */,
		unsigned int		/* width */,
		unsigned int		/* height */,
		unsigned long		/* fg */,
		unsigned long		/* bg */,
		unsigned int		/* depth */
#endif
	);

	int	(*FreePixmap)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Pixmap				/* pixmap */
#endif
	);

	/*****
	* XImage functions
	*****/

	XImage *(*CreateImage)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Visual*				/* visual */,
		unsigned int		/* depth */,
		int					/* format */,
		int					/* offset */,
		char*				/* data */,
		unsigned int		/* width */,
		unsigned int		/* height */,
		int					/* bitmap_pad */,
		int					/* bytes_per_line */
#endif
	);

	void (*DestroyImage)(
#ifdef NeedFunctionPrototypes
		XImage *image
#endif
	);

	int  (*PutImage)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Drawable			/* d */,
		GC					/* gc */,
		XImage*				/* image */,
		int					/* src_x */,
		int					/* src_y */,
		int					/* dest_x */,
		int					/* dest_y */,
		unsigned int		/* width */,
		unsigned int		/* height */
#endif
	);

	void (*DrawImage)(
#ifdef NeedFunctionPrototypes
		XmHTMLWidget					/* Widget */,
		XmHTMLImage*					/* image */,
		GC								/* gc */,
		int								/* src_x */,
		int								/* src_y */,
		unsigned int					/* width */,
		unsigned int					/* height */,
		int								/* dest_x */,
		int								/* dest_y */
#endif
	);

	void (*DrawAnchorData)(
#ifdef NeedFunctionPrototypes
		Display*						/* display */,
		Window							/* win */,
		GC								/* gc */,
		int								/* x */,
		int								/* y */,
		XmHTMLObjectTableElement		/* anchor */
#endif
	);
		

	XImage* (*GetImage)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Drawable			/* d */,
		int					/* x */,
		int					/* y */,
		unsigned int		/* width */,
		unsigned int		/* height */,
		unsigned long		/* plane_mask */,
		int					/* format */
#endif
	);

	unsigned long (*GetPixel)(
#ifdef NeedFunctionPrototypes
		XImage *ximage		/* image */,
		_XtDimension x		/* x coordinate */,
		_XtDimension y		/* y coordinate */
#endif
	);
		
	/*****
	* string/text functions
	*****/

	int (*TextWidth)(
#ifdef NeedFunctionPrototypes
		XmHTMLfont*		/* font_struct */,
		_Xconst char*		/* string */,
		int					/* count */
#endif
	);

	int (*TextExtents)(
#ifdef NeedFunctionPrototypes
		XFontStruct*		/* font_struct */,
		_Xconst char*		/* string */,
		int					/* nchars */,
		int*				/* direction_return */,
		int*				/* font_ascent_return */,
		int*				/* font_descent_return */,
		XCharStruct*		/* overall_return */
#endif
	);

	/*****
	* Render functions
	*****/

	int  (*DrawString)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Drawable			/* d */,
		struct _XmHTMLFont*	/* font */,
		GC					/* gc */,
		int					/* x */,
		int					/* y */,
		_Xconst char*		/* string */,
		int					/* length */
#endif
	);

	/*****
	* Render functions
	*****/

	int (*DrawLine)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Drawable			/* d */,
		GC					/* gc */,
		int					/* x1 */,
		int					/* x2 */,
		int					/* y1 */,
		int					/* y2 */
#endif
	);

	int (*DrawLines)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Drawable			/* d */,
		GC					/* gc */,
		XPoint*				/* points */,
		int					/* npoints */,
		int					/* mode */
#endif
	);

	int (*DrawRectangle)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Drawable			/* d */,
		GC					/* gc */,
		int					/* x */,
		int					/* y */,
		unsigned int		/* width */,
		unsigned int		/* height */
#endif
	);

	int (*FillRectangle)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Drawable			/* d */,
		GC					/* gc */,
		int					/* x */,
		int					/* y */,
		unsigned int		/* width */,
		unsigned int		/* height */
#endif
	);

	int (*DrawArc)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Drawable			/* d */,
		GC					/* gc */,
		int					/* x */,
		int					/* y */,
		unsigned int		/* width */,
		unsigned int		/* height */,
		int					/* angle1 */,
		int					/* angle2 */
#endif
	);

	int (*FillArc)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Drawable			/* d */,
		GC					/* gc */,
		int					/* x */,
		int					/* y */,
		unsigned int		/* width */,
		unsigned int		/* height */,
		int					/* angle1 */,
		int					/* angle2 */
#endif
	);

	/*****
	* misc. functions
	*****/

	int (*CopyArea)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Drawable			/* src */,
		Drawable			/* dest */,
		GC					/* gc */,
		int					/* src_x */,
		int					/* src_y */,
		unsigned int		/* width */,
		unsigned int		/* height */,
		int					/* dest_x */,
		int					/* dest_y */
#endif
	);

	int (*ClearArea)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Window				/* w */,
		int					/* x */,
		int					/* y */,
		unsigned int		/* width */,
		unsigned int		/* height */,
		Bool				/* exposures */
#endif
	);

	int (*Sync)(
#ifdef NeedFunctionPrototypes
		Display*			/* display */,
		Bool				/* discard */
#endif
	);

	/**********
	* X Toolkit Intrinsics wrappers
	**********/

	Boolean	(*IsRealized)(
#ifdef NeedFunctionPrototypes
		Widget				/* widget */
#endif
	);

	Boolean (*IsManaged)(
#ifdef NeedFunctionPrototypes
		Widget				/* rectobj */
#endif
	);

	void	(*ManageChild)(
#ifdef NeedFunctionPrototypes
		Widget				/* child */
#endif
	);

	void (*UnmanageChild)(
#ifdef NeedFunctionPrototypes
		Widget				/* child */
#endif
	);

	void	(*MoveWidget)(
#ifdef NeedFunctionPrototypes
		Widget				/* widget */,
		_XtPosition			/* x */,
		_XtPosition			/* y */
#endif
	);

	void	(*ResizeWidget)(
#ifdef NeedFunctionPrototypes
		Widget				/* widget */,
		_XtDimension		/* width */,
		_XtDimension		/* height */,
		_XtDimension		/* border_width */
#endif
	);

	void	(*ConfigureWidget)(
#ifdef NeedFunctionPrototypes
		Widget				/* widget */,
		_XtPosition			/* x */,
		_XtPosition			/* y */,
		_XtDimension		/* width */,
		_XtDimension		/* height */,
		_XtDimension		/* border_width */
#endif
	);

	void	(*DestroyWidget)(
#ifdef NeedFunctionPrototypes
		 Widget				/* widget */
#endif
	);

	void	(*SetMappedWhenManaged)(
#ifdef NeedFunctionPrototypes
		Widget				/* widget */,
		_XtBoolean			/* mapped_when_managed */
#endif
	);

	void	(*RemoveTimeOut)(
#ifdef NeedFunctionPrototypes
		XtIntervalId		/* timer */
#endif
	);

	XtIntervalId	(*AddTimeOut)(
#ifdef NeedFunctionPrototypes
		XtAppContext		/* app_context */,
		unsigned long		/* interval */,
		XtTimerCallbackProc	/* proc */,
		XtPointer			/* closure */
#endif
	);

	/**********
	* Motif Wrappers
	**********/

	void (*DrawShadows)(
#ifdef NeedFunctionPrototypes
		Display *display,
		Drawable d,
		GC top_gc,
		GC bottom_gc,
#if NeedWidePrototypes
		int x,
		int y,
		int width,
		int height,
		int shad_thick,
#else
		Position x,
		Position y,
		Dimension width,
		Dimension height,
		Dimension shad_thick,
#endif	/* NeedWidePrototypes */
		unsigned int shad_type
#endif
	);

	/**********
	* Implementation Specific data
	**********/
	void *data;

	void (*FreeData)(
#ifdef NeedFunctionPrototypes
		void*
#endif
	);

}ToolkitAbstraction;

/* Create a new toolkit abstraction */
extern ToolkitAbstraction *XmHTMLTkaCreate(void);

/* destroy a toolkit abstraction */
extern void XmHTMLTkaDestroy(ToolkitAbstraction *tka);

/* Copy a toolkit abstraction */
extern ToolkitAbstraction *XmHTMLTkaCopy(ToolkitAbstraction *tka);

/* Supply a new toolkit abstraction to a XmHTML Widget */
#ifdef NOTYET
extern Boolean XmHTMLTkaSet(Widget w, ToolkitAbstraction *tka);
#endif

/* Set the render area for a tka to use */
extern void XmHTMLTkaSetDrawable(ToolkitAbstraction *tka, Drawable drawable);

/* Set the display area for a tka to use */
extern void XmHTMLTkaSetDisplay(ToolkitAbstraction *tka, Widget w);

/* Recompute new top, bottom & highlight colors */
extern void XmHTMLTkaRecomputeColors(XmHTMLWidget html, Pixel bg_pixel);

/* Recompute highlight color */
extern void XmHTMLTkaRecomputeHighlightColor(XmHTMLWidget html,
	Pixel bg_pixel);

/* Recompute top & shadow colors */
extern void XmHTMLTkaRecomputeShadowColors(XmHTMLWidget html, Pixel base);

/* Don't add anything after this endif! */
#endif /* _tka_h_ */

