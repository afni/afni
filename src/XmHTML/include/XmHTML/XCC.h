/*****
* XCC.h : XCC.c public header file: XColorContext routines
*
* This file Version	$Revision$
*
* Creation date:		Mon Mar  3 00:28:18 GMT+0100 1997
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				John L. Cwikla
*
* Copyright 1994,1995 John L. Cwikla
* Copyright (C) 1997 by Ripley Software Development 
* All Rights Reserved
* 
* This file is part of the XmHTML Widget Library.
*
* See below for John L. Cwikla's original copyright notice and distribution
* Policy.
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
* $Id: XCC.h,v 1.6 1995/08/04 17:46:53 cwikla
*
* Copyright 1994,1995 John L. Cwikla
*
* Permission to use, copy, modify, distribute, and sell this software
* and its documentation for any purpose is hereby granted without fee,
* provided that the above copyright notice appears in all copies and that
* both that copyright notice and this permission notice appear in
* supporting documentation, and that the name of John L. Cwikla or
* Wolfram Research, Inc not be used in advertising or publicity
* pertaining to distribution of the software without specific, written
* prior permission.  John L. Cwikla and Wolfram Research, Inc make no
* representations about the suitability of this software for any
* purpose.  It is provided "as is" without express or implied warranty.
*
* John L. Cwikla and Wolfram Research, Inc disclaim all warranties with
* regard to this software, including all implied warranties of
* merchantability and fitness, in no event shall John L. Cwikla or
* Wolfram Research, Inc be liable for any special, indirect or
* consequential damages or any damages whatsoever resulting from loss of
* use, data or profits, whether in an action of contract, negligence or
* other tortious action, arising out of or in connection with the use or
* performance of this software.
*
* Author:
*  John L. Cwikla
*  X Programmer
*  Wolfram Research Inc.
*
*  cwikla@wri.com
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
* Revision 1.14  1997/10/23 00:24:42  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.13  1997/08/31 17:31:55  newt
* log edit
*
* Revision 1.12  1997/08/30 00:32:02  newt
* Fixed palette preparations & changed proto's for XCCGetPixels and
* XCCGetPixelsIncremental.
*
* Revision 1.11  1997/08/01 12:54:41  newt
* Proto for XCCGetPixelsIncremental added.
*
* Revision 1.10  1997/05/28 01:35:04  newt
* Updated XCCCreate proto.
*
* Revision 1.9  1997/04/03 05:29:02  newt
* Added XCCFreeColors proto
*
* Revision 1.8  1997/03/20 08:03:19  newt
* XCCGetPixels proto updated
*
* Revision 1.7  1997/03/02 23:44:25  newt
* Expanded copyright marker
*
*****/ 

#ifndef _xcc_h_
#define _xcc_h_

typedef struct _XColorContext *XCC;

_XFUNCPROTOBEGIN

extern XCC XCCCreate(Widget w, Visual *_visual, Colormap _colormap);

/* copy an XCC entirely */
extern XCC XCCCopy(XCC src);

extern XCC XCCMonoCreate(Display *_dpy, Visual *_visual, Colormap _colormap);

extern void XCCFree(XCC _xcc);

extern unsigned long XCCGetPixel(XCC _xcc, unsigned short _red, 
	unsigned short _green, unsigned short _blue, Boolean *failed);

extern void XCCGetPixels(XCC _xcc, unsigned short *reds,
	unsigned short *greens, unsigned short *blues, int ncolors,
	unsigned long *colors, int *nallocated);

extern void XCCGetPixelsIncremental(XCC _xcc, unsigned short *reds,
	unsigned short *greens, unsigned short *blues, int ncolors,
	Boolean *used, unsigned long *colors, int *nallocated);

extern int XCCGetNumColors(XCC _xcc);

extern Colormap XCCGetColormap(XCC _xcc);
extern Visual *XCCGetVisual(XCC _xcc);
extern XVisualInfo *XCCGetVisualInfo(XCC _xcc);
extern int XCCGetDepth(XCC _xcc);
extern int XCCGetClass(XCC _xcc);
extern Display *XCCGetDisplay(XCC _xcc);
extern int XCCQueryColor(XCC _xcc, XColor *_color);
extern int XCCQueryColors(XCC _xcc, XColor *_colors, int _numColors);

/* get colorcomponents of the given color */
extern int XCCGetColor(XCC _xcc, unsigned long pixel_val,
	unsigned short *red, unsigned short *green, unsigned short *blue);

/*****
* Returns visual of either the widget (or any parent that is a subclass of
* shell) or the default visual
*****/
extern Visual *XCCGetParentVisual(Widget w);

/*****
* Add a palette to the given XCC. All colors will then be mapped onto this
* palette.
*****/
extern int XCCAddPalette(XCC _xcc, XColor *palette, int num_palette);

/*****
* Initialize dithering: allocate & initialize a precomputed error correction
* matrices.
*****/
extern void XCCInitDither(XCC _xcc);

/* and free it again */
extern void XCCFreeDither(XCC _xcc);

/*****
* Pick a color using an installed palette. If a direct match is not found,
* a least squares algorithm is used to map the color components.
* The failed arg will be True if the mapping failed. In all other cases a
* pixel value is returned and the _red, _green and _blue args will contain
* the difference between the original and used component (which can be
* negative, hence these args are signed). If they are all equal to 0 a
* perfect match was found (which I suspect will be seldomly the case).
*
* The intended use of this routine is dithering. Either using ordered or
* Floyd-Steinberg.
*****/
unsigned long XCCGetPixelFromPalette(XCC _xcc, unsigned short *_red,
	unsigned short *_green, unsigned short *_blue, Boolean *failed);

/*****
* Same as above, but this time the *index* into the palette at which the
* requested color can be found is returned. Used by the dither algorithms.
*****/
extern unsigned char XCCGetIndexFromPalette(XCC _xcc, int *_red, int *_green,
	int *_blue, Boolean *failed);

_XFUNCPROTOEND

/* Don't add anything after this endif! */
#endif /* _xcc_h_ */
