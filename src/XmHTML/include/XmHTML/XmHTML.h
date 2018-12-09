/*****
* XmHTML.h : XmHTML Widget public header file
*
* This file Version	$Revision$
*
* Creation date:		Tue Nov 19 23:18:37 GMT+0100 1996
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
* $Source$
*****/
/*****
* ChangeLog
* $Log$
* Revision 1.2  2012/03/01 17:56:31  ziad
* Cput
*
* Revision 1.1  2011/06/30 16:08:41  rwcox
* Cadd
*
* Revision 1.21  1998/04/27 06:56:31  newt
* Updated version number
*
* Revision 1.20  1998/04/04 06:27:53  newt
* XmHTML Beta 1.1.3
*
* Revision 1.19  1997/10/26 23:49:52  newt
* Moved internal symbol defines to XmHTMLP.h
*
* Revision 1.18  1997/10/23 00:24:44  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.17  1997/08/31 17:32:43  newt
* log edit
*
* Revision 1.16  1997/08/30 00:40:16  newt
* Changed proto's for XmHTMLImageReplace and XmHTMLImageUpdate. They now
* return an XmImageStatus instead of void.
*
* Revision 1.15  1997/08/01 12:54:55  newt
* Progressive image loading changes.
*
* Revision 1.14  1997/05/28 01:41:04  newt
* Added the XmHTMLImageGZFSupported, XmHTMLGIFtoGZF, XmHTMLGifReadOK and
* XmHTMLGifGetDataBlock convienience routines to convert GIF images to an
* alternate format. Added the XmHTMLAllocColor and XmHTMLFreeColor protos for
* easy color allocation. Modified the XmImageCreate routines to use a
* XmImageConfig structure.
*
* Revision 1.13  1997/04/29 14:22:35  newt
* Completely revised due to XmHTMLParserObject integration
*
* Revision 1.12  1997/04/03 05:31:30  newt
* XmHTMLFrameGetChild proto. XmIMAGE_SHARED_DATA option bit added.
*
* Revision 1.11  1997/03/28 07:06:14  newt
* XmNframeCallback, XmHTMLFrameCallbackStruct. XmHTMLParserCallback changes.
*
* Revision 1.10  1997/03/20 08:05:02  newt
* XmHTMLImageFreeImageInfo, XmNrepeatDelay
*
* Revision 1.9  1997/03/11 19:50:27  newt
* Changes in XmImageInfo; grouped and renamed convenience functions
*
* Revision 1.8  1997/03/04 18:45:55  newt
* XmNimagemapBoundingBoxForeground and XmCImagemapDrawBoundingBoxes
* resources added
*
* Revision 1.7  1997/03/04 00:56:30  newt
* Delayed Image Loading: added the delayed field to the XmImageInfo structure.
* Removed a number of obsolete defines
*
* Revision 1.6  1997/03/02 23:07:10  newt
* Image/Imagemap related changes. XmHTMLAnchorCallbackStruct changed
*
* Revision 1.5  1997/02/11 02:01:26  newt
* Added the XmNhandleShortTags resource
*
* Revision 1.4  1997/02/04 02:55:57  newt
* added the basefont element
*
* Revision 1.3  1997/01/09 06:55:53  newt
* expanded copyright marker
*
* Revision 1.2  1997/01/09 06:47:58  newt
* New resource: XmNparserCallback and corresponding error structure/defines
*
* Revision 1.1  1996/12/19 02:17:14  newt
* Initial Revision
*
*****/
/*****
* Public interfaces are listed at the end of this file
*****/

#ifndef _XmHTML_h_
#define _XmHTML_h_

#define XmHTMLVERSION	1
#define XmHTMLREVISION	1
#define XmHTMLUPDATE_LEVEL 7
#define XmHTMLVersion \
	(XmHTMLVERSION * 1000 + XmHTMLREVISION * 100 + XmHTMLUPDATE_LEVEL)

/* used by Imake to get Shared library version numbering */
#ifndef _LIBRARY

#define XmHTMLVERSION_STRING \
	"XmHTML Beta Version 1.1.7 (C)Ripley Software Development"

/* Required includes */
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <XmHTML/HTML.h>

#ifndef XmHTML_LIBEXPORT
#define XmHTML_LIBEXPORT extern
#endif

_XFUNCPROTOBEGIN

/* XmHTML type defines */
typedef struct _XmHTMLClassRec *XmHTMLWidgetClass;
typedef struct _XmHTMLRec *XmHTMLWidget;

/*****
* VMS works differently here: it defines externalref to globalref,
* so guard against incorrectly overriding the externaldef define.
*****/
#ifdef VMS
externalref WidgetClass xmHTMLWidgetClass;
#else
XmHTML_LIBEXPORT WidgetClass xmHTMLWidgetClass;
#endif

/* XmHTML Widget subclassing macro */
#ifndef XmIsHTML
#define XmIsHTML(w)	XtIsSubclass(w, xmHTMLWidgetClass)
#endif /* XmIsHTML */

/********    Public Function Declarations    ********/

/*****
* Convenience/public functions
* There are six categories:
* - image related
* - anchor related
* - text related
* - tag analyzers
* - object related
* - functions that don't fit in any of the above three
*****/

/*****
* Image related convenience functions
*****/
XmHTML_LIBEXPORT XmImageInfo *XmHTMLImageDefaultProc(Widget w, String file,
	unsigned char *buf, int size);

/* Return image type */
XmHTML_LIBEXPORT unsigned char XmHTMLImageGetType(String file,
	unsigned char *buf, int size);

/* returns True if XmHTMLImageDefaultProc supports JPEG images */
XmHTML_LIBEXPORT Boolean XmHTMLImageJPEGSupported(void);

/* returns True if XmHTMLImageDefaultProc supports PNG images */
XmHTML_LIBEXPORT Boolean XmHTMLImagePNGSupported(void);

/* returns True if XmHTMLImageDefaultProc supports GZF images */
XmHTML_LIBEXPORT Boolean XmHTMLImageGZFSupported(void);

/* Replace image with new_image */
XmHTML_LIBEXPORT XmImageStatus XmHTMLImageReplace(Widget w, XmImageInfo *image,
		XmImageInfo *new_image);

/* update image */
XmHTML_LIBEXPORT XmImageStatus XmHTMLImageUpdate(Widget w, XmImageInfo *image);

/* release all memory occupied by the images */
XmHTML_LIBEXPORT void XmHTMLImageFreeAllImages(Widget w);

/* add an imagemap to a HTML widget. */
XmHTML_LIBEXPORT void XmHTMLImageAddImageMap(Widget w, String image_map);

/* free an XmImageInfo structure */
XmHTML_LIBEXPORT void	XmHTMLImageFreeImageInfo(Widget w, XmImageInfo *info);

/* return the total size of a given XmImageInfo structure */
XmHTML_LIBEXPORT int XmHTMLImageGetImageInfoSize(XmImageInfo *info);

/* suspend progressive image loading */
XmHTML_LIBEXPORT void XmHTMLImageProgressiveSuspend(Widget w);

/* reactivate progressive image loading */
XmHTML_LIBEXPORT void XmHTMLImageProgressiveContinue(Widget w);

/* terminate progressive image loading */
XmHTML_LIBEXPORT void XmHTMLImageProgressiveKill(Widget w);

/*****
* Special image functions
*****/
/*****
* Create and return a XmImage for use other than with XmHTML.
* When width and height are non-zero, the image read is scaled to the specified
* dimensions.
*****/
XmHTML_LIBEXPORT XmImage *XmImageCreate(Widget w, String file,
	Dimension width, Dimension height, XmImageConfig *config);

/*****
* Create an XmImage from the given XmImageInfo. When the image type is
* IMAGE_UNKNOWN, but the url field represents a local file, this routine
* calls XmImageCreate with the url field as the file argument.
* Only honors the ImageFrameSelect, ImageCreateGC and ImageBackground
* XmImageConfig flag and appropriate fields of that structure.
*****/
XmHTML_LIBEXPORT XmImage *XmImageCreateFromInfo(Widget w, XmImageInfo *info,
	Dimension width, Dimension height, XmImageConfig *config);

/* destroy a XmImage */
XmHTML_LIBEXPORT void XmImageDestroy(XmImage *image);

/*****
* Tiles "dest" with the given XmImage. Please note that "src" *must* have been
* created with the ImageCreateGC flag, otherwise this function does nothing and
* returns -1. Returns 0 upon success.
* Internally, this routine is more or less a combination of XSetTile,
* XSetTSOrigin and XFillRectangle in one.
*
* (UNTESTED)
*****/
XmHTML_LIBEXPORT int XmImageSetBackgroundImage(XmImage *src, Drawable dest,
	int src_x, int src_y, int width, int height, int dest_x, int dest_y);

/*****
* XCopyArea for an XmImage which also takes a possible clipmask into account.
* Please note that "image" *must* have been created with the ImageCreateGC
* flag, otherwise this function does nothing and returns -1.
* Returns 0 upon success.
*
* (UNTESTED)
*****/
XmHTML_LIBEXPORT int XmImageDrawImage(XmImage *image, Drawable dest, int src_x,
	int src_y, int dest_x, int dest_y);

/*****
* Write an image to file. Returns False upon failure.
* Saving an image in the GIF format is *not* possible due to Unisys's
* stupid LZW licensing policy. Exporting an image as PNG, JPEG or GZF is only
* possible if support for the required libraries has been compiled in.
*
* (UNIMPLEMENTED, always returns False)
*****/
XmHTML_LIBEXPORT Boolean XmImageExport(XmImageInfo *info, String file,
	Dimension width, Dimension height, unsigned char type);

/* convert a GIF image to a GZF image */
XmHTML_LIBEXPORT Boolean XmHTMLGIFtoGZF(String infile, unsigned char *buf,
	int size, String outfile);

/*****
* Anchor related convenience functions
* These routines can be used to jump to named anchors.
*****/
/* return the internal id of a named anchor given it's name or -1. */
XmHTML_LIBEXPORT int XmHTMLAnchorGetId(Widget w, String anchor);

/* scroll to a named anchor, given it's id */
XmHTML_LIBEXPORT void XmHTMLAnchorScrollToId(Widget w, int anchor_id);

/* scroll to a named anchor, given it's name */
XmHTML_LIBEXPORT void XmHTMLAnchorScrollToName(Widget w, String anchor);

/* return True if a named anchor is currently visible, given it's id */
XmHTML_LIBEXPORT Boolean XmHTMLAnchorVisibleByName(Widget w, String anchor);

/* return True if a named anchor is currently visible, given it's name */
XmHTML_LIBEXPORT Boolean XmHTMLAnchorVisibleById(Widget w, int anchor_id);

/*****
* Checks a HTML instance against a current href name and alters any
* matching anchors to visited and causes a refesh.
* If visited is True, the matching anchor is rendered as visited. If it's
* False, it will be rendered as not visited.
*****/
XmHTML_LIBEXPORT void XmHTMLAnchorReEval(Widget w, String href,
	Boolean visited);

/*****
* Text related convenience functions
*****/
/* This macro sets the given text into a HTML widget */
#define XmHTMLTextSet(WIDGET,TEXT) XtVaSetValues((WIDGET), \
		XmNvalue, (TEXT), NULL)

/* scroll to the requested line number */
XmHTML_LIBEXPORT void XmHTMLTextScrollToLine(Widget w, int line);

/* set text into a html widget */
XmHTML_LIBEXPORT void XmHTMLTextSetString(Widget w, String text);

/*****
* same as XmHTMLTextSetString with one fundamental difference: text doesn't
* have to be NULL terminated. The size of the input string is instead
* given by len. If text is NULL or len is 0, the current contents are
* cleared.
*****/
XmHTML_LIBEXPORT void XmHTMLTextSetStringWithLength(Widget w, String text,
	size_t len);

/* return a *pointer* to the original text */
XmHTML_LIBEXPORT String XmHTMLTextGetSource(Widget w);

/* return a copy of the current parser output */
XmHTML_LIBEXPORT String XmHTMLTextGetString(Widget w);

/*****
* Return a formatted copy of the current widget contents
* (UNIMPLEMENTED, always returns NULL)
*****/
XmHTML_LIBEXPORT String XmHTMLTextGetFormatted(Widget w,
	unsigned char papertype, XmHTMLPaperSize *paperdef, unsigned char type,
	unsigned long PSoptions);

XmHTML_LIBEXPORT String XmHTMLTextGetSelection(Widget w);

XmHTML_LIBEXPORT void XmHTMLTextClearSelection(Widget w, Time time);

XmHTML_LIBEXPORT Boolean XmHTMLTextSetSelection(Widget w,
	XmHTMLTextPosition first, XmHTMLTextPosition last, Time time);

XmHTML_LIBEXPORT void XmHTMLTextSetHighlight(Widget w, XmHTMLTextPosition first,
	XmHTMLTextPosition last, XmHighlightMode mode);

XmHTML_LIBEXPORT Boolean XmHTMLTextCopy(Widget w, Time time);

XmHTML_LIBEXPORT Boolean XmHTMLTextShowPosition(Widget w,
	XmHTMLTextPosition position);

/* text search functions */
XmHTML_LIBEXPORT XmHTMLTextFinder XmHTMLTextFinderCreate(Widget w);

XmHTML_LIBEXPORT void XmHTMLTextFinderDestroy(XmHTMLTextFinder finder);

XmHTML_LIBEXPORT Boolean XmHTMLTextFinderSetPattern(XmHTMLTextFinder finder,
	String to_find);

XmHTML_LIBEXPORT Boolean
	XmHTMLTextFinderSetPatternFlags(XmHTMLTextFinder finder, int flags,
		Boolean ignore_case, XmHTMLDirection direction);

/* return the id of the last known regex error */
XmHTML_LIBEXPORT int XmHTMLTextFinderGetError(XmHTMLTextFinder finder);

/* same as above but now returns a string which must be freed by caller */
XmHTML_LIBEXPORT String XmHTMLTextFinderGetErrorString(XmHTMLTextFinder finder);

XmHTML_LIBEXPORT XmHTMLRegexStatus XmHTMLTextFindString(Widget w,
	XmHTMLTextFinder finder);

XmHTML_LIBEXPORT void XmHTMLTextFinderReset(XmHTMLTextFinder finder);

XmHTML_LIBEXPORT Boolean XmHTMLTextFindToPosition(Widget w,
	XmHTMLTextFinder finder, XmHTMLTextPosition *start,
	XmHTMLTextPosition *end);

/*****
* Tag analyzers.
* These routines allow you to filter values out of the attributes
* attached to a HTML element.
*****/
XmHTML_LIBEXPORT int XmHTMLTagGetNumber(String attributes, String tag,
	int default_value);

XmHTML_LIBEXPORT int XmHTMLTagCheckNumber(String attributes, String tag,
	int default_value);

XmHTML_LIBEXPORT String XmHTMLTagGetValue(String attributes, String tag);

XmHTML_LIBEXPORT Boolean XmHTMLTagCheck(String attributes, String tag);

XmHTML_LIBEXPORT Boolean XmHTMLTagCheckValue(String attributes, String tag,
	String check);

/*****
* Object related convenience functions.
* These functions allow one to define custom HTML elements to XmHTML.
*****/

/* define a new element to the HTML parser */
XmHTML_LIBEXPORT XmHTMLElementId XmHTMLObjectDefine(String element,
	Boolean terminated, unsigned long attribute_flags, XtPointer user_data);

/* undefine a previously defined element */
XmHTML_LIBEXPORT XmHTMLObjectStatus
	XmHTMLObjectUndefine(XmHTMLElementId element_id);

/* reconfigure the given object (change position and/or size) */
XmHTML_LIBEXPORT XmHTMLObjectStatus XmHTMLObjectReconfigure(Widget w,
	XmHTMLObjectId object_id, int x, int y, Dimension width, Dimension height);

/* show/hide the given object */
XmHTML_LIBEXPORT XmHTMLObjectStatus XmHTMLObjectShow(Widget w,
	XmHTMLObjectId object_id, Boolean show);

/* raise the given object from the currently displayed document */
XmHTML_LIBEXPORT XmHTMLObjectStatus XmHTMLObjectRaise(Widget w,
	XmHTMLObjectId object_id);

/* lower the given object into the currently displayed document */
XmHTML_LIBEXPORT XmHTMLObjectStatus XmHTMLObjectLower(Widget w,
	XmHTMLObjectId object_id);

/* Create an external object corresponding to a previously defined element */
XmHTML_LIBEXPORT XmHTMLObjectId XmHTMLObjectCreate(XmHTMLElementId element_id);

/* Destroy an external object */
XmHTML_LIBEXPORT void XmHTMLObjectDestroy(XmHTMLObjectId object_id);

/* register an external object for use with XmHTML */
XmHTML_LIBEXPORT XmHTMLObjectStatus XmHTMLObjectRegister(Widget w,
	XmHTMLObjectId object_id);

/* unregister a previously registered object from XmHTML */
XmHTML_LIBEXPORT XmHTMLObjectStatus XmHTMLObjectUnregister(Widget w,
	XmHTMLObjectId object_id);

/* fetch the element data from the given object */
XmHTML_LIBEXPORT XmHTMLElementId XmHTMLObjectGetElement(Widget w,
	XmHTMLObjectId object_id);

/*****
* Miscelleneous convenience functions
*****/
/* return the library version number */
XmHTML_LIBEXPORT int XmHTMLGetVersion(void);

/* return version string */
XmHTML_LIBEXPORT char *XmHTMLGetVersionString(void);

/* return the URL type of the given href */
XmHTML_LIBEXPORT URLType XmHTMLGetURLType(String href);

/* return the value of the <TITLE></TITLE> element */
XmHTML_LIBEXPORT String XmHTMLGetTitle(Widget w);

/* return an info structure for the specified location */
XmHTML_LIBEXPORT XmHTMLInfoStructure *XmHTMLXYToInfo(Widget w, int x, int y);

/* return document structure (list of images & hyperlinks) */
XmHTML_LIBEXPORT XmHTMLDocumentInfo *XmHTMLGetDocumentInfo(Widget w);

/* free a document structure */
XmHTML_LIBEXPORT void XmHTMLFreeDocumentInfo(XmHTMLDocumentInfo *doc_info);

/****
* Return the contents of the document head. Returns True when a <head></head>
* section is present in the current document, False if not. When mask_bits
* only contains HeadClear, the given attribute structure is wiped clean and
* this function will return False immediatly.
* The only exception concerns the <!DOCTYPE> tag and the HeadDocType mask bit:
* if this bit is set, the value of this tag is returned whether or not a
* head is present.
****/
XmHTML_LIBEXPORT Boolean XmHTMLGetHeadAttributes(Widget w,
	XmHTMLHeadAttributes *head, unsigned char mask_bits);

/* return the widget id of a framechild given its name */
XmHTML_LIBEXPORT Widget XmHTMLFrameGetChild(Widget w, String name);

/* Create a HTML widget if parent is not null and no subclass of XmGadget */
XmHTML_LIBEXPORT Widget XmCreateHTML(Widget parent, String name,
	ArgList arglist,
	Cardinal argcount);

/* force a recomputation of screen layout and trigger a redisplay */
XmHTML_LIBEXPORT void XmHTMLRedisplay(Widget w);
XmHTML_LIBEXPORT void XmHTMLRefresh(Widget w);

/* return info about the font cache for display of the given widget */
XmHTML_LIBEXPORT XmHTMLFontCacheInfo *XmHTMLGetFontCacheInfo(Widget w);

/* free the given font cache info */
XmHTML_LIBEXPORT void XmHTMLFreeFontCacheInfo(XmHTMLFontCacheInfo *info);

/*
* Allocate given color (symbolic name of rgb triplet) using the widget's
* colormap. Works with the XmNmaxImageColors resource.
*/
XmHTML_LIBEXPORT Pixel XmHTMLAllocColor(Widget w, String color,
	Pixel def_pixel);

/* free a color allocated with XmHTMLAllocColor */
XmHTML_LIBEXPORT void XmHTMLFreeColor(Widget w, Pixel pixel);

_XFUNCPROTOEND

#endif /* _LIBRARY */

/* Don't add anything after this endif! */
#endif /* _XmHTML_h_ */
