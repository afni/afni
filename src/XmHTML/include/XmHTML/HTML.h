/*****
* HTML.h : XmHTML Widget public header file.
*			Resource defines, enumerations and structures.
*
* This file Version	$Revision$
*
* Creation date:		Tue Apr 15 23:39:26 GMT+0100 1997
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
* Revision 1.8  1998/04/27 06:54:30  newt
* Changed XmImageProc proto
*
* Revision 1.7  1998/04/04 06:27:46  newt
* XmHTML Beta 1.1.3
*
* Revision 1.6  1997/10/23 00:24:27  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.5  1997/08/31 17:30:39  newt
* Removed HT_TEXTFLOW
*
* Revision 1.4  1997/08/30 00:22:46  newt
* Alpha channel resources: XmNalphaChannelProcessing and
* XmNimageRGBConversion. Updated comments and reorganized a bunch of things.
*
* Revision 1.3  1997/08/01 12:52:11  newt
* Progressive image loading changes
*
* Revision 1.2  1997/05/28 01:29:28  newt
* XmImage changes: added the XmImageConfig structure and configuration flags.
* Added support for the XmNdecodeGIFProc resource.
*
* Revision 1.1  1997/04/29 14:19:18  newt
* Initial Revision
*
*****/ 

#ifndef _HTML_h_
#define _HTML_h_

/* include our new resources */
#include <XmHTML/HTMLStrings.h>

/******************************************************************************
* Enumerations and other constants
******************************************************************************/

/*****
* HTML Elements internal id's
* This list is alphabetically sorted to speed up the searching process.
* DO NOT MODIFY
*****/
typedef enum{
HT_DOCTYPE, HT_A, HT_ADDRESS, HT_APPLET, HT_AREA, HT_B, HT_BASE, HT_BASEFONT,
HT_BIG, HT_BLOCKQUOTE, HT_BODY, HT_BR, HT_CAPTION, HT_CENTER, HT_CITE, HT_CODE,
HT_DD, HT_DFN, HT_DIR, HT_DIV, HT_DL, HT_DT, HT_EM, HT_FONT, HT_FORM, HT_FRAME,
HT_FRAMESET, HT_H1, HT_H2, HT_H3, HT_H4, HT_H5, HT_H6, HT_HEAD, HT_HR, HT_HTML,
HT_I, HT_IMG, HT_INPUT, HT_ISINDEX, HT_KBD, HT_LI, HT_LINK, HT_MAP, HT_MENU,
HT_META, HT_NOFRAMES, HT_OL, HT_OPTION, HT_P, HT_PAGE, HT_PARAM, HT_PRE,
HT_SAMP, HT_SCRIPT, HT_SELECT, HT_SMALL, HT_STRIKE, HT_STRONG, HT_STYLE, HT_SUB,
HT_SUP, HT_TAB, HT_TABLE, HT_TD, HT_TEXTAREA, HT_TH, HT_TITLE,
HT_TR, HT_TT, HT_U, HT_UL, HT_VAR, HT_ZTEXT
}htmlEnum;

/*****
* Corresponding HTML element name table. Indexing with the above enumeration
* will give the corresponding element name.
*****/
extern String *html_tokens;

/***** 
* XmHTML defines the following callback reasons. This might produce strange
* results once Motif decides to uses enum values above 16383.
* Send us a mail at ripley@xs4all.nl if you get problems that are due to
* these enumeration values.
*****/
enum{
	XmCR_HTML_ANCHORTRACK = 16384,		/* XmNanchorTrackCallback	*/
	XmCR_HTML_DOCUMENT,					/* XmNdocumentCallback		*/
	XmCR_HTML_FORM,						/* XmNformCallback			*/
	XmCR_HTML_FRAME,					/* XmNframeCallback			*/
	XmCR_HTML_FRAMECREATE,				/* XmNframeCallback			*/
	XmCR_HTML_FRAMEDESTROY,				/* XmNframeCallback			*/
	XmCR_HTML_IMAGEMAPACTIVATE,			/* XmNimagemapCallback		*/
	XmCR_HTML_IMAGEMAP,					/* XmNimagemapCallback		*/
	XmCR_HTML_LINK,						/* XmNlinkCallback			*/
	XmCR_HTML_MODIFYING_TEXT_VALUE,		/* XmNmodifyVerifyCallback	*/
	XmCR_HTML_MOTIONTRACK,				/* XmNmotionTrackCallback	*/
	XmCR_HTML_PARSER,					/* XmNparserCallback		*/
	XmCR_HTML_EVENT,					/* XmNeventCallback			*/
	XmCR_HTML_EVENTDESTROY,				/* XmNeventCallback			*/
	XmCR_HTML_OBJECT,					/* XmNobjectCallback		*/
	XmCR_HTML_OBJECTCREATE,				/* XmNobjectCallback		*/
	XmCR_HTML_OBJECTDESTROY				/* XmNobjectCallback		*/
};

/*****
* XmNeventCallback sub event types
*****/
enum{
	/* Document/Frame specific events */
	XmCR_HTML_LOAD = 0,				/* onLoad		*/
	XmCR_HTML_UNLOAD,				/* onUnLoad		*/

	/* HTML Form specific events */
	XmCR_HTML_SUBMIT,				/* onSubmit		*/
	XmCR_HTML_RESET,				/* onReset		*/
	XmCR_HTML_FOCUS,				/* onFocus		*/
	XmCR_HTML_BLUR,					/* onBlur		*/
	XmCR_HTML_SELECT,				/* onSelect		*/
	XmCR_HTML_CHANGE,				/* onChange		*/

	/* object events */
	XmCR_HTML_CLICK,				/* onClick		*/
	XmCR_HTML_DOUBLE_CLICK,			/* onDblClick	*/
	XmCR_HTML_MOUSEDOWN,			/* onMouseDown	*/
	XmCR_HTML_MOUSEUP,				/* onMouseUp	*/
	XmCR_HTML_MOUSEOVER,			/* onMouseOver	*/
	XmCR_HTML_MOUSEMOVE,			/* onMouseMove	*/
	XmCR_HTML_MOUSEOUT, 			/* onMouseOut	*/
	XmCR_HTML_KEYPRESS,				/* onKeyPress	*/
	XmCR_HTML_KEYDOWN,				/* onKeyDown	*/
	XmCR_HTML_KEYUP,				/* onKeyUp		*/
	XmCR_HTML_USEREVENT				/* must always be last */
};

/*****
* URL types XmHTML knows of.
* The hostnames, files and port numbers are only shown for demonstration
* purposes, XmHTML doesn't care whether they are present or not.
* The first 16 elements are alphabetically sorted to speed up URL
* translations.
*****/
typedef enum{
	ANCHOR_ABOUT = 0,			/* href="about:..."							*/
	ANCHOR_EXEC,				/* href="exec:foo_bar"						*/
	ANCHOR_FILE_REMOTE,			/* href="file://foo.bar/file.html"			*/
	ANCHOR_FTP,					/* href="ftp://foo.bar/file"				*/
	ANCHOR_GOPHER,				/* href="gopher://foo.bar:70"				*/
	ANCHOR_HELP,				/* href="help:..."							*/
	ANCHOR_HTTP,				/* href="http://foo.bar/file.html"			*/
	ANCHOR_SECURE_HTTP,			/* href="https://foo.bar/file.html"			*/
	ANCHOR_INFO,				/* href="info:.."							*/
	ANCHOR_MAILTO,				/* href="mailto:foo@bar"					*/
	ANCHOR_MAN,					/* href="man:..."							*/
	ANCHOR_NEWS,				/* href="news://foo.bar"					*/
	ANCHOR_PIPE,				/* href="pipe:foo_bar"						*/
	ANCHOR_TELNET,				/* href="telnet://foo.bar:23"				*/
	ANCHOR_WAIS,				/* href="wais://foo.bar"					*/
	ANCHOR_XEXEC,				/* href="xexec:foo_bar"						*/
	ANCHOR_UNKNOWN,				/* unknown href								*/
	ANCHOR_FILE_LOCAL,			/* href="file.html"							*/
	ANCHOR_FORM_IMAGE,			/* <input type=image>, only used internally	*/
	ANCHOR_JUMP,				/* href="#..."								*/
	ANCHOR_NAMED				/* name="...."								*/
}URLType;

/*****
* Various methods of loading documents.
* LOAD_NORMAL
*	The entire document is provided. This is the default method of loading
*	documents.
* LOAD_PROGRESSIVE
*	Progressive document load. A complete source is provided each time:
*	caller takes care of appending new text to th current buffer before
*	flushing it to XmHTML.
* LOAD_INCREMENTAL
*	Progressive document load. A new chunk is provided each time: XmHTML
*	takes care of appending new text to the already existing text.
* LOAD_SUSPEND
*	Suspend progressive or incremental loading. XmHTML display the document
*	upto the last valid combination of tokens.
* LOAD_ABORT
*	Abort progressive or icremental loading. XmHTML will flush it's
*	buffers and display all the data (including any constructs it
*	requires to close the document).
*****/
enum{
	XmLOAD_NORMAL = 0,		/* normal load, all data at once		*/
	XmLOAD_PROGRESSIVE,		/* progressive load, use new text		*/
	XmLOAD_INCREMENTAL,		/* progressive load, append new text	*/
	XmLOAD_SUSPEND,			/* suspend load							*/
	XmLOAD_ABORT			/* abort load							*/
};

/*****
* Procedure to be called whenever a reference to any of the HTML4.0 events is
* encountered
* Arguments:
*	Widget:		XmHTMLWidget id
*	String:		script data
*	XtPointer:	XmNclientData value
* Return value:
*	data to be stored with this event.
*	This data is unused internally and is provided as the user_data
*	argument in the XmHTMLEvent structure. For example, the return value
*	could be a pointer into some internal procedural database, a ptr to a
*	compiled script procedure or the script source text if you want to
*	process it at some later time (when the event occurs).
*
* When NULL is returned the event in question is disabled.
*
* Note that, most of the time, the script data will reference a script defined
* with the <script>...</script> HTML elements.
*****/
typedef XtPointer (*XmHTMLEventProc)(Widget, String, XtPointer);

/*****
* Definition of <script> contents.
* All fields must be copied into userspace; XmHTML will release the original
* strings when it no longer needs them.
*****/
typedef struct{
	String type;			/* internet content type for script language	*/
	String lang;			/* predefined script language name				*/
	String src;				/* URL for an external script					*/
	String script;			/* script text									*/
}XmHTMLScriptData;

/*****
* Procedure to be called whenever a <SCRIPT> is encountered.
* Arguments:
*	Widget:		XmHTMLWidget id
*	Script:		script data
*	XtPointer:	XmNclientData value
*
* Note that the script data may contain both a script definition as well as a
* reference to a script.
* Examples:
*
* Reference to a script:
*		<script language="javascript">
*		<!--
*			document.write("NewWin('passwd.html')");
*		// -->
*		</script>
*
* Definition of a script:
*		<script language="javascript">
*		<!--
*			document.passwordform.login.focus();
*			document.passwordform.passwd.value='';
*			document.passwordform.js.value="yes";
*			function NewWin(url)
*			{
*				var win = window.open(url,"PassWin",
*					"width=500,height=350,resize=yes, scrollbars=yes");
*			}
*		// -->
*		</script>
*
* Your XmHTMLScriptProc should be able to handle both.
*
*****/
typedef void (*XmHTMLScriptProc)(Widget, XmHTMLScriptData*, XtPointer);

/*****
* HTML Form component types. Alphabetically sorted to speed up searching.
*****/
typedef enum{
	FORM_CHECK = 0,		/* checkbox				*/
	FORM_FILE,			/* file selection box	*/
	FORM_HIDDEN,		/* hidden input			*/
	FORM_IMAGE,			/* drawnbutton			*/
	FORM_OPTION,		/* select child			*/
	FORM_PASSWD,		/* password textfield	*/
	FORM_RADIO,			/* radiobox				*/
	FORM_RESET,			/* reset button			*/
	FORM_SELECT,		/* select parent		*/
	FORM_SUBMIT,		/* submit button		*/
	FORM_TEXT,			/* singleline textfield	*/
	FORM_TEXTAREA,		/* multiline edit field	*/
	FORM_UNKNOWN		/* unknown type			*/
}componentType;

/*****
* Supported HTML Form method types
*****/
enum{
	XmHTML_FORM_GET = 0,			/* method = get						*/
	XmHTML_FORM_POST,				/* method = post					*/
	XmHTML_FORM_PIPE				/* method = pipe					*/
};

/*****
* possible error codes for XmNparserCallback
*****/
typedef enum{
	HTML_UNKNOWN_ELEMENT = 1,	/* unknown HTML element						*/
	HTML_BAD,					/* very badly placed element				*/
	HTML_OPEN_BLOCK,			/* block still open while new block started	*/
	HTML_CLOSE_BLOCK,			/* block closed but was never opened		*/
	HTML_OPEN_ELEMENT,			/* unbalanced terminator					*/
	HTML_NESTED,				/* improperly nested element				*/
	HTML_VIOLATION,				/* bad content for current block/element	*/
	HTML_NOTIFY,				/* notification of text insertion/removal	*/
	HTML_INTERNAL				/* internal parser error					*/
}parserError;

/*****
* And corresponding values for XmNenableBadHTMLWarnings.
* These are or'd together.
* XmNONE disables warnings and XmHTML_ALL enables all warnings.
* See parserError for their meaning.
*****/
enum{
	XmHTML_NONE = 0,				/* no warnings	*/
	XmHTML_UNKNOWN_ELEMENT = 1,	
	XmHTML_BAD = 2,
	XmHTML_OPEN_BLOCK = 4,
	XmHTML_CLOSE_BLOCK = 8,
	XmHTML_OPEN_ELEMENT = 16,
	XmHTML_NESTED = 32,
	XmHTML_VIOLATION = 64,
	XmHTML_ALL = 127			/* all warnings	*/
};

/*****
* possible action codes for the action field in the XmHTMLParserCallbackStruct
*****/
enum{
	HTML_REMOVE = 1,			/* remove offending element					*/
	HTML_INSERT,				/* insert missing element					*/
	HTML_SWITCH,				/* switch offending and expected element	*/
	HTML_KEEP,					/* keep offending element					*/
	HTML_IGNORE,				/* ignore, proceed as if nothing happened	*/
	HTML_ALIAS,					/* alias an unknown element to known one	*/
	HTML_TERMINATE				/* terminate parser							*/
};

/*****
* Possible return codes for XmHTMLImageGetType().
*****/
enum{
	IMAGE_ERROR = 0,			/* error on image loading			*/
	IMAGE_UNKNOWN,				/* unknown image					*/
	IMAGE_XPM,					/* X11 pixmap						*/
	IMAGE_XBM,					/* X11 bitmap						*/
	IMAGE_GIF,					/* CompuServe(C) Gif87a or Gif89a	*/
	IMAGE_GIFANIM,				/* animated gif						*/
	IMAGE_GIFANIMLOOP,			/* animated gif with loop extension	*/
	IMAGE_GZF,					/* compatible Gif87a or Gif89a		*/
	IMAGE_GZFANIM,				/* compatible animated gif			*/
	IMAGE_GZFANIMLOOP,			/* compatible animated gif 			*/
	IMAGE_JPEG,					/* JPEG image						*/
	IMAGE_PNG,					/* PNG image						*/
	IMAGE_FLG					/* Fast Loadable Graphic			*/
};

/*****
* Possible return values for a function installed on the
* XmNprogressiveReadProc resource.
*****/
#define STREAM_OK		 1		/* internally used value					*/
#define STREAM_END		 0		/* data stream ended (no more data)			*/
#define STREAM_SUSPEND	-1		/* data stream suspended (not enough data)	*/
#define STREAM_ABORT	-2		/* data stream aborted						*/
#define STREAM_RESIZE	-3		/* resize input buffer						*/

/*****
* Possible return values for the XmNdecodeGIFProc resource and 
* values for the XmHTMLGIFStream state.
*****/
#define GIF_STREAM_OK		 2
#define GIF_STREAM_END		 1
#define GIF_STREAM_ERR		 0
#define GIF_STREAM_INIT		-1
#define GIF_STREAM_FINAL	-2

/*****
* Possible return values from a number of image related routines.
* The actual meaning depends on the routine used.
*****/
typedef enum{
	XmIMAGE_ERROR = 0,		/* unknown error occured */
	XmIMAGE_BAD,			/* bad function call: missing arg or so */
	XmIMAGE_UNKNOWN,		/* provided XmImage/XmImageInfo unknown/unbound */
	XmIMAGE_ALMOST,			/* action completed, further response necessary */
	XmIMAGE_OK				/* action completed. */
}XmImageStatus;

/*****
* Possible values for transparency (value for the "bg" field in both
* XmImage and XmImageInfo structures). Possible values are:
*
* XmIMAGE_NONE
*	indicates the image is not transparent
* XmIMAGE_TRANSPARENCY_BG
*	indicates the image achieves transparency by substituting the current
*	background setting (can be a single color or background image. Internally,
*	such transparency is achieved by using a clipmask).
* XmIMAGE_TRANSPARENCY_ALPHA
*	indicates the image achieves transparency by using an alpha channel.
*	This transparency is currently only used by PNG images with an alpha
*	channel or a tRNS chunk (which is expanded to an alpha channel internally).
*****/
enum{
	XmIMAGE_NONE = 0,
	XmIMAGE_TRANSPARENCY_BG,
	XmIMAGE_TRANSPARENCY_ALPHA
};

/*****
* Possible values for the colorspace value.
*
* XmIMAGE_COLORSPACE_GRAYSCALE
*	image contains only shades of gray. The colorcube is reduced to a 1D
*	representation. All components in a shade have the same value. The
*	pixel values are equal to the value of a single color component.
* XmIMAGE_COLORSPACE_INDEXED
*	image uses a fixed palette. Colorcube is mapped to a 1D lookup-table.
* XmIMAGE_COLORSPACE_RGB
*	image uses a full 3D colorcube. 
*****/
enum{
	/* XmIMAGE_NONE */
	XmIMAGE_COLORSPACE_GRAYSCALE = 1,
	XmIMAGE_COLORSPACE_INDEXED,
	XmIMAGE_COLORSPACE_RGB
};

/*****
* XmImageInfo structure options field bits.
* The ``Set by default'' indicates a bit set when the XmHTMLImageDefaultProc
* is used to read an image. The ``Read Only'' indicates a bit you should
* consider as read-only.
* XmIMAGE_DELAYED
*	Indicates the image is delayed, e.i. it will be provided at a later stage;
* XmIMAGE_DEFERRED_FREE
*	Indicates XmHTML may free this structure when a new document is loaded.
* XmIMAGE_IMMEDIATE_FREE
*	Indicates XmHTML may free this structure when XmHTML no longer needs it;
* XmIMAGE_RGB_SINGLE
*	Indicates that the reds, greens and blues fields are allocated in a single
*	memory area instead of three seperate memory arrays.
* XmIMAGE_ALLOW_SCALE
*	Indicates that scaling an image is allowed.
* XmIMAGE_FRAME_IGNORE
*	Use with animations: set this bit when a frame falls outside the logical
*	screen area. No pixmap is created but the timeout for the frame is kept.
* XmIMAGE_CLIPMASK
*	This bit is set when the returned XmImageInfo structure contains clipmask
*	data. XmHTML uses this info to create a clipping bitmap. Changing this
*	bit from set to unset will lead to a memory leak while changing it from
*	unset to set *without* providing a clipmask yourself *will* cause an error
*	to happen. You can set this bit when you are providing your own clipmask
*	(to provide non-rectangular images for example), PROVIDED you fill the
*	``clip'' field with valid bitmap data (a stream of bytes in XYBitmap format
*	and the same size of the image).
* XmIMAGE_SHARED_DATA
*	This bit is set when images share data. XmHTML sets this bit when the image
*	in question is an internal image, e.i., one for which the image data may
*	never be freed. Be carefull setting this bit yourself, since it prevents
*	XmHTML from freeing the image data present in the XmImageInfo structure.
*	It can easily lead to memory leaks when an image is *not* an internal
*	image.
* XmIMAGE_PROGRESSIVE
*	Setting this bit will enable XmHTML progressive image loading. A function
*	*must* have been installed on the XmNprogressiveReadProc resource *prior*
*	to setting this bit. Installing a function on the XmNprogressiveEndProc
*	is optional. When this bit is set all other bits will be ignored.
* XmIMAGE_DELAYED_CREATION
*	This bit is read-only. It is used internally by XmHTML for images with
*	an alpha channel. Alpha channel processing merges the current background
*	with the original RGB data from the image and uses the result to compose
*	the actual on-screen image (the merged data is stored in the ``data''
*	field of the XmImageInfo structure). XmHTML needs to store the original
*	data somewhere, and when this bit is set it is stored in the ``rgb'' field
*	of the XmImageInfo structure.
*	When this bit is set, the returned XmImageInfo may *NOT BE FREED* as long
*	as the current document is alive. You can discard it as soon as a new
*	document is loaded.
*****/
#define XmIMAGE_DELAYED			(1<<1)
#define XmIMAGE_DEFERRED_FREE	(1<<2)		/* set by default */
#define XmIMAGE_IMMEDIATE_FREE	(1<<3)
#define XmIMAGE_RGB_SINGLE		(1<<4)		/* set by default */
#define XmIMAGE_ALLOW_SCALE		(1<<5)		/* set by default */
#define XmIMAGE_FRAME_IGNORE	(1<<6)
#define XmIMAGE_CLIPMASK		(1<<7)		/* Read Only */
#define XmIMAGE_SHARED_DATA		(1<<8)		/* Read Only */
#define XmIMAGE_PROGRESSIVE		(1<<9)

#define XmIMAGE_DELAYED_CREATION (1<<10)	/* Read Only */

/*****
* XmImageInfo animation disposal values
* A disposal method specifies what should be done before the current frame is
* rendered. Possible values are:
* XmIMAGE_DISPOSE_NONE
*	do nothing, overlays the previous frame with the current frame.
* XmIMAGE_DISPOSE_BY_BACKGROUND
*	Restore to background color. The area used by the previous frame must
*	be restored to the background color/image
* XmIMAGE_DISPOSE_BY_PREVIOUS
*	Restore to previous. The area used by the previous frame must be
*	restored to what was there prior to rendering the previous frame.
*****/
enum{
	/* XmIMAGE_NONE */
	XmIMAGE_DISPOSE_NONE = 1,		/* default behaviour */
	XmIMAGE_DISPOSE_BY_BACKGROUND,
	XmIMAGE_DISPOSE_BY_PREVIOUS
};

/*****
* Primary image cache actions
* (unimplemented)
*****/
#define IMAGE_STORE		0			/* store an image in the cache */
#define IMAGE_GET		1			/* retrieve an image from the cache */
#define IMAGE_DISCARD	2			/* discard an image from the cache */

/*****
* XmNperfectColors/XmNalphaChannelProcessing resource values.
*
* Note: these values are represented by the XmCEnableMode resource class.
*****/
enum{
	/* XmAUTOMATIC */
	XmALWAYS = 1,
	XmNEVER
};

/*****
* Possible XmNimageMapToPalette/XmNimageRGBConversion resource values:
*
* XmQUICK
*	RGBConversion:
*		first checks if the 24bit image contains less than XmNmaxImageColors.
*		If not, XmHTML will dither to a fixed palette. This is fast but has
*		the disadvantage that the background color in an alpha channelled
*		image will not be matched exactly.
*	MapToPalette:
*		Use closest distance algorithm to map colors to the palette. No
*		error correction is performed. Reasonably fast, but quality 
*		heavily depends on the distribution of the colors in the image.
* XmBEST
*	RGBConversion (default):
*		first checks if the 24bit image contains less than XmNmaxImageColors.
*		If it is, the actual image colors are used. If not, a histogram of the
*		image is computed, the most used colors are selected and the resulting
*		image is dithered to this palette.
*		Offers best 24 to 8bit conversion and is probably faster than XmSLOW
*		as only images with more than XmNmaxImageColors will be dithered. 
*	MapToPalette:
*		Ordered dithering using predefined error matrices. Reasonably fast and
*		quite good results;
* XmFAST
*	RGBConversion:
*		Skips the check and dithers to a fixed palette right away. This is the
*		fastest way to do 24 to 8bit conversion but has the disadvantage that
*		every 24bit image is dithered to a fixed palette, regardless of the
*		actual no of colors in the image.
*	MapToPalette:
*		Simple ordered dithering. Fastest but probably the poorest results.
* XmSLOW
*	RGBConversion:
*		Skips the check and does histogram stuff right away.
*	MapToPalette:
*		closest distance algorithm to map image colors to the palette and use
*		dynamic error correction. Slowest but best results;
* XmDISABLED
*	RGBConversion:
*		ignored;
*	MapToPalette (default):
*		Disables palette mapping;
*
* Note: these values are represented by the XmCConversionMode resource class.
*****/
enum{
	XmQUICK = 0,
	XmBEST,
	XmFAST,
	XmSLOW,
	XmDISABLED
};

/*****
* Embedded object flags. These flags tell XmHTML which attributes are
* defined for an object.
*****/
#define HT_TAG_CLASSID		(1<<1)	/* classid, defines an implementation	*/
#define HT_TAG_CODEBASE		(1<<2)	/* base url for applet					*/
#define HT_TAG_DATA			(1<<3)	/* object data							*/
#define HT_TAG_SRC			(1<<4)	/* object data							*/
#define HT_TAG_TYPE			(1<<5)	/* internet data content type			*/
#define HT_TAG_CODETYPE		(1<<6)	/* internet code content type			*/
#define HT_TAG_STANDBY		(1<<7)	/* standby message						*/
#define HT_TAG_ALIGN		(1<<8)	/* global alignment						*/
#define HT_TAG_HALIGN		(1<<9)	/* horizontal alignment					*/
#define HT_TAG_VALIGN		(1<<10)	/* vertical alignment					*/
#define HT_TAG_HEIGHT		(1<<11)	/* suggested height						*/
#define HT_TAG_WIDTH		(1<<12)	/* suggested width						*/
#define HT_TAG_BORDER		(1<<13)	/* suggested border width				*/
#define HT_TAG_HSPACE		(1<<14)	/* suggested horizontal gutting space	*/
#define HT_TAG_VSPACE		(1<<15)	/* suggested vertical gutting space		*/
#define HT_TAG_USEMAP		(1<<16)	/* imagemap reference					*/
#define HT_TAG_BACKGROUND	(1<<17)	/* background image specification		*/
#define HT_TAG_BGCOLOR		(1<<18)	/* background color specification		*/
#define HT_TAG_HREF			(1<<19)	/* hyperlink							*/
#define HT_TAG_NAME			(1<<20)	/* object name/named hyperlink			*/
#define HT_TAG_EVENTS		(1<<21)	/* HTML4.0 events						*/

/*****
* Possible return values for the embedded object convenience functions
*****/
typedef enum{
	OBJECT_ALMOST = 0,		/* action completed, further action required */
	OBJECT_DESTROYED,		/* object has been destroyed		*/
	OBJECT_EMPTY,			/* object is empty					*/
	OBJECT_ERROR,			/* unknown error					*/
	OBJECT_FATAL,			/* fatal object error has occured	*/
	OBJECT_INVALID,			/* invalid object					*/
	OBJECT_INVALID_LOCATION,/* object has an invalid location	*/
	OBJECT_INVALID_SIZE,	/* object has an invalid size		*/
	OBJECT_LOWERED,			/* object is lowered				*/
	OBJECT_MAPPED,			/* object is mapped to screen		*/
	OBJECT_OK,				/* action completed succesfully		*/
	OBJECT_ORPHANED,		/* object has no parent				*/
	OBJECT_PARENTED,		/* object already has a parent		*/
	OBJECT_RAISED,			/* object is raised					*/
	OBJECT_UNIMPLEMENTED,	/* requested function unimplemented	*/
	OBJECT_UNKNOWN,			/* unknown object					*/
	OBJECT_UNKNOWN_ELEMENT,	/* invalid element id				*/
	OBJECT_UNMAPPED,		/* object has been unmapped			*/
	OBJECT_UNUSED,			/* object is not being used			*/
	OBJECT_USED,			/* object is currently being used	*/
	OBJECT_VISIBLE			/* object is visible				*/
}XmHTMLObjectStatus;

/*****
* Search directions: forward or backward search
*****/
typedef enum{
	XmHTML_FORWARD = 0,
	XmHTML_BACKWARD
}XmHTMLDirection;

/*****
* XmHTMLTextFind return codes
*****/
typedef enum{
	XmREG_ERROR = 0,		/* An error occured					*/
	XmREG_NOMATCH,			/* end of text and no match found	*/
	XmREG_MATCH				/* a match was found				*/
}XmHTMLRegexStatus;

/*****
* In addition to the POSIX regex error codes, XmHTMLTextFinderGetError
* can return the following errors
*****/
#define RE_EEMPTY       -1  /* no search string given					*/
#define RE_ENOMEM       -2  /* out of memory							*/
#define RE_EBADPARENT   -3  /* parent is not of class xmHTMLWidgetClass	*/
#define RE_EWORDS       -4  /* no words to be searched (empty document)	*/
#define RE_ERROR        -5  /* unknown error							*/

/*****
* Finder is a fully opaque types.
*****/
typedef struct _XmHTMLTextFinder	*XmHTMLTextFinder;

/*****
* Custom Papersize dimension unit type
* (under construction)
*****/
enum{
	XmHTML_CHAR = 0,
	XmHTML_CENTIMETER,			/* 1cm = 0.39in					*/
	XmHTML_MILLIMETER,			/* 1mm = 0.1cm					*/
	XmHTML_INCH,				/* 1in = 2.54cm					*/
	XmHTML_PICA,				/* 1pc = 12pt					*/
	XmHTML_POINT				/* 1in = 72.27pt, 1cm = 28.45pt	*/
};

/*****
* XmHTMLTextGetFormatted paper size defines
* (under construction)
*****/
enum{
	XmHTMLTEXT_PAPERSIZE_A4 = 0,
	XmHTMLTEXT_PAPERSIZE_LETTER,
	XmHTMLTEXT_PAPERSIZE_CUSTOM
};

/*****
* XmHTMLTextGetFormatted type definitions
* (under construction)
*****/
enum{
	XmHTMLTEXT_PLAIN = 0,		/* generate plain ASCII document		*/
	XmHTMLTEXT_FORMATTED,		/* generate formatted ASCII document	*/
	XmHTMLTEXT_POSTSCRIPT 		/* generate formatted Postscript output	*/
};

/*****
* XmHTMLTextGetFormatted Postscript option bits
* (under construction)
* The MIMIC_FONTS bit instructs XmHTML to use any of the supported postscript
* fonts to approach the fonts used in the document. When set, all other font
* bits are ignored. When not used, the PSFONT bits can be or'd together.
* XmHTML will attempt to do the following mapping:
*	PSFONT_ROMAN/PSFONT_CENTURY -> default text font;
*	PSFONT_HELVETICA/PSFONT_LUCIDA -> fixed width font;
* If only one of the PSFONT bits is set, the entire document will be rendered
* in that font.
*****/
#define XmHTMLTEXT_ANCHORFOOTNOTES			(1L<<0)		/* footnote anchors */
#define XmHTMLTEXT_ADDHEADER				(1L<<1)		/* put title */
#define XmHTMLTEXT_ADDFOOTER				(1L<<2)		/* put pagenumbers */
#define XmHTMLTEXT_PSFONT_ROMAN				(1L<<3)
#define XmHTMLTEXT_PSFONT_HELVETICA			(1L<<4)
#define XmHTMLTEXT_PSFONT_CENTURY			(1L<<5)
#define XmHTMLTEXT_PSFONT_LUCIDA			(1L<<6)
#define XmHTMLTEXT_MIMIC_FONTS				(1L<<7)

/*****
* XmHTMLGetHeadAttributes mask bits
*****/
#define HeadClear		((unsigned char)0)	/* clear everything		*/
#define HeadDocType		(1<<0)				/* fill doctype member	*/
#define HeadTitle		(1<<1)				/* fill title member	*/
#define HeadIsIndex		(1<<2)				/* fill isIndex member	*/
#define HeadBase		(1<<3)				/* fill Base member		*/
#define HeadMeta		(1<<4)				/* fill meta members	*/
#define HeadLink		(1<<5)				/* fill link members	*/
#define HeadScript		(1<<6)				/* fill script members	*/
#define HeadStyle		(1<<7)				/* fill Style members	*/
#define HeadAll			((unsigned char)~0)	/* fill all members		*/

/*****
* XmImage frame selection flags.
* any positive number will return the requested frame. If larger than
* total framecount, last frame is returned.
*****/
#define AllFrames		-1 			/* do all frames				*/
#define FirstFrame		-2			/* only use first frame			*/
#define LastFrame		-3			/* only do last frame			*/

/*****
* XmImage configuration flags
*****/
#define ImageFSDither		(1L<<1)	/* Floyd-Steinberg on quantized images	*/
#define ImageCreateGC		(1L<<2)	/* create gc for image					*/
#define ImageWorkSpace		(1L<<3)	/* create animation workspace			*/
#define ImageClipmask		(1L<<4)	/* create clipmask						*/
#define ImageBackground		(1L<<5)	/* substitute background pixel			*/
#define ImageQuantize		(1L<<6)	/* quantize image						*/
#define ImageMaxColors		(1L<<7)	/* sets maximum colors					*/
#define ImageGifDecodeProc	(1L<<8)	/* gif lzw decoder function				*/
#define ImageGifzCmd		(1L<<9)	/* gif lzw uncompress command			*/
#define ImageFrameSelect	(1L<<10)/* frame selection						*/
#define ImageScreenGamma	(1L<<11)/* gamma correction. JPEG and PNG only	*/

/******************************************************************************
* Commonly used structures
******************************************************************************/

/*****
* Representation of parsed HTML elements
*****/
typedef struct _XmHTMLObject{
	htmlEnum id;				/* internal ID for this element				*/
	String element;				/* element text								*/
	String attributes;			/* element attributes (if any)				*/
	Boolean is_end;				/* true if this is a closing element		*/
	Boolean terminated;			/* true if element has closing counterpart	*/
	Cardinal line;				/* line number in input for this element	*/
	struct _XmHTMLObject *next;
	struct _XmHTMLObject *prev;
}XmHTMLObject;

/*****
* Custom papersize definition
*****/
typedef struct _XmHTMLPaperSize{
	unsigned char unit_type;	/* unit in which dimensions are specified	*/
	unsigned char paper_type;	/* type of paper: A4, US or custom			*/
	Cardinal width;				/* total paper width						*/
	Cardinal height;			/* total paper height						*/
	Cardinal left_margin;		/* left text margin							*/
	Cardinal right_margin;		/* right text margin						*/
	Cardinal top_margin;		/* top text margin							*/
	Cardinal bottom_margin;		/* bottom text margin						*/
}XmHTMLPaperSize;

/*****
* Definition of a HTMLTextPosition.
* Should be considered an opaque object.
*****/
typedef struct _XmHTMLTextPosition{
	struct _XmHTMLObjectTable *start;	/* selection start					*/
	int idx;							/* first word in selection start	*/
	int nwords;							/* no of words in selection start	*/
	int fc;								/* first char in selection start	*/
}XmHTMLTextPosition;

/***** 
* The following structure is returned by the XmHTMLImageDefaultProc convenience
* function. When a procedure for the XmNimageProc resource is installed,
* it *must* return this structure.
*****/
typedef struct _XmImageInfo
{
	/* regular image fields */
	String url;					/* original location of image				*/
	unsigned char *data;		/* raw image data. ZPixmap format			*/
	unsigned char *clip;		/* raw clipmask data. XYBitmap format		*/
	Dimension width;			/* used image width, in pixels				*/
	Dimension height;			/* used image height, in pixels				*/
	Dimension *reds;			/* red image pixels							*/
	Dimension *greens;			/* green image pixels						*/
	Dimension *blues;			/* blue image pixels						*/
	int bg;						/* transparent pixel index/type				*/
	unsigned int ncolors;		/* Number of colors in the image			*/
	unsigned int options;		/* image option bits						*/

	/* image classification fields and original data */
	unsigned char type;			/* image type, see the IMAGE_ enum above	*/
	unsigned char depth;		/* bits per pixel for this image			*/
	unsigned char colorspace;	/* colorspace for this image				*/
	unsigned char transparency;	/* transparency type for this image			*/
	Dimension swidth;			/* image width as read from image			*/
	Dimension sheight;			/* image height as read from image			*/
	unsigned int scolors;		/* Original number of colors in the image	*/

	/* Special fields for images with an alpha channel */
	unsigned char *alpha;		/* alpha channel data 						*/
	float fg_gamma;				/* image gamma								*/

	/* Additional animation data */
	int x;						/* logical screen x-position for this frame	*/
	int y;						/* logical screen y-position for this frame	*/
	int loop_count;				/* animation loop count						*/
	unsigned char dispose;		/* image disposal method					*/
	int timeout;				/* frame refreshment in milliseconds		*/
	int nframes;				/* no of animation frames remaining			*/
	struct _XmImageInfo *frame;	/* ptr to next animation frame				*/

	XtPointer user_data;		/* any data to be stored with this image	*/
}XmImageInfo, *XmImageInfoStruct;

/* XmHTML method to load images */
typedef XmImageInfo* (*XmImageProc)(Widget, String, Dimension, Dimension,
	XtPointer);

/****
* The next two structures constitute the XmImage definition which are used by
* the XmImageCreate and XmImageDestroy routines. Please note that the *only*
* safe way to destroy an XmImage is to use the XmImageDestroy function.
* XmHTML does not use the XmImage structure, it is provided for your
* convenience.
****/
/****
* Animation frame data.
****/
typedef struct{
	int x;					/* x position in logical screen		*/
	int y;					/* y position in logical screen		*/
	int w;					/* width of this particular frame	*/
	int h;					/* height of this particular frame	*/
	int timeout;			/* timeout for the next frame		*/
	unsigned char dispose;	/* previous frame disposal method	*/
	Pixmap pixmap;			/* actual image						*/
	Pixmap clip;			/* image clipmask					*/
	Pixmap prev_state;		/* previous screen state			*/
}XmImageFrame;

/*****
* Actual image definition.
* If you want access to the xcc member, include the XCC.h header file.
*****/
typedef struct{
	String file;				/* originating file							*/
	unsigned char type;			/* image type, see the IMAGE_ enum below	*/
	Pixmap pixmap;				/* actual image								*/
	Pixmap clip;				/* for transparant pixmaps					*/
	unsigned int options;		/* image option bits						*/
	int width;					/* current image width, in pixels			*/
	int height;					/* current image height, in pixels			*/
	int ncolors;				/* no of colors in this image				*/
	int scolors;				/* specified no of colors 					*/
	int swidth;					/* image width as read from image			*/
	int sheight;				/* image height as read from image			*/
	int depth;					/* depth of this image						*/
	int npixels;				/* no of really allocated pixels			*/
	GC gc;						/* graphics context for rendering			*/

	/* animation data */
	XmImageFrame *frames;		/* array of animation frames				*/
	int nframes;				/* no of frames following					*/
	int current_frame;			/* current frame count						*/
	int current_loop;			/* current loop count						*/
	int loop_count;				/* maximum loop count						*/
	XtIntervalId proc_id;		/* timer id for animations					*/
	Widget w;					/* image owner								*/
	XtAppContext context;		/* Application context for animations		*/

	/* private data */
  	struct _XColorContext *xcc;	/* a lot of visual info						*/
	struct _ToolkitAbstraction *tka;
}XmImage;

/*****
* Link member information.
*****/
typedef struct
{
	String url;				/* value of URL tag		*/
	String rel;				/* value of REL tag		*/
	String rev;				/* value of REV tag		*/
	String title;			/* value of TITLE tag	*/
}XmHTMLLinkDataRec, *XmHTMLLinkDataPtr;

/*****
* Meta member information.
*****/
typedef struct
{
	String http_equiv;		/* value of HTTP-EQUIV tag	*/
	String name;			/* value of NAME tag		*/
	String content;			/* value of CONTENT tag		*/
}XmHTMLMetaDataRec, *XmHTMLMetaDataPtr;

/*****
* XmHTMLHeadAttributes definition
*****/
typedef struct{
	String doctype;				/* doctype data								*/
	String title;				/* document title							*/
	Boolean is_index;			/* true when the <isindex> element exists	*/
	String base;				/* value of the <base> element				*/
	int num_meta;				/* number of META info to process			*/
	XmHTMLMetaDataPtr meta;		/* array of META info to process			*/
	int num_link;				/* number of LINK info to process			*/
	XmHTMLLinkDataPtr link;		/* array of LINK info to process			*/
	String style_type;			/* value of the style type element tag		*/
	String style;				/* <style></style> contents					*/
	String script_lang;			/* value of the language element tag		*/
	String script;				/* <script></script> contents				*/
}XmHTMLHeadAttributes;

/*****
* Document Information.
* images and anchors contain all src and href's in a single string. Entries
* are separated by the null char and terminated by two null chars.
*****/
typedef struct{
	String bg_image;			/* background image (if any)				*/
	String images;				/* list of all images (if any)				*/
	String anchors;				/* list of all anchors (excluding named)	*/
}XmHTMLDocumentInfo;

/*****
* XmHTMLFontCacheInfo definition. This structure is returned by the
* XmHTMLGetFontCacheInfo convenience routine and contains information about
* the font cache bound to the display of a given widget.
* The fonts and mapping arrays are in sync: when a name in the fonts array
* has a non-NULL entry at the corresponding position in the mapping array,
* the value of the mapping entry is the real font being used.
*****/
typedef struct{
	int nentries;					/* total no of cached fonts */
	int nmaps;						/* of which this many are mapped fonts */
	int nlookups;					/* no of search actions */
	int nrequests;					/* no of requests made */
	int hits;						/* no of hits */
	int misses;						/* no of misses */
	String *fonts;					/* array of font names, size nentries */
	String *mapping;				/* array of font mappings, size nentries */
	int nwidgets;					/* no of widgets using this cache */
	WidgetList widgets;				/* array of widgets */
}XmHTMLFontCacheInfo;

/*****
* forward declaration of XmHTMLAnchorCallback structure
*****/
typedef struct _XmHTMLAnchorCallbackStruct *XmHTMLAnchorPtr;

/*****
* XmHTMLXYToInfo return value
* This structure and any of it members may *never* be freed by the caller.
*****/
typedef struct
{
	Cardinal line;				/* line number at selected position			*/
	Boolean is_map;				/* true when clicked image is an imagemap	*/
	int x,y;					/* position relative to image corner		*/
	XmImageInfo *image;			/* image data								*/
	XmHTMLAnchorPtr anchor;		/* possible anchor data						*/
}XmHTMLInfoStructure, *XmHTMLInfoPtr;

/*****
* XmHTML progressive object loading
* (PLC stands for Progressive Loader Context)
*****/
typedef struct _XmHTMLPLCStream{
	Cardinal total_in;          /* no of bytes received so far              */
	Cardinal min_out;           /* minimum number of bytes requested        */
	Cardinal max_out;           /* maximum number of bytes requested        */
	XtPointer user_data;        /* any data registered on this PLC          */
	unsigned char pad[24];      /* reserved for future use                  */
}XmHTMLPLCStream;

/*****
* External GIF decoder stream object. This is the only argument to any
* procedure installed on the XmNdecodeGIFProc resource.
*
* The first block is kept up to date by XmHTML and is read-only. When state
* is GIF_STREAM_INIT, the decoder should initialize it's private data and store
* it in the external_state field so that it can be used for successive calls
* to the decoder. When state is GIF_STREAM_FINAL, the decoder should wrapup
* and flush all pending data. It can also choose to destruct it's internal
* data structures here (another call with state set to GIF_STREAM_END will be
* made when the internal loader is destroying it's internal objects as
* well).
*
* All following fields are the ``public'' fields and must be updated by the 
* external decoder. The msg field can be set to an error message if the
* external decoder fails for some reason. XmHTML will then display this 
* error message and abort this image.
*****/
typedef struct _XmHTMLGIFStream{
	/* read-only fields */
	int state;					/* decoder state						*/
	int codesize;				/* initial LZW codesize					*/
	Boolean is_progressive;		/* when used by a progressive loader	*/
	unsigned char *next_in;		/* next input byte						*/
	Cardinal avail_in;			/* number of bytes available at next_in	*/
	Cardinal total_in;			/* total nb of input bytes read so far	*/

	/* fields to be updated by caller */
	unsigned char *next_out;	/* next output byte should be put there	*/
	Cardinal avail_out;			/* remaining free space at next_out		*/
	Cardinal total_out;			/* total nb of bytes output so far		*/

	String msg;					/* last error message, or NULL			*/
	XtPointer external_state;	/* room for decoder-specific data		*/
}XmHTMLGIFStream;

/* and the proto for the XmNdecodeGIFProc resource */
typedef int (*XmImageGifProc)(XmHTMLGIFStream*);

/*****
* Progressive Loading function prototypes.
* XmHTMLGetDataProc: proto for function installed on the
*                    XmNprogressiveReadProc resource;
* XmHTMLEndDataProc: proto for function installed on the
*                    XmNprogressiveEndProc resource;
*****/
typedef int  (*XmHTMLGetDataProc)(XmHTMLPLCStream*, XtPointer);
typedef void (*XmHTMLEndDataProc)(XmHTMLPLCStream*, XtPointer, int, Boolean);

/*****
* possible values for the third argument on the EndDataProc
*****/
enum{
	/* XmHTML_NONE = 0 */	/* PLCObject referenced by all objects */
	XmPLC_IMAGE,			/* PLCObject for an image */
	XmPLC_DOCUMENT,			/* PLCObject for a document */
	XmPLC_FINISHED			/* indicates all plc's have been processed */
};

/*****
* XmImage configuration
*****/
typedef struct{
	unsigned long flags;		/* XmImage configuration flags, see above	*/
	int ncolors;				/* desired number of colors					*/
	int which_frames;			/* animation frames selection flag			*/
	int bg_color;				/* background pixel on transparent images	*/
	String z_cmd;				/* gif uncompress command					*/
	XmImageGifProc gif_proc;	/* external gif decoder						*/
	float gamma;				/* gamma correction. JPEG and PNG only		*/
	struct _ToolkitAbstraction *tka;	/* private data						*/
}XmImageConfig;

/*****
* HTML4.0 Event structure
*****/
typedef struct _XmHTMLEvent{
	int type;					/* HTML4.0 event type, see above			*/
	String script;				/* contents of event 						*/
}XmHTMLEvent;

/*****
* XmNobjectCallback tag_attributes substructure.
*****/
typedef struct{ 
	unsigned long flags;	/* defines which fields have a value		*/
	String attributes;		/* unfiltered attributes					*/

	String classid;			/* contents of the classid attribute		*/
	String codebase;		/* contents of the codebase attribute		*/
	String data;			/* location of object data					*/
	String src;				/* location of object data					*/
	String type;			/* type of object data						*/
	String codetype;		/* type of codebase							*/
	String standby;			/* message to display while loading			*/
	String align;			/* global alignment							*/
	String halign;			/* horizontal alignment						*/
	String valign;			/* vertical alignment						*/
	int height;				/* following five fields are always integers*/
	int width;
	int border;				/* border = 0: no border					*/
	int hspace;
	int vspace;
	String usemap;			/* location or name of imagemap				*/
	String background;		/* location of background image				*/
	String bgcolor;			/* name of background color					*/
	String href;			/* hyperlink reference						*/
	String name;			/* named hyperlink							*/
	XmHTMLEvent *events;	/* defined events							*/
	int nevents;			/* no of events								*/
}XmHTMLTagAttributes;

/*****
* The XmHTMLElementId and XmHTMLObjectId are opaque structures. They
* are fully defined in XmHTMLP.h
*****/
typedef struct _XmHTMLParserTag *XmHTMLElementId;

typedef struct _XmHTMLExtObj *XmHTMLObjectId;

/******************************************************************************
* Callback structures
* Unless explicitly mentioned, *none* of these structures (or any of its
* members) may be freed.
******************************************************************************/

/*****
* XmNactivateCallback and XmNanchorTrackCallback callback structure.
*****/
typedef struct _XmHTMLAnchorCallbackStruct{
	int reason;				/* the reason the callback was called		*/
	XEvent *event;			/* event structure that triggered callback	*/
	URLType url_type;		/* type of url referenced					*/
	Cardinal line;			/* line number of the selected anchor		*/
	String href;			/* pointer to the anchor value				*/
	String target;			/* pointer to target value					*/
	String rel;				/* pointer to rel value						*/
	String rev;				/* pointer to rev value						*/
	String title;			/* pointer to title value					*/
	Boolean is_frame;		/* true when inside a frame					*/
	Boolean doit;			/* local anchor vetoing flag				*/
	Boolean visited;		/* local anchor visited flag				*/
	Boolean doc_modified;	/* Set to True when document is modified	*/
}XmHTMLAnchorCallbackStruct;

typedef Boolean (*XmHTMLAnchorProc)(Widget, String, XtPointer);

/*****
* XmNeventCallback callback structure.
*
* Note on the doc_modified field: this field *MUST* be set to True if the
* processing of this event leads to a modification of the currently
* displayed document (XmHTMLTextSetString for example) or even destruction
* of the widget. It informs XmHTML of the fact that it can no longer rely on
* the consistency of it's internal data structures.
*
* Note on processing of the XmCR_HTML_LOAD and XmCR_HTML_UNLOAD events:
* processing of these events may *NEVER* cause a modification of the currently
* displayed document. It is a FATAL ERROR if it does change: XmHTML can no
* longer guarantee the consistency of it's internal data.
*****/
typedef struct{
	int			reason;			/* the reason the event was called		*/
	XEvent		*event;			/* event triggering this action			*/
	int			type;			/* HTML4.0 event type, see above		*/
	XtPointer	data;			/* HTML4.0 event callback data			*/
	Boolean		doc_modified;	/* Set to True when document is modified*/
}XmHTMLEventCallbackStruct, *XmHTMLEventCallbackPtr;

/*****
* XmNdocumentCallback callback structure.
*****/
typedef struct
{
	int reason;					/* the reason the callback was called		*/
	XEvent *event;				/* always NULL for XmNdocumentCallback		*/
	Boolean html32;				/* True if document was HTML 3.2 conforming	*/
	Boolean verified;			/* True if document has been verified		*/
	Boolean balanced;			/* True if parser tree is balanced			*/
	Boolean terminated;			/* True if parser terminated prematurely	*/
	int pass_level;				/* current parser level count. Starts at 1	*/
	Boolean redo;				/* perform another pass?					*/
}XmHTMLDocumentCallbackStruct, *XmHTMLDocumentPtr;

/*****
* XmNformCallback callback structure.
*****/
/*****
* Form Component data
*****/
typedef struct
{
	componentType type;			/* Form component type	*/
	String name;				/* component name		*/
	String value;				/* component value		*/
}XmHTMLFormDataRec, *XmHTMLFormDataPtr;

/*****
* Actual XmNformCallback callback structure.
*****/
typedef struct
{
	int reason;				/* the reason the callback was called		*/
	XEvent *event;			/* event structure that triggered callback	*/
	String action;			/* URL or cgi-bin destination				*/
	String enctype;			/* form encoding							*/
	int method;				/* Form Method, GET, POST or PIPE			*/
	int ncomponents;		/* no of components in this form			*/
	XmHTMLFormDataPtr components;
	Boolean doc_modified;	/* Set to True when document is modified	*/
}XmHTMLFormCallbackStruct, *XmHTMLFormPtr;

/*****
* XmNframeCallback callback structure.
* This callback is activated when one of the following events occurs:
* 1. XmHTML wants to create a frame, reason = XmCR_HTML_FRAMECREATE
*    can be veto'd by setting doit to False and supplying a HTML widget
*    id yourself;
* 2. XmHTML wants to destroy a frame, reason = XmCR_HTML_FRAMEDESTROY
*    can be veto'd by setting doit to False (widget reuse).
* 3. XmHTML has finished creating a frame, reason = XmCR_HTML_FRAME.
*    This is the time to attach callbacks and set additional resources on the
*    newly created XmHTMLWidget.
*****/
typedef struct
{
	int reason;			/* the reason the callback was called		*/
	XEvent *event;		/* event structure that triggered callback	*/
	String src;			/* requested document						*/
	String name;		/* frame name								*/
	Widget html;		/* XmHTML widget id							*/
	Boolean doit;		/* destroy/create vetoing flag				*/
}XmHTMLFrameCallbackStruct, *XmHTMLFramePtr;

/*****
* XmNimagemapCallback callback structure.
* callback reasons can be one of the following: 
* XmCR_HTML_IMAGEMAP_ACTIVATE
*	user clicked on an image. Valid fields are x, y and image_name. x and y
*	are relative to the upper-left corner of the image. Only invoked when
*	an image has it's ismap attribute set and no client-side imagemap is
*	present for this image.
* XmCR_HTML_IMAGEMAP
*	an image requires an external imagemap. The only valid field is map_name
*	which contains the location of the imagemap to fetch. If the contents
*	of this imagemap is set in the map_contents field, it will be loaded
*	by the widget. Alternatively, one could also use the XmHTMLAddImagemap
*	convenience routine to set an imagemap into the widget.
*****/
typedef struct
{
	int reason;			/* the reason the callback was called				*/
	XEvent *event;		/* event structure that triggered callback			*/
	int x,y;			/* position relative to the upper-left image corner	*/
	String image_name;	/* name of referenced image, value of src attribute	*/
	String map_name;	/* name of imagemap to fetch/referenced				*/
	String map_contents;/* contents of fetched imagemap						*/
	XmImageInfo *image;	/* image data										*/
}XmHTMLImagemapCallbackStruct, *XmHTMLImagemapPtr;

/*****
* XmNlinkCallback callback structure.
*****/
typedef struct{
	int reason;					/* the reason the callback was called		*/
	XEvent *event;				/* event structure that triggered callback	*/
	int num_link;				/* number of LINK info to process			*/
	XmHTMLLinkDataPtr link;		/* array of LINK info to process			*/
}XmHTMLLinkCallbackStruct, *XmHTMLLinkPtr;

/*****
* XmNpageCallback callback structure.
*****/
typedef struct{
	int reason;					/* the reason the callback was called		*/
	XEvent *event;				/* event structure that triggered callback	*/
	Cardinal curr_page;			/* current page								*/
	Cardinal total_pages;		/* no of pages in document					*/
}XmHTMLPageCallbackStruct, *XmHTMLPagePtr;

/*****
* XmNobjectCallback callback structure.
*****/
typedef struct{
	int reason;				/* the reason the callback was called		*/
	XEvent *event;			/* event structure that triggered callback	*/

	int object_id;			/* id of object, set by XmHTML				*/

	int tag_id;				/* HTML tag identifying an embedded object	*/
	XtPointer tag_user_data;/* any data associated with this tag		*/
	XmHTMLTagAttributes *attributes;
							/* parsed attributes, immutable				*/

	Widget parent;			/* Widget id of parent.						*/
	Widget object;
	Window window;

	Boolean scrollable;		/* If True, object is part of document
							 * If False, object stays on top of document.
							 */
	Boolean wrap;			/* Only used when scrollable == False.
							 * If False, document scrolls underneath
							 * the object.
							 * If True, document scrolls around object.
							 */
	Boolean reparent;		/* If True, XmHTML will reparent the window */

	XtPointer user_data;	/* additional data registered for this object.
							 * Unused by XmHTML.
							 */

	/* Next two fields are only meaningfull when scrollable == False */
	int x;					/* suggested upper-left position			*/
	int y;					/* suggested upper-left position			*/

	Dimension width;		/* suggested object width					*/
	Dimension height;		/* suggested object height					*/

	Boolean is_frame;		/* true when inside a frame					*/
	Boolean doit;			/* destruct veto flag						*/
}XmHTMLObjectCallbackStruct, *XmHTMLObjectPtr;

/* Don't add anything after this endif! */
#endif /* _HTML_h_ */
