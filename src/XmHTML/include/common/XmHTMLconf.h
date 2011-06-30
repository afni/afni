/*****
* XmHTMLconf.h : overall configuration settings.
*
* This file Version	$Revision$
*
* Creation date:		Tue Oct 13 23:49:13 CEST 1998
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				XmHTML Developers Account
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
* $Source$
*****/
/*****
* ChangeLog 
* $Log$
* Revision 1.1  2011/06/30 16:08:56  rwcox
* Cadd
*
*
*****/ 

/*****
* This file contains a number of default values that can only be set at
* compile-time. These values modify the default behaviour of the XmHTML
* widget, so be carefull when modifying these values.
*****/

#ifndef _XmHTMLconf_h_
#define _XmHTMLconf_h_

#include <errno.h>

#ifndef BYTE_ALREADY_TYPEDEFED
#define BYTE_ALREADY_TYPEDEFED
typedef unsigned char Byte;
#endif /* BYTE_ALREADY_TYPEDEFED */

/***** 
* Time window in which button press & release trigger an anchor activation.
* (specified in milliseconds).
*****/
#define XmHTML_BUTTON_RELEASE_TIME			500		

/*****
* Default horizontal & vertical marginwidth.
*****/
#define XmHTML_DEFAULT_MARGIN				20

/*****
* Number of pixels to scroll when the scrollbar isn't being dragged but
* moved by either keyboard actions or arrow pressing. Be warned that
* using large values will cause a jumpy scrolling as XmHTML will have to
* render increasing amounts of data.
*****/
#define XmHTML_HORIZONTAL_SCROLL_INCREMENT	12		/* average char width */
#define XmHTML_VERTICAL_SCROLL_INCREMENT	18		/* average line height */

/*****
* Width of a horizontal tab (in spaces) for preformatted text.
*****/
#define XmHTML_DEFAULT_TABWIDTH				8

/*****
* Number of milliseconds that must elapse before the next scroll movement
* (through the keyboard interface) will be honored.
*****/
#define XmHTML_DEFAULT_REPEAT_DELAY			25

/*****
* Absolute maximum no of colors XmHTML may use. It is the maximum value for
* the XmNmaxImageColors resource.
* Increasing this number isn't a wise thing to do since all image routines
* are optimized for using paletted images. If you want XmHTML to handle more
* than 256 colors, you will have to modify the code.
*****/
#define XmHTML_MAX_IMAGE_COLORS				256

/*****
* Initial size of the Pixel hashtable. A large value will provide a sparse
* hashtable with few collisions. A small value will make the table compact
* but can lead to a (considerable) number of collisions.
* Calculating the optimal value is a tradeoff between speed and memory:
* in a sparse table, few collisions will occur but the memory consumption
* will be considerable (each entry occupies 28 bytes). A compact table will
* undoubtably lead to a number of collisions which has its impact on both
* storing and retrieving a value from the hashtable.
* Note: this setting is unused by Gtk/XmHTML, it uses hashing routines
* provided for by gtk.
*****/
#define XmHTML_COLORHASH_SIZE				1024 

/*****
* Default character set and fonts sets for proportional and fixed
* text.
*****/
#define XmHTML_DEFAULT_CHARSET 				"iso8859-1"

#define XmHTML_DEFAULT_PROPORTIONAL_FONT	"adobe-times-normal-*"

#define XmHTML_DEFAULT_FIXED_FONT			"adobe-courier-normal-*"

/*****
* Default font sizes
* Scalable size array: default,sub/superscript,h1,h2,h3,h4,h5,h6
* Fixed size array   : default,sub/superscript
*****/
#define XmHTML_DEFAULT_FONT_SCALABLE_SIZES	"14,8,24,18,14,12,10,8"
#define XmHTML_DEFAULT_FONT_FIXED_SIZES		"12,8"

/*****
* Default Table border width, cell & row spacing and padding.
* Default border width is only used if the border attribute is present
* but doesn't have a value.
* BTW: the rowspacing and rowpadding attributes are not part of the
* HTML spec.
*****/
#define XmHTML_DEFAULT_TABLE_BORDERWIDTH	1

#define XmHTML_DEFAULT_CELLSPACING			0

#define XmHTML_DEFAULT_ROWSPACING			0

#define XmHTML_DEFAULT_CELLPADDING			2

#define XmHTML_DEFAULT_ROWPADDING			2

/*****
* Maximum depth of nested lists and the default indentation for a single
* list item.
*****/
#define XmHTML_NESTED_LISTS_MAX				26
#define XmHTML_INDENT_SPACES				3

/*****
* Maximum number of iterations the text-justification routines may reach.
* Decreasing the default value of 1500 will lead to an increasing amount
* of warnings.
*****/
#define XmHTML_MAX_JUSTIFY_ITERATIONS		1500

/*****
* Maximum number of iterations the table layout computation routines may
* reach. This only occurs when the minimum suggested table width is smaller
* than the available width and the maximum suggested table width is larger
* than the available width. In this case, the layout routines have to compute
* an optimum balance between the different column widths within the available
* width. The algorithm used should be convergent, but could be divergent for
* nested tables or tables prefixed with an extreme indentation (nested lists
* and such). Hence the safeguard.
*****/
#define XmHTML_MAX_TABLE_ITERATIONS			128

/*****
* Default gamma correction value for your display. This is only used for
* images that support gamma correction (JPEG and PNG).
* 2.2 is a good assumption for almost every X display.
* For a Silicon Graphics displays, change this to 1.8
* For Macintosh displays (MkLinux), change this to 1.4 (so I've been told)
* If you change this value, it *must* be a floating point value.
* Note: this define is the default value for the XmNscreenGamma resource.
*****/
#define XmHTML_DEFAULT_GAMMA				2.2

/*****
* Maximum size of the PLC get_data() buffer. This is the maximum amount
* of data that will be requested to a function installed on the
* XmNprogressiveReadProc. Although this define can have any value, using
* a very small value will make progressive loading very slow, while using
* a large value will make the response of XmHTML slow while any PLC's are
* active.
* The first call to the get_data() routine will request PLC_MAX_BUFFER_SIZE
* bytes, while the size requested by any following calls will depend on the
* type of image being loaded and the amount of data left in the current input
* buffer.
*****/
#define PLC_MAX_BUFFER_SIZE					2048

/*****
* The default timeout value for the Progressive Loader Context. This
* timeout is the default value for the XmNprogressiveInitialDelay and
* specifies the polling interval between subsequent PLC calls.
* 
* Specified in milliseconds (1 second = 1000 milliseconds)
* XmHTML dynamically adjusts the timeout value as necessary and recomputes
* it after each PLC call.
* PLC_MIN_DELAY is the minimum value XmHTML can reduce the timeout to while
* PLC_MAX_DELAY is the maximum value XmHTML can increase the timeout to.
*****/
#define PLC_DEFAULT_DELAY					250
#define PLC_MIN_DELAY						5
#define PLC_MAX_DELAY						1000

/***************** End of User configurable section *****************/ 

/*****
* magic number for the XmHTMLImage structure. XmHTML uses this field to verify
* the return value from a user-installed primary image cache.
*****/
#define XmHTML_IMAGE_MAGIC		0xce

/*****
* Number of default icon entities. This number must equal the number
* of elements in the _XmHTMLIconEntities array in icons.h
*****/
#define NUM_ESCAPE_ICONS 61

/* lint kludge */
#ifdef lint
#undef True
#undef False
#define True ((Boolean)1)
#define False ((Boolean)0)
#endif /* lint */

/*****
* When X was written, noboby ever heard of C++, let alone what words
* would be reserved for this language, and as a result of this, a few
* structures in X contain words that could cause a problem when compiling
* XmHTML with a C++ compiler. 
*****/
#if defined(__cplusplus) || defined(c_plusplus)
#define MEMBER_CLASS	c_class
#define MEMBER_NEW		c_new
#define MEMBER_DELETE	c_delete
#else
#define MEMBER_CLASS	class
#define MEMBER_NEW		new
#define MEMBER_DELETE	delete
#endif

/*****
* None can be undefined if we aren't compiled for Xt/Motif.
*****/
#ifndef None
#define None		0
#endif
#define IdleKeep					False
#define IdleRemove					True
#define NullTimeout					None

/*****
* Sanity check: production is only allowed when both ndebug and _library
* have been defined.
*****/
#ifdef production
# ifndef NDEBUG
#  error You can not define production without defining NDEBUG.
# endif /* NDEBUG */
# ifndef _LIBRARY
#  error Configuration error: production defined without _LIBRARY defined.
# endif /* _LIBRARY */
#endif /* production */

/* Don't add anything after this endif! */
#endif /* _XmHTMLconf_h_ */

