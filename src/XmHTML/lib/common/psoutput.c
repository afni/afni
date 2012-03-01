#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* psoutput.c : XmHTML Postscript output routines
*
* This file Version	$Revision$
*
* Creation date:		Tue Nov 17 13:49:18 CET 1998
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Authors:				Scott Gregory, gregory@sccoast.net
*						Koen D'Hondt, ripley@xs4all.nl
*						Ameet A. Raval, aar@gfdl.gov
*						Frans van Hoesel, hoesel@chem.rug.nl 
*
* Gratefully donated to XmHTML by Scott Gregory.
*
* Copyright (C) 1994-1998 by Ripley Software Development 
* All Rights Reserved
*
* Portions Copyright by Ameet A. Raval & Frans van Hoesel.
* Portions Copyright by Robert C. Tatar & Craig A. McGowan.
* Portions Copyright by John Bradley.
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
* Scott's Note:
* 
* A hugely collective effort!
*
* The structure and portions of PS code uncerimoniously horked from code
* given to Mosaic by Ameet A. Raval & Frans van Hoesel. Their header reads:
*
* Institution: for Ameet A. Raval:
*					  Geophysical Fluid Dynamics Laboratory,
*					  National Oceanic and Atmospheric Administration,
*					  U.S. Department of Commerce
*					  P.O. Box 308
*					  Princeton, NJ 08542
*			  for Frans van Hoesel:
*					  Xtreme graphics software
*					  Herepoortenmolendrift 36
*					  9711 DH  Groningen
*					  The Netherlands
*
* Copyright
* This work is the product of the United States Government, and is precluded
* from copyright protection.  It is hereby released into the public domain.
*
* WE MAKE NO REPRESENTATIONS ABOUT THE SUITABILITY OF THIS SOFTWARE FOR
* ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED
* WARRANTY. WE SHALL NOT BE LIABLE FOR ANY DAMAGES SUFFERED BY THE
* USERS OF THIS SOFTWARE. 
*
* Pieces of code are taken from xvps by kind permission of John Bradley.
* 
* Extensive hacks from xwd2ps by Robert C. Tatar and Craig A. McGowan.
* 
*****/
/*****
* ChangeLog 
* $Log$
* Revision 1.2  2012/03/01 17:56:31  ziad
* Cput
*
* Revision 1.1  2011/06/30 16:10:38  rwcox
* Cadd
*
*****/ 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif

#include <stdlib.h>
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <stdio.h>		/* must follow stdarg or varargs on LynxOs */
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <math.h>

/* Our private header files */
#include "toolkit.h"
#include XmHTMLPrivateHeader
#include "XmHTMLfuncs.h"

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/
/* for regular-font, bold-font, italic-font, fixed-font */
typedef enum { 
	RF, BF, IF, FR, FB, FI 
}PSFontstyle;

/* structure for holding all footnotes on a page (anchor data) */
typedef struct{
	int nmalloc;
	int count;
	char **items;
}PSFootnote;

typedef struct _PSDisplay {
	Display *dpy;			/* must be the first field!				*/

	XmHTMLWidget html;		/* the XmHTML widget id					*/
	Byte options;			/* options to XmHTMLFormatPostscript()	*/

	/* Output area, Screen definition */
	XmHTMLPaperSize screen;	/* defined output area					*/
	float Points_Pixel;		/* Postscript Pointer Per Pixel			*/
	int Pixels_Page;		/* height of page in pixels				*/
	int Pixels_This_Page;	/* useable height of page in pixels		*/

	/* Output area, positioning */
	int start_y;			/* current vertical pixel position		*/
	int offset;
	int curr_page;			/* current page counter					*/

	/* Font data */
	XmHTMLfont *font;		/* latest font							*/
	char font_style[3];		/* PS font macro name, "RF", etc.		*/
	int font_size;			/* size of the font (ascent)			*/

	/* Output buffer */
	char *string;			/* Output buffer						*/
	int size;				/* size of output buffer				*/
	int len;				/* used size of output buffer			*/ 

	/* Footnotes */
	PSFootnote footnotes;

	/* Hexadecimal conversion */
	Byte hexline[80];		/* accumulation buffer					*/
	int hexi;				/* current position						*/

	/* document fore & background colors */
	unsigned short fg[3];	/* foreground color components			*/
	unsigned short bg[3];	/* background color components			*/

	/* Following fields are unused. Might be used later	*/
	int page_offset;		/* unused								*/
	PSFontstyle oldfn;		/* unused								*/
	int fontascent;			/* unused								*/
	int oldfs;				/* unused								*/
}PSDisplay;

/*** Private Function Prototype Declarations ****/
#define max(a,b)		(a>b ? a : b)
#define min(a,b)		(a<b ? a : b)

/* MONO returns total intensity of r,g,b components .33R+ .5G+ .17B */
#define MONO(rd,gn,bl) (((rd)*11 + (gn)*16 + (bl)*5) >> 13)

/* PSconst_out outputs to the postscript buffer an array of constant strings */
#define PSconst_out(dpy, txt) do{ \
	int i, n = (sizeof txt)/(sizeof txt[0]); \
	for(i = 0; i < n; i++) PSprintf(dpy, "%s\n", txt[i]); \
}while(0)

static float GetDpi(XmHTMLWidget hw);
static void fnDestroy(PSFootnote footnote);
static int fnAdd(PSFootnote footnote, char *buf);
static int PSprintf(PSDisplay *dpy, char *format, ...);
static void PSfootnotes(PSDisplay *dpy);
static void PSfont(PSDisplay *dpy, XmHTMLfont *font, Boolean flush);
static void PSwidgetsOnPage(PSDisplay *dpy);
static void PSshowpage(PSDisplay *dpy);
static void PSnewpage(PSDisplay *dpy);
static void PSinit_latin1(PSDisplay *dpy);
static void PSinit(PSDisplay *dpy);
static void PStrailer(PSDisplay *dpy);
static void PSfootnotes(PSDisplay *dpy);
static void PStext(PSDisplay *dpy, String t, Boolean underline);
static void PScheckPage(PSDisplay *dpy, int x, int y);
static void PSshowpage(PSDisplay *dpy);
static void PSmoveto(PSDisplay *dpy, int x, int y);
static void PSmove_offset(PSDisplay *dpy, int offset);
static int PSencode(unsigned char *data, unsigned char *rle, int size);
static void PSfootnotes(PSDisplay *dpy);
static int PShex(PSDisplay *dpy, Byte val, int flush);
static void PSColorImage(PSDisplay *dpy);
static void PScolormap(PSDisplay *dpy, Boolean color, int nc, 
	   Dimension *rmap, Dimension *gmap, Dimension *bmap);
static void PSrle_cmapimage(PSDisplay *dpy, int color);
static int PSImageBW(PSDisplay *dpy, Byte *data, int w, int h, Boolean inverse);
static void PSImage(PSDisplay *dpy, XmHTMLImage *image, int x, int y);
static void PSColorImage(PSDisplay *dpy);

/* Postscript TKA functions */
static ToolkitAbstraction *_CreatePostscriptTka(XmHTMLWidget html);

static int pstkSetForeground(Display *disp, XGC gc, unsigned long foreground);
static int pstkSetFont(Display *disp, XGC gc, XmHTMLfont *font);
static int pstkDrawString(Display *disp, DRAWABLE win,
	struct _XmHTMLFont *font, XGC gc, int x, int y, char *string, int length);
static void pstkDrawAnchorData(Display *disp, WINDOW win, XGC gc, int x,
	int y, XmHTMLObjectTableElement data);
static int pstkDrawLine(Display *disp, WINDOW win, XGC gc, int x1, int y1,
	int x2, int y2);
static int pstkDrawLines(Display *disp, DRAWABLE win, XGC gc, XPoint *points,
	int num_points, int mode);
static int pstkDrawRectangle(Display *disp, WINDOW win, XGC gc, int x, int y,
	unsigned int width, unsigned int height);
static int pstkFillRectangle(Display *disp, WINDOW win, XGC gc, int x, int y,
	unsigned int width, unsigned int height);
static int pstkDrawArc(Display *disp, WINDOW win, XGC gc, int x, int y,
	unsigned int width, unsigned int height, int angle1, int angle2);
static int pstkFillArc(Display *disp, WINDOW win, XGC gc, int x, int y,
	unsigned int width, unsigned int height, int angle1, int angle2);
static void pstkDrawImage(XmHTMLWidget html, XmHTMLImage *image, XGC gc,
	int src_x, int src_y, unsigned int width, unsigned int height,
	int dest_x, int dest_y);
static void pstkDrawShadows(Display *disp, DRAWABLE drawable, 
	XGC top_shadow_GC, XGC bottom_shadow_GC,
#if NeedWidePrototypes
	int x, int y, int width, int height, int shadow_thickness,
#else
	Position x, Position y, Dimension width, Dimension height,
	Dimension shadow_thickness,
#endif
	unsigned int shadow_type);

/* empty wrappers */


/*** Private Variable Declarations ***/

#define CR '\015'
#define LF '\012'

#define TOP_MARGIN		dpy->screen.top_margin
#define BOT_MARGIN		dpy->screen.bottom_margin
#define LEFT_MARGIN		dpy->screen.left_margin
#define PAGE_HEIGHT		dpy->screen.height
#define PAGE_WIDTH		dpy->screen.width

#define F_FULLCOLOR		0
#define F_GREYSCALE		1
#define F_BWDITHER		2
#define F_REDUCED		3

#define L_PAREN			'('
#define R_PAREN			')'
#define B_SLASH			'\\'
#define MAX_ASCII		'\177'

#define FOOTNOTE_ROW_HEIGHT	8		/* helvetica 8 */

/*****
* Name:			GetDpi
* Return Type: 	float
* Description: 	returns dots-per-inch of the screen. Calculates the pixel
*				density in dots per inch on the current Widget screen.
* In: 
*	html:		XmHTMLWidget id
* Returns:
*	screen density.
*****/
static float
GetDpi(XmHTMLWidget html)
{
	ToolkitAbstraction *tka = HTML_ATTR(tka);
	float dpi;

	dpi = 25.4 * tka->width/tka->widthMM;

	/* no earthly monitor does this */
	if(dpi < 1.0 || dpi > 10000.0)
		dpi = 72.0;
	return dpi;
}

/**********
***** Footnote functions. If requested, XmHTML prints the referring hyperlink
***** of each anchor as a footnote.
**********/

/*****
* Name:			fnDestroy
* Return Type: 	void
* Description: 	destroy all footnote data for this page.
* In: 
*	footnote:	array of footnotes to be destroyed.
* Returns:
*	nothing.
*****/
static void
fnDestroy(PSFootnote footnote)
{
	int i;

	if(footnote.items)
	{
		for(i = 0; footnote.items[i] != NULL; ++i)
			free(footnote.items[i]);
		free(footnote.items);
		footnote.items = NULL;
	}
	footnote.count = footnote.nmalloc = 0;
}

/*****
* Name: 		fnAdd
* Return Type: 	int
* Description:	add a footnote to the current page and return its number
* In: 
*	buf:		footnote value
* Returns:
*	footnote number.
*****/
static int
fnAdd(PSFootnote footnote, char *buf)
{
	int fnum;

	if(!footnote.items)
	{
		/* initialize current page footnotes buffer */
		footnote.nmalloc = 10;
		footnote.count = 0;
		footnote.items = (char **)malloc(sizeof(char *)*footnote.nmalloc);
		memset(footnote.items, 0, sizeof(char *)*footnote.nmalloc);
	}
	else
	{
		/*****
		* We've already got a buffer, see if it's large enough and extend
		* if required.
		*****/
		if(footnote.count >= (footnote.nmalloc-1))
		{
			footnote.nmalloc += 10;
			footnote.items = (char **)realloc(footnote.items,
								sizeof(char *)*footnote.nmalloc);
		}
	}

	/* Check if this footnote has already been added */
	for(fnum = 0; fnum < footnote.count; ++fnum)
		if(strcmp(footnote.items[fnum], buf) == 0)
			return(fnum);
			
	/* this is a new footnote, store it */
	fnum = footnote.count;
	footnote.count++;

	footnote.items[fnum] = strdup(buf);
	footnote.items[fnum+1] = NULL;

	return(fnum);
}

/*****
* Name: 		PSprintf
* Return Type: 	int
* Description: 	sprintf but can dynamically extend the destination buffer.
* In: 
*
* Returns:
*
*****/
static int
#ifdef __STDC__
PSprintf(PSDisplay *dpy, char *format, ...)
#else
PSprintf(dpy, format, va_alist)
	PSDisplay *dpy;
	String format;
	va_dcl
#endif
{
	int len;
	char *s;
#ifdef __STDC__
	va_list args;
#else
	va_alist args
#endif

	if(dpy->size - dpy->len < 1024)
	{
		dpy->size += 1024;
		s = (char*)realloc(dpy->string, dpy->size);
		dpy->string = s;
	}
#ifdef __STDC__
	va_start(args, format);
#else
	va_start(args);
#endif
	len = vsprintf(dpy->string + dpy->len, format, args);

	va_end(args);

	/* update size of destination buffer */
	if(len != 0)
		dpy->len += strlen(dpy->string + dpy->len);

	/* all done */
	return(len);
}

/*****
* Name: 		PSfootnotes
* Return Type: 	void
* Description: Display the footer and all footnotes collected during this page
* In: 
*	dpy:		current postscript output area
* Returns:
*	nothing.
*****/
static void
PSfootnotes(PSDisplay *dpy)
{
	int x, y, i;

	if(!(dpy->options & XmHTMLTEXT_ADDFOOTER))
		return;

	x = dpy->screen.left_margin;
	y = dpy->Pixels_This_Page;

	PSprintf(dpy, "%% PSfootnotes\n");
	PSprintf(dpy, "0 setgray\n");

	PSprintf(dpy, "%d -%d M %d 0 RL stroke\n", x, y,
		dpy->screen.width - dpy->screen.left_margin -
		dpy->screen.right_margin);

	/* set font */
	PSprintf(dpy, "\n/helvetica-bold %d SF\n", FOOTNOTE_ROW_HEIGHT);

	/* show page number */
	PSprintf(dpy, "newpath %d -%d M 0 -%d RL ( Page %d ) stringwidth "
		"pop neg 0 RL 0 %d RL closepath stroke\n",
		dpy->screen.width - dpy->screen.right_margin, y,
		FOOTNOTE_ROW_HEIGHT + 2, dpy->curr_page, FOOTNOTE_ROW_HEIGHT+2);
	PSprintf(dpy, "%d -%d M ( Page %d ) stringwidth pop neg -%d R "
		"(Page %d ) S\n",
		dpy->screen.width - dpy->screen.right_margin, y,
		dpy->curr_page, FOOTNOTE_ROW_HEIGHT, dpy->curr_page);

	/* print the footnotes if requested */
	if(!(dpy->options & XmHTMLTEXT_ANCHORFOOTNOTES) ||
		dpy->footnotes.count <= 0)
	{
		fnDestroy(dpy->footnotes);
		return;
	}
	for(i = 0; dpy->footnotes.items[i] != NULL; ++i)
	{
		y += 2 + FOOTNOTE_ROW_HEIGHT;

		/* set footnote number font */
		PSprintf(dpy, "/helvetica-bold %d SF\n", FOOTNOTE_ROW_HEIGHT);

		/* footnote number */
		PSprintf(dpy, "%d -%d M (%d. )S\n", x, y, i+1);

		/* set footnote content font */
		PSprintf(dpy, "/helvetica %d SF\n", FOOTNOTE_ROW_HEIGHT);

		/* footnote contents */
		PSprintf(dpy, "(%s)S\n", dpy->footnotes.items[i]);
	}

	/* all done */
	fnDestroy(dpy->footnotes);
}

/*****
* Name:			PSfont
* Return Type: 	void
* Description: 	change local font.
* In: 
*	dpy:		current postscript output area;
*	font:		master font;
*	flush:		flush font settings at end of page.
* Returns:
*	nothing.
* Note:
*	This needs some work. Ultimatly, Postscript output should use the
*	specified document fonts instead of referring to a set of default
*	fonts. This will require a lot of work as it requires XmHTML to include
*	font definitions for each font that is used in this document.
*	Ideally, the Postscript ToolkitAbstraction should have an associated
*	font cache instead of basing font selection on the fonts residing
*	in the X server.
*****/
static void
PSfont(PSDisplay *dpy, XmHTMLfont *font, Boolean flush)
{
	static XmHTMLfont *last_font=NULL;
	char fstyle[3];
	static char fstr[25]="\0";
	int i;

	/* no font change */
	if(font == last_font && font != NULL)
		return;

	/* Forced font flush or no font provided */
	if(flush || font == NULL)
	{
		/* First time entry, set initial size */
		if(last_font == NULL || fstr[0]=='\0')
		{
			PSprintf(dpy, "RF 14 SF\n");
			return;
		}
		else
		{
			/* use current font */
			PSprintf(dpy, "%s\n", fstr);
			return;
		}
	}

	/* initialize font style array */
	memset(fstyle, 0, sizeof(fstyle));

	if(font->style & FONT_SCALABLE || strstr(font->font_family, "times"))
	{
		/* scalable font */
		fstyle[1]='F';
		i = 0;
	}
	else
	{
		/* fixed font */
		fstyle[0]='F';
		i = 1;
	}

	/* set font styles */
	if(font->style & FONT_BOLD)
		fstyle[i]='B';
	else if(font->style & FONT_MEDIUM)
		fstyle[i] = 'R';
	else if(font->style & FONT_ITALIC)
		fstyle[i] = 'I';
	else
		fstyle[i] = 'R';

	/* comment stating original fontname */
	PSprintf(dpy, "%%FontStyle=0x%x %s, size = %i points\n", (int)font->style,
		font->font_name, font->ptsize);

	/* store & set size */
	sprintf(fstr, "%s %d SF", fstyle, font->ptsize);
	PSprintf(dpy, "%s\n", fstr);

	/* store font data */
	dpy->font = font;
	strcpy(dpy->font_style, fstyle);
	dpy->font_size = font->m_ascent;

	if(font)
		last_font = font;
}

/*****
* Name:			PSwidgetsOnPage
* Return Type: 	void
* Description: 	render all HTML FORM elements residing on the current page.
* In: 
*	dpy:		current Postscript output area
* Returns:
*	nothing.
* Note:
*	Called at end of a page to draw the form widgets.
*	
*	Koen,
*		for this to work correctly, each widget would have had to have
*	been mapped at one time to get its formatted position. Then in here
*	we would have to map those that aren't, wait on the expose, then
*	grab the image and unmap it.
*	
*	Kind of academic until we can ensure all form widgets have been mapped.
*
*	Scott,
*		Since we can't rely on each widget being mapped (it's very well
*		possible the XmHTMLWidget will get mapped at all: think about a
*		HTML to Postscript converter), we just draw a grayed rectangle.
*		Later on we could decide to do it another way, say map the
*		widget to an offscreen position, take a snapshot of that and render
*		the resulting image. Come to think of it, that would be the way
*		to do it.
*****/
static void
PSwidgetsOnPage(PSDisplay *dpy)
{
	XmHTMLWidget html = dpy->html;
#if 0
	ToolkitAbstraction *tka = HTML_ATTR(tka);
#endif
	XmHTMLFormData *form;
	XmHTMLForm *entry;
	int xs, ys;

	/* XmHTMLExtObj *u; ??Should we print these also?? Not implemented yet */
	
	if((form = HTML_ATTR(form_data)) == NULL)
		return;
		
	/* walk all defined forms */
	for(; form != NULL; form = form->next)
	{
		/* check all form components */
		for(entry = form->components; entry != NULL; entry = entry->next)
		{
			/* only render visible form members */
			if(entry->w)
			{
				/*****
				* get widget position on current page
				* data->x and data->y contain the computed widget's position
				* relative to the start of the document, so to compute
				* the widget position relative to the current page we must
				* substract the region that's already behind us.
				*****/
				xs = entry->data->x - HTML_ATTR(scroll_x);
				ys = entry->data->y - HTML_ATTR(scroll_y);

				/* check if this widget is in the viewable area */
				if(xs + entry->width > 0 && xs < HTML_ATTR(work_width) &&
					ys + entry->height > 0 && ys < HTML_ATTR(work_height))
				{
					PSprintf(dpy, "%% PSwidgetsOnPage %s (%dx%d+%d+%d)\n",
						TkaWidgetName(entry->w), entry->data->width,
						entry->data->height, xs, ys);

					/* render a grayed rectangle */
#if 1
					/* Positioning might be wrong... */

					PSprintf(dpy, "%d %d translate", xs,
						-(ys - dpy->start_y) - entry->data->height);
					PSprintf(dpy, "gsave currentpoint %d sub translate ",
						entry->data->height);
					PSprintf(dpy, "%d %d scale\n", entry->data->width,
						entry->data->height);
					PSprintf(dpy, "SQ 0.9 setgray fill\ngrestore\n");
#else
					if(tka->IsRealized(entry->w) && entry->mapped)
						ImageToPs(dpy, XtDisplay((Widget)html),
							XtWindow(c->w), 0, 0, c->width, c->height,
							xs, -(ys - dpy->start_y), 1);
#endif
				}
			}
		}
	}
}

/*****
* Name:			PSshowpage
* Return Type: 	end of page function. Show current page and restore
*				any changes to the printer state.
* Description: 
* In: 
*	dpy:		current postscript output area.
* Returns:
*	nothing.
*****/
static void
PSshowpage(PSDisplay *dpy)
{
	PSwidgetsOnPage(dpy);
	
	if(dpy->curr_page > 0)
		PSfootnotes(dpy);

	dpy->Pixels_This_Page = dpy->Pixels_Page;

	PSprintf(dpy, "showpage restore\n");
}

/*****
* Name: 		PSnewpage
* Return Type: 	begin a fresh page. Increments page count and handles
*				structured comment conventions.
* Description: 
* In: 
*	dpy:		current postscript output area.
* Returns:
*	nothing.
*****/
static void
PSnewpage(PSDisplay *dpy)
{
	dpy->curr_page++;

	/*****
	* The PostScript reference Manual states that the Page: Tag
	* should have a label and a ordinal; otherwise programs like
	* psutils fail -- gustaf
	*****/
	PSprintf(dpy, "%%%%Page: %d %d\n", dpy->curr_page, dpy->curr_page);
	PSprintf(dpy, "save\nNP\n");
	PSfont(dpy, (Font)0, True); /* force re-flush of last font used */

	/* reserve footnote space (if requested) */
	if(dpy->options & XmHTMLTEXT_ADDFOOTER)
		dpy->Pixels_This_Page -= FOOTNOTE_ROW_HEIGHT;
}

/*****
* Name:			PSinit_latin1
* Return Type: 	void
* Description: 	handle ISO Latin1 encoding.
* In: 
*	dpy:		current postscript output area
* Returns:
*	nothing.
* Note:
*	This table contains the names of all characters in the ISO Latin1 font
*	encoding (191 defined characters in total). The first valid character
*	is space.
*	This table comes from the Idraw program (from Stanford's InterViews 
*	package), courtesy of Steinar Kjaernsrd, steinar@ifi.uio.no
*****/
static void
PSinit_latin1(PSDisplay *dpy)
{
	static char *txt[] = {
		"/reencodeISO {",
		"dup dup findfont dup length dict begin",
		"{ 1 index /FID ne { def }{ pop pop } ifelse } forall",
		"/Encoding ISOLatin1Encoding D",
		"currentdict end definefont",
		"} D",
		"/ISOLatin1Encoding [",
		"/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef",
		"/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef",
		"/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef",
		"/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef",
		"/space/exclam/quotedbl/numbersign/dollar/percent/ampersand/quoteright",
		"/parenleft/parenright/asterisk/plus/comma/minus/period/slash",
		"/zero/one/two/three/four/five/six/seven/eight/nine/colon/semicolon",
		"/less/equal/greater/question/at/A/B/C/D/E/F/G/H/I/J/K/L/M/N",
		"/O/P/Q/R/S/T/U/V/W/X/Y/Z/bracketleft/backslash/bracketright",
		"/asciicircum/underscore/quoteleft/a/b/c/d/e/f/g/h/i/j/k/l/m",
		"/n/o/p/q/r/s/t/u/v/w/x/y/z/braceleft/bar/braceright/asciitilde",
		"/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef",
		"/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef",
		"/.notdef/dotlessi/grave/acute/circumflex/tilde/macron/breve",
		"/dotaccent/dieresis/.notdef/ring/cedilla/.notdef/hungarumlaut",
		"/ogonek/caron/space/exclamdown/cent/sterling/currency/yen/brokenbar",
		"/section/dieresis/copyright/ordfeminine/guillemotleft/logicalnot",
		"/hyphen/registered/macron/degree/plusminus/twosuperior/threesuperior",
		"/acute/mu/paragraph/periodcentered/cedilla/onesuperior/ordmasculine",
		"/guillemotright/onequarter/onehalf/threequarters/questiondown",
		"/Agrave/Aacute/Acircumflex/Atilde/Adieresis/Aring/AE/Ccedilla",
		"/Egrave/Eacute/Ecircumflex/Edieresis/Igrave/Iacute/Icircumflex",
		"/Idieresis/Eth/Ntilde/Ograve/Oacute/Ocircumflex/Otilde/Odieresis",
		"/multiply/Oslash/Ugrave/Uacute/Ucircumflex/Udieresis/Yacute",
		"/Thorn/germandbls/agrave/aacute/acircumflex/atilde/adieresis",
		"/aring/ae/ccedilla/egrave/eacute/ecircumflex/edieresis/igrave",
		"/iacute/icircumflex/idieresis/eth/ntilde/ograve/oacute/ocircumflex",
		"/otilde/odieresis/divide/oslash/ugrave/uacute/ucircumflex/udieresis",
		"/yacute/thorn/ydieresis",
		"] D",
		"[RF BF IF FR FB FI] {reencodeISO D} forall"
	};

	PSconst_out(dpy, txt);
}

/*****
* Name:			PSinit
* Return Type: 	void
* Description: 	initialize postscript output per HTML document.
* In: 
*	dpy:		current postscript output area
* Returns:
*	nothing.
*****/
static void
PSinit(PSDisplay *dpy)
{
	dpy->offset = dpy->hexi = dpy->page_offset = 0;
	dpy->start_y = 0;

	/* Initialize output buffer */
	dpy->size = 1024;
	dpy->string = (char*)calloc(1, sizeof(char)*dpy->size);
	dpy->len = 0;

	dpy->oldfs = 0;
	dpy->oldfn = RF;
	dpy->curr_page = 0 ;
}

/*****
* Name: 		PSheader
* Return Type: 	void
* Description: 	initialize postscript output and prints the postscript prolog.
* In: 
*	dpy:		current postscript output area
*	title:		document title (if any)
*	font:		document basefont.
* Returns:
*	nothing.
*****/
static void
PSheader(PSDisplay *dpy, char *title, int font)
{
	time_t currtime = time(NULL);

	static char *fontname[] = {
		/* in order: regular, bold, italic */
		"Times-Roman", "Times-Bold", "Times-Italic",
		"Helvetica", "Helvetica-Bold", "Helvetica-Oblique",
		"NewCenturySchlbk-Roman", "NewCenturySchlbk-Bold",
				"NewCenturySchlbk-Italic",
		/* this is a nasty trick, I have put Times in place of
		 * Lucida, because most printers don't have Lucida font
		 */
		"Times-Roman", "Times-Bold", "Times-Italic"
		/* "Lucida", "Lucida-Bold", "Lucida-Italic" */
	};

	static char *txt[] = {
		"%%EndComments",
		"save",
		"/D {def} def /E {exch} D",
		"/M {moveto} D",
		"/S {show} D",
		"/R {rmoveto} D",
		"/L {lineto} D",
		"/RL {rlineto} D",
		"/SQ {newpath 0 0 M 0 1 L 1 1 L 1 0 L closepath} D",
		"/U {gsave currentpoint currentfont /FontInfo get /UnderlinePosition get",
		" 0 E currentfont /FontMatrix get dtransform E pop add newpath moveto",
		" dup stringwidth rlineto stroke grestore S } D",
		"/B {/r E D gsave -13 0  R currentpoint ",
		"  newpath r 0 360 arc closepath fill grestore } D",
		"/OB {/r E D gsave -13 0  R currentpoint ",
		"  newpath r 0 360 arc closepath stroke grestore } D",
		"/NP {xmargin topmargin translate scalfac dup scale newpath } D",
		"/HR {/l E D gsave l 0 RL  stroke grestore } D",
		"/SF {E findfont E scalefont setfont } D",
		"/FR {/Courier } D",
		"/FB {/Courier-Bold } D",
		"/FI {/Courier-Oblique } D"
	};

	/* Postscript level */
	PSprintf(dpy, "%%!PS-Adobe-1.0\n");
	PSprintf(dpy,"%%%%Creator: %s\n", XmHTMLVERSION_STRING);

	/* set document title (if any) */
	if(title != NULL && *title != '\0')
	{
		char *tmp;
		for(tmp = title; *tmp; tmp++)
		{
			/* convert newlines to spaces */
			if(*tmp == CR || *tmp == LF)
				*tmp = ' ';
		}
		PSprintf(dpy, "%%%%Title: %s\n", title);
	}
	else
		PSprintf(dpy, "%%%%Title: (no title)\n");

	/* print remainder of header */

	/* creation data */
	PSprintf(dpy, "%%%%CreationData: %s", ctime(&currtime));

	/* tell no of pages is at the end of the document */
	PSprintf(dpy, "%%%%Pages: (atend)\n");
	PSprintf(dpy, "%%%%PageOrder: Ascend\n");

	/* set document fonts */
	PSprintf(dpy, "%%%%DocumentFonts: %s %s %s Courier Courier-Bold "
		"Courier-Oblique\n", fontname[font*3], fontname[font*3+1],
		fontname[font*3+2]);

	/* print remainder of prolog */
	PSconst_out(dpy, txt);

	PSprintf(dpy, "/RF {/%s} D\n", fontname[font*3]);
	PSprintf(dpy, "/BF {/%s} D\n", fontname[font*3+1]);
	PSprintf(dpy, "/IF {/%s} D\n", fontname[font*3+2]);

	PSinit_latin1(dpy);

	PSprintf(dpy, "/xmargin %d D\n", (int)LEFT_MARGIN);
	PSprintf(dpy, "/topmargin %d D\n", (int)TOP_MARGIN);
	PSprintf(dpy, "/scalfac %.5f D\n", dpy->Points_Pixel);
	PSprintf(dpy, "%%%%EndProlog\n");
}

/*****
* Name:			PStrailer
* Return Type: 	void
* Description: 	write document trailer.
* In: 
*	dpy:		current postscript output area
* Returns:
*	nothing.
*****/
static void
PStrailer(PSDisplay *dpy)
{
	PSprintf(dpy, "%%%%Trailer\n");
	PSprintf(dpy, "restore\n");
	PSprintf(dpy, "%%%%Pages: %d\n", dpy->curr_page);
}

/*****
* Name: 		PStext
* Return Type: 	void
* Description: 	output text, renders the given text, protects special
*				characters and underlines words if required.
* In: 
*	dpy:		current postscript output area
*	t:			text to be drawn
*	underline:	if non-zero, text is drawn underlined.
* Returns:
*	nothing.
*****/
static void
PStext(PSDisplay *dpy, String t, Boolean underline)
{
	String chPtr, t2;					/* temporary buffers */
	int nspecial = 0, nisochar = 0;		/* special character counters */

	chPtr = t;

	/* Count number of special characters in the given text */
	while(*chPtr != '\0')
	{
		if(*chPtr == L_PAREN || *chPtr == R_PAREN || *chPtr == B_SLASH)
			nspecial++;
		else if(*(Byte *)chPtr > (Byte )MAX_ASCII)
			nisochar++;
		chPtr++;
	}

	if(nspecial == 0 && nisochar == 0)
	{
		/* no special characters found, display as is */
		PSprintf(dpy, "(%s)%c\n", t, (underline) ? 'U' : 'S');
		return;
	}

	/*****
	* Special characters found. Create a new output buffer that will
	* contain the original text and any special characters that need to
	* be protected.
	*
	* We don't need to check the return value: all memory allocation
	* routines are actually defined as macros to functions that perform
	* return value checking.
	*****/

	t2 = (String)malloc((chPtr - t) + nspecial + 3*nisochar + 1);

	/*****
	* For each character in the provided text, check if it is a special char.
	* If we have a special, escape it by inserting a backslash into t2, then
	* insert the actual char. Ordinary characters are copied as is.
	*****/
	chPtr = t2;
	while(*t != '\0')
	{
		if(*t == L_PAREN || *t == R_PAREN || *t == B_SLASH)
		{
			/* parenthesis and backslash must be escaped */
			*(chPtr++) = B_SLASH;
			*(chPtr++) = *t;
		}
		else
		{
			/* non-ascii character, escape and convert to octal */
			if(*(Byte*)t > (Byte)MAX_ASCII)
			{
				/*  convert to octal */
				*(chPtr++) = B_SLASH;
				*(chPtr++) = ((int)(*(Byte*)t)>>6 & 007) + '0';
				*(chPtr++) = ((int)(*(Byte*)t)>>3 & 007) + '0';
				*(chPtr++) = ((int)(*(Byte*)t) & 007) + '0';
			}
			else
				*(chPtr++) = *t;
		}

		t++;
	}
	/* terminate */
	*(chPtr) = '\0';

	/* output */
	PSprintf(dpy, "(%s)%c\n", t2, (underline) ? 'U' : 'S');

	/* all done */
	free(t2);
}

/*****
* Name:			PScheckPage
* Return Type: 	void
* Description: 	check if a new new page should be started.
* In: 
*	dpy:		current postscript output area
*	x:			current horizontal pixel position
*	y:			current vertical pixel position
* Returns:
*	nothing
*****/
static void
PScheckPage(PSDisplay *dpy, int x, int y)
{
	if(y > dpy->start_y + dpy->Pixels_This_Page)
	{
		PSshowpage(dpy);
		dpy->start_y = y;
		PSnewpage(dpy);
	}
	dpy->offset = 0;
}

/*****
* Name: 		PSmoveto
* Return Type: 	void
* Description: 	move to output vector to a new x,y location. If the given
*				y value does not fit within the current page, start a new
*				page.
* In: 
*	dpy:		current postscript output area
*	x:			current horizontal pixel position
*	y:			current vertical pixel position
* Returns:
*	nothing.
*****/
static void
PSmoveto(PSDisplay *dpy, int x, int y)
{
	PScheckPage(dpy, x, y);
	dpy->offset = 0;
	PSprintf(dpy, "%d %d M\n", x, -(y - dpy->start_y));
}

/*****
* Name: 		PSmove_offset
* Return Type: 	void
* Description: 	set y-offset. Performs a relative vertical move whenever
*				the offset changes.
* In: 
*	dpy:		current postscript output area
*	offset:		vertical (pixel) offset.
* Returns:
*	nothing.
*****/
static void
PSmove_offset(PSDisplay *dpy, int offset)
{
	if(offset != dpy->offset)
	{
		PSprintf(dpy, "0 %d R\n", dpy->offset - offset );
		dpy->offset = offset;
	}
}

/*****************************************************************************
****************** Image & Color conversion Routines *************************
*****************************************************************************/

/*****
* Name:			PScolormap
* Return Type: 	void
* Description: 	writes a colormap. Produces code for the colormap of
*				any images following. If this is a grayscale image, a
*				grayscale colormap is produced.
* In: 
*	dpy:		current postscript output area
*	color:		indicates color or grayscale.
*	nc:			number of colors
*	rmap:		red color components;
*	gmap:		green color components;
*	bmap:		blue color components;
* Returns:
*	nothing.
*****/
static void
PScolormap(PSDisplay *dpy, Boolean color, int nc,
	Dimension *rmap, Dimension *gmap, Dimension *bmap)
{
	int i;

	/* define the colormap */
	PSprintf(dpy, "/cmap %d string def\n\n\n", nc * ((color) ? 3 : 1));

	/* load it */
	PSprintf(dpy, "currentfile cmap readhexstring\n");

	/* write out each color */
	for(i = 0; i < nc; i++)
	{
		if(color)	/* downscale to 0-255 */
			PSprintf(dpy, "%02x%02x%02x ", rmap[i]>>8, gmap[i]>>8, bmap[i]>>8);
		else 
			PSprintf(dpy, "%02x ", MONO(rmap[i], gmap[i], bmap[i]));

		/* make it look nice */
		if((i % 10) == 9)
			PSprintf(dpy, "\n");
	}
	PSprintf(dpy, "\n");
	PSprintf(dpy, "pop pop\n"); /* lose return values from readhexstring */
}

/*****
* Name:			PSencode
* Return Type: 	int
* Description: 	perform run length encoding.
*				RLE is used to reduce file size (and thus reduce the time
*				required to send it to the printer). The disadvantage is
*				increased processing time.
* In: 
*	data:		data to be rle'd
*	rle:		return buffer.
*	size:		size of data.
* Returns:
*	size of return buffer.
* Note:
*	rle is encoded as such:
*	<count> <value>			# 'run' of count+1 equal pixels
*	<count | 0x80> <count+1 data bytes>	# count+1 non-equal pixels
*	count can range between 0 and 127
*****/
static int
PSencode(unsigned char *data, unsigned char *rle, int size) 
{
    int i, j, blocklen, isrun, rlen;
    unsigned char block[256], pix;

    blocklen = isrun = rlen = 0;

    for(i = 0; i < size; i++) 
    {
		/*****
		* 5 possible states:
		*   0: block empty.
		*   1: block is a run, current pix == previous pix
		*   2: block is a run, current pix != previous pix
		*   3: block not a run, current pix == previous pix
		*   4: block not a run, current pix != previous pix
		*****/

		pix = data[i];

		if(!blocklen) 
		{
		    /* case 0:  empty */
		    block[blocklen++] = pix;
		    isrun = 1;
		}
		else if(isrun) 
		{
		    if(pix == block[blocklen-1])
		    { 
				/*  case 1:  isrun, prev==cur */
				block[blocklen++] = pix;
		    }
		    else 
		    {
				/*  case 2:  isrun, prev!=cur */
				if(blocklen>1) 
				{
				    /*  we have a run block to flush */
					rle[rlen++] = blocklen-1;
					rle[rlen++] = block[0];
					/*  start new run block with pix */
					block[0] = pix;
					blocklen = 1;
				}
				else
				{
					/*  blocklen<=1, turn into non-run */
					isrun = 0;
					block[blocklen++] = pix;
				}
			}
		}
		else
		{ 
			/* not a run */
			if(pix == block[blocklen-1]) 
			{
				/* case 3: non-run, prev==cur */
				if(blocklen>1) 
				{
					/*  have a non-run block to flush */
					rle[rlen++] = (blocklen-1) | 0x80;
					for(j = 0; j < blocklen; j++)
						rle[rlen++] = block[j];
					/*  start new run block with pix */
					block[0] = pix;
					blocklen = isrun = 1;
				} 
				else
				{
					/*  blocklen<=1 turn into a run */
					isrun = 1;
					block[blocklen++] = pix;
				}
			}
			else 
			{
				/* case 4:  non-run, prev!=cur */
				block[blocklen++] = pix;
			}
		}
	
		/* max block length.  flush */
		if(blocklen == 128)
		{
			if(isrun) 
			{
				rle[rlen++] = blocklen-1;
				rle[rlen++] = block[0];
			}
			else
			{
				rle[rlen++] = (blocklen-1) | 0x80;
				for(j = 0; j < blocklen; j++)
					rle[rlen++] = block[j];
			}
			blocklen = 0;
		}
	}

	/* flush last block */
	if(blocklen)
	{
		if(isrun)
		{
			rle[rlen++] = blocklen-1;
			rle[rlen++] = block[0];
		}
		else
		{
			rle[rlen++] = (blocklen-1) | 0x80;
			for(j = 0; j < blocklen; j++)
				rle[rlen++] = block[j];
		}
	}
	return(rlen);
}

/*****
* Name: 		PShex
* Return Type: 	output a hexadecimal value.
* Description: 
* In: 
*	dpy:		current postscript output area
*	val:		value to be converted
*	flush:		if True, flush out the current buffer.
* Returns:
*	EOF on failure, something else otherwise.
*****/
static int 
PShex(PSDisplay *dpy, Byte val, int flush)
{
	static char digit[] = "0123456789abcdef";

	/* convert to hexadecimal and collect */
	if(!flush) 
	{
		dpy->hexline[dpy->hexi++] = (char) digit[((unsigned) val >>
			(unsigned) 4) & (unsigned) 0x0f];
		dpy->hexline[dpy->hexi++] = (char) digit[(unsigned) val &
			(unsigned) 0x0f];
	}

	/* flush requested or buffer full */
	if((flush && dpy->hexi) || (dpy->hexi > 77)) 
	{
		dpy->hexline[dpy->hexi] = '\0';
		dpy->hexi = 0;
		return(PSprintf(dpy, "%s\n", dpy->hexline));
	}
	return(0);
}

/*****
* Name: 		PSColorImage
* Return Type:	void
* Description:	created postscript colorimage operator 
*				Adds code that checks if the PostScript device in question
*				knows about the 'colorimage' operator.  If it doesn't, it
*				defines 'colorimage' in terms of image (ie, generates a
*				greyscale image from RGB data)
* In: 
*	dpy:		current postscript output area
* Returns:
*	nothing.
*****/
static void 
PSColorImage(PSDisplay *dpy) 
{
    static char *txt[] = {
	"% define 'colorimage' if it isn't defined",
	"%   ('colortogray' and 'mergeprocs' come from xwd2ps",
	"%	 via xgrab)",
	"/colorimage where   % do we know about 'colorimage'?",
	"  { pop }		   % yes: pop off the 'dict' returned",
	"  {				 % no:  define one",
	"	/colortogray {  % define an RGB->I function",
	"	  /rgbdata exch store	% call input 'rgbdata'",
	"	  rgbdata length 3 idiv",
	"	  /npixls exch store",
	"	  /rgbindx 0 store",
	"	  /grays npixls string store  % str to hold the result",
	"	  0 1 npixls 1 sub {",
	"		grays exch",
	"		rgbdata rgbindx	   get 20 mul	% Red",
	"		rgbdata rgbindx 1 add get 32 mul	% Green",
	"		rgbdata rgbindx 2 add get 12 mul	% Blue",
	"		add add 64 idiv	  % I = .5G + .31R + .18B",
	"		put",
	"		/rgbindx rgbindx 3 add store",
	"	  } for",
	"	  grays",
	"	} bind def\n",
	/* Utility procedure for colorimage operator.
	 * This procedure takes two procedures off the
	 * stack and merges them into a single procedure
	 */
	"	/mergeprocs { % def",
	"	  dup length",
	"	  3 -1 roll dup length dup 5 1 roll",
	"	  3 -1 roll add array cvx dup",
	"	  3 -1 roll 0 exch putinterval",
	"	  dup 4 2 roll putinterval",
	"	} bind def\n",
	"	/colorimage { % def",
	/* remove 'false 3' operands */
	"	  pop pop",
	"	  {colortogray} mergeprocs",
	"	  image",
	"	} bind def",
	/* end of 'false' case */
	"  } ifelse"
    };

    PSconst_out(dpy, txt);
}

/*****
* Name:			PSrle_cmapimage
* Return Type: 	void
* Description: 	define rlecmapimage operator
* In: 
*	dpy:		current postscript output area
*	color:		indicates color or mono.
* Returns:
*	nothing.
*****/
static void
PSrle_cmapimage(PSDisplay *dpy, int color) 
{
	/* prolog */
	static char *txt[] = {
		/* rlecmapimage expects to have 'w h bits matrix' on stack */
		"/rlecmapimage {",
		"  /buffer 1 string def",
		"  /rgbval 3 string def",
		"  /block  384 string def",
		"  { currentfile buffer readhexstring pop",
		"	/bcount exch 0 get store",
		"	bcount 128 ge",
		"	{ ",
		"	  0 1 bcount 128 sub",
		"	{ currentfile buffer readhexstring pop pop"
	};

	/* color operator */
	static char *txt_color[] = {
		"		/rgbval cmap buffer 0 get 3 mul 3 getinterval store",
		"		block exch 3 mul rgbval putinterval",
		"	  } for",
		"	  block  0  bcount 127 sub 3 mul  getinterval",
		"	}",
		"	{ ",
		"	  currentfile buffer readhexstring pop pop",
		"	  /rgbval cmap buffer 0 get 3 mul 3 getinterval store",
		"	  0 1 bcount { block exch 3 mul rgbval putinterval } for",
		"	  block 0 bcount 1 add 3 mul getinterval",
		"	} ifelse",
		"  }",
		"  false 3 colorimage",
		"} bind def"
	};

	/* grayscale operator */
	static char *txt_gray[] = {
		"		/rgbval cmap buffer 0 get 1 getinterval store",
		"		block exch rgbval putinterval",
		"	  } for",
		"	  block  0  bcount 127 sub  getinterval",
		"	}",
		"	{ ",
		"	  currentfile buffer readhexstring pop pop",
		"	  /rgbval cmap buffer 0 get 1 getinterval store",
		"	  0 1 bcount { block exch rgbval putinterval } for",
		"	  block 0 bcount 1 add getinterval",
		"	} ifelse",
		"  }",
		"  image",
		"} bind def"
	};

	/* put prolog */
	PSconst_out(dpy, txt);

	if(color) 
		PSconst_out(dpy, txt_color);
	else 
		PSconst_out(dpy, txt_gray);
}

/*****
* Name: 		PSImageBW
* Return Type: 	int
* Description: 	writes out a Black & White image
* In: 
*	dpy:		current postscript output area
*	data:		image data
*	w:			scanline width
*	h:			no of scanlines
*	inverse:	indicates color reversal
* Returns:
*	0 on success, EOF if write failed.
* Note
*	Write the given image array 'pic' (B/W stippled, 1 byte per pixel,
*	0=blk,1=wht) out as hexadecimal, max of 72 hex chars per line.  If
*	inverse is True, then white = 0 and black = 1.
*****/
static int
PSImageBW(PSDisplay *dpy, Byte *data, int w, int h, Boolean inverse)
{
	int	i, j;
	int	err = 0;
	Byte outbyte, bitnum, bit;
    
	outbyte = bitnum = 0;

	/* from left to right, top to bottom */
	for(i = 0; i < h && err != EOF; i++)
	{
		for(j = 0; j < w && err != EOF; j++)
		{
			bit = *(data++);
			outbyte = (outbyte<<1) | ((bit)&0x01);
			bitnum++;
	    
			if(bitnum == 8)
			{
				if(inverse)
					outbyte = ~outbyte & 0xff;
				err = PShex(dpy, outbyte, False);
				outbyte = bitnum = 0;
			}
		}
		if(bitnum)
		{	/*  few bits left over in this row */
			outbyte <<= 8-bitnum;
			if(inverse)
				outbyte = ~outbyte & 0xff;
			err = PShex(dpy, outbyte, False);
			outbyte = bitnum = 0;
		}
	}
	err = PShex(dpy, '\0', True);	/*  Flush the hex buffer if needed */

	return(err);
}

/*****
* Name:			PSImage
* Return Type: 	void
* Description: 	convert image to postscript.
* In: 
*	dpy:		current postscript output area
*	image:		image data
* Returns:
*
*****/
static void 
PSImage(PSDisplay *dpy, XmHTMLImage *image, int x, int y)
{
	XmImageInfo *info = image->html_image;
	Byte *data = info->data;
	int nc = info->ncolors;
	int i, j;
	int w = info->width;
	int h = info->height;
	int slen, colortype, bits;
	int err=0;
	int extra = 0;
	int dest_x = x, dest_y = 0;
	Boolean isanchor = False;
	Boolean colorps = False;

	/* set image name as a comment */
	PSprintf(dpy, "%% PSImage, URL=%s, width = %i, height = %i\n",
		image->url ? image->url : "(unknown)", image->width, image->height);

    /* Isgray returns true if the nth color index is a gray value */
#define Isgray(i,n) (i->reds[n] == i->greens[n] && i->reds[n] == i->blues[n])

    /* Is_bg returns true if the nth color index is the screen background */
#define Is_bg(i,n)  (i->reds[n] == dpy->bg[0] && \
		i->greens[n] == dpy->bg[1] && i->blues[n]== dpy->bg[2])

    /* Is_fg returns true if the nth color index is the screen foreground */
#define Is_fg(i,n)  (i->reds[n] == dpy->fg[0] && \
		i->greens[n] == dpy->fg[1] && i->blues[n] == dpy->fg[2])

	dest_y = -(y - dpy->start_y) - image->height;

	if(data == NULL) 
	{
		/*****
		*  image was not available, draw an empty square instead
		*****/
		PSprintf(dpy, "gsave\n%i %i translate\n%d %d scale\n",
			dest_x, dest_y, w, h);
		PSprintf(dpy, "0.9 setgray SQ fill\n");
		PSprintf(dpy, "grestore\n\n");
		return;
	}

	if(image->owner && image->owner->anchor != NULL &&
		image->owner->anchor->url_type != ANCHOR_NAMED)
		isanchor = True;

	/* draw outline if this is an anchored image */
	if(isanchor) 
	{
		PSprintf(dpy, "gsave\n%i %i translate\n%d %d scale\n",
			dest_x-2, dest_y-2, w+4, h+4);
		PSprintf(dpy, "SQ fill\n");
		PSprintf(dpy, "grestore\n");
		extra = 4;
	}
	
	/*****
	* This is a hack to see if the image is Black & White, 
	* Greyscale or 8 bit color
	* assume it's bw if it has only one or two colors, both some grey's
	* assume it's greyscale if all the colors (>2) are grey
	* Images with only one color do occur too.
	*****/
    
	if(((nc == 2) 
			&& ((Isgray(info,0) && Isgray(info,1))
			|| (Is_bg(info,0) && Is_fg(info,1)) 
			|| (Is_fg(info,0) && Is_bg(info,1)) ))
		|| ((nc == 1)
			&& (Isgray(info,0) || Is_bg(info,0) || Is_fg(info,0))))
	{
		/* this is a black & white image */
		colortype = F_BWDITHER;
		slen = (w+7)/8;
		bits = 1;
		colorps = False;
	}
	else
	{
		/* assume grayscale unless proven otherwise */
		colortype = F_GREYSCALE;
		slen = w;
		bits = 8;
		colorps = False;
		for(i = 0; i < nc; i++)
		{
			if(!Isgray(info,i))
			{
				/* it's color */
				colortype = F_REDUCED;
				slen = w*3;
				bits = 8;
				colorps = True;
				break;
			}
		}
	}
	
	/*  build a temporary dictionary */
	PSprintf(dpy, "20 dict begin\n\n");

	/*  define string to hold a scanline's worth of data */
	PSprintf(dpy, "/pix %d string def\n\n", slen);

	/*  position and scaling */
	PSprintf(dpy, "gsave\n");
    
	if(colortype == F_BWDITHER) 
	{
		/*  1-bit dither code uses 'image' */
		Boolean inverse = False;
	
		/*  set if color#0 is 'white' */
		if((nc == 2 &&
			MONO(info->reds[0], info->greens[0], info->blues[0]) >
				MONO(info->reds[1], info->greens[1], info->blues[1])) ||
			(nc == 1 && 
				MONO(info->reds[0], info->greens[0],info->blues[0]) >
				MONO(127, 127, 127)))
		{
			inverse = True;
		}
	
		/*  dimensions of data */
		PSprintf(dpy, "%d %d %d\n", w, h, bits);
	
		/*  mapping matrix */
		PSprintf(dpy, "[%d 0 0 %d 0 %d]\n\n", w, -h, h);

		/* Position and scaling */
		PSprintf(dpy, "%i %i translate\n%d %d scale\n", dest_x, dest_y, w, h);
	
		PSprintf(dpy, "{currentfile pix readhexstring pop}\n");
		PSprintf(dpy, "image\n");

		/*  write the actual image data */
		err = PSImageBW(dpy, data, w, h, inverse);
	} 
	else
	{
		/*  all other formats */
		unsigned char *rleline = (unsigned char *) NULL;
		int rlen;
	
		/*  if we're using color, make sure 'colorimage' is defined */
		if(colorps)
			PSColorImage(dpy);
	
		PScolormap(dpy, colorps, nc, info->reds, info->greens, info->blues);
		PSrle_cmapimage(dpy, colorps);

		/*  dimensions of data */
		PSprintf(dpy, "%d %d %d\n", w, h, bits);

		/*  mapping matrix */
		PSprintf(dpy, "[%d 0 0 %d 0 %d]\n", w, -h, h);

		/* Position and scaling */
		PSprintf(dpy, "%i %i translate\n%d %d scale\n", dest_x, dest_y, w, h);

		PSprintf(dpy, "rlecmapimage\n");
	
		rleline = (unsigned char *) malloc(w * 2);
		if(!rleline) 
			return;

		for(i = 0; i < h && err != EOF; i++) 
		{
		    rlen = PSencode(data, rleline, w);
		    data += w;
		    for(j = 0; j < rlen && err != EOF; j++)
				err=PShex(dpy, rleline[j], False);
		    err=PShex(dpy, '\0', True);	/*  Flush the hex buffer */
		}
		free(rleline);
	}
	
	/*  stop using temporary dictionary */
	PSprintf(dpy, "end\n");
	PSprintf(dpy, "grestore\n\n");
	
#undef Isgray
#undef Is_fg
#undef Is_bg
}


/************************************************************************/
/***********************								 ****************/
/*********************** XmHTML ToolkitAbstrion Routines ****************/
/***********************								 ****************/
/************************************************************************/

/*****
* Name:			pstkSetForeground
* Return Type: 	int
* Description: 	sets foreground color
* In: 
*	dpy:		current postscript output area
*	gc:			graphics context, unused
*	foreground:	foreground color to set
* Returns:
*	PSprintf return value (ignored by caller)
*****/
static int
pstkSetForeground(Display *disp, XGC gc, unsigned long foreground)
{
	PSDisplay *dpy = (PSDisplay*)disp;
	XmHTMLWidget html = dpy->html;
	unsigned short red, green, blue;

	XCCGetColor(HTML_ATTR(xcc), foreground, &red, &green, &blue);

	return(PSprintf(dpy, "%u %u %u setrgbcolor\n", red, green, blue));
}

static GC
pstkCreateGC(Display *disp, DRAWABLE win, unsigned long valuemask,
	XGCValues *values)
{
	return(NULL);
}

static int
pstkFreeGC(Display *disp, XGC gc)
{
	return(1);
}

static int
pstkCopyGC(Display *disp, XGC src, unsigned long valuemask, XGC dest)
{
	return(1);
}

static int
pstkSetFunction(Display *disp, XGC gc, int function)
{
	return(1);
}

static int
pstkSetClipMask(Display *disp, XGC gc, PIXMAP pixmap)
{
	return(1);
}

static int
pstkSetClipOrigin(Display *disp, XGC gc, int clip_x_origin,
	int clip_y_origin)
{
	return(1);
}

static int
pstkSetTile(Display *disp, XGC gc, PIXMAP tile)
{
	return(1);
}

static int
pstkSetTSOrigin(Display *disp, XGC gc, int ts_x_origin, int ts_y_origin)
{
	return(1);
}

static int
pstkSetFillStyle(Display *disp, XGC gc, int fill_style)
{
	return(1);
}

static int
pstkSetLineAttributes(Display *disp, XGC gc, unsigned int line_width,
	int line_style, int cap_style, int join_style)
{
	return(1);
}

static int
pstkSetBackground(Display *disp, XGC gc, unsigned long background)
{
	return(0);
}

/*****
* Name:			pstkSetFont
* Return Type: 	int
* Description: 	sets the requested font
* In: 
*	dpy:		current postscript output area
*	gc:			graphics context, unused
*	font:		font to set
* Returns:
*	always 1 (ignored by caller)
*****/
static int
pstkSetFont(Display *disp, XGC gc, XmHTMLfont *font)
{
	PSDisplay *dpy = (PSDisplay*)disp;
	PSfont(dpy, font, False);
	return(1);
}

/*****
* Name:			pstkTextWidth
* Return Type: 	int
* Description: 	Postscript textwidth function.
* In: 
*	font:		font to be used when computing pixel width;
*	string:		string for which to compute pixel width;
*	count:		no of characters in string.
* Returns:
*	pixel width of given string.
*****/
static int
pstkTextWidth(XmHTMLfont *font, const char* string, int count)
{
	char *chPtr;
	int width = 0;
	int i = 0;
   fprintf(stderr,"Warning: This function does nothing, returning 0 width.\n");
	for(chPtr = (char*)string; i < count; chPtr++, i++)
	{
	}
   return(0); /* Looks like an incomplete function */
}

#if 0
/*****
* Name:			pstkCopyArea
* Return Type: 	int
* Description: 	postscript XCopyArea.
* In: 
*	too many
* Returns:
*	always 1 (ignored by caller)
*****/
static int
pstkCopyArea(Display *disp, DRAWABLE src, DRAWABLE dest, XGC gc, int src_x,
	int src_y, unsigned int width, unsigned int height, int dest_x, int dest_y)
{
	PSDisplay *dpy = (PSDisplay*)disp;

	PSprintf(dpy, "%% pstkCopyArea\n");

	PScheckPage(dpy, dest_x, dest_y);
	ImageToPs(XtDisplay(dpy->html), src, src_x, src_y,
		width, height, dest_x, -(dest_y - dpy->start_y), 1);

	return(1);
}
#endif

/*****
* Name:			pstkDrawString
* Return Type: 	int
* Description: 	renders text
* In: 
*	too many
* Returns:
*	always 1 (ignored by caller)
*****/
static int
pstkDrawString(Display *disp, DRAWABLE win, struct _XmHTMLFont *font,
	XGC gc, int x, int y, char *string, int length)
{
	PSDisplay *dpy = (PSDisplay*)disp;
	static char *last_ep=NULL;
	char *ep = string+strlen(string);

	/*****
	* XmHTML sends preformatted(<PRE>) text as multiple words within 'str'.
	* Then makes successive calls to this routine adjusting the pointer to
	* the start of the next word in the same string. The problem is, what
	* XmHTML (actually the X server's adobe fonts) thinks  is the width of a
	* space is not what my postscript printer thinks it is. So, until XmHTML
	* changes how it draws <PRE> text, or until I can figure a better way,
	* we'll just ignore successive calls on the rest of the string.
	* -- scott
	*
	* The only correct way would be to add full postscript font handling:
	* one of the properties contained in the XmHTMLfont structure is the
	* width of a single space. I guess we need a real postscript wizard
	* for this as I haven't got *any* clue on how postscript fonts are defined
	* let alone what properties it contains. -- kdh
	*****/
	if(last_ep && last_ep==ep)
		return(1);
	last_ep = ep;
	PSmoveto(dpy, x, y);
	PSfont(dpy, font, False);
	PStext(dpy, string, False);

	return(1);
}

/*****
* Name:			pstkDrawAnchorData
* Return Type: 	void
* Description: 
* In: 
*
* Returns:
*	nothing
*****/
static void
pstkDrawAnchorData(Display *disp, WINDOW win, XGC gc, int x, int y,
	XmHTMLObjectTableElement data)
{
	PSDisplay *dpy = (PSDisplay*)disp;
	int fnum, offset;
	static char *last_href;

	/*****
	* Check if this is a real anchor
	* We don't footnote internal anchors. seems sane to me.
	* might want to exclude other types as well. -- scott
	*****/
	if(!data->anchor || !data->anchor->href || data->anchor->href[0]=='\0' ||
		data->anchor->href[0] == '#' || !dpy->font)
		return;

	if(last_href == data->anchor->href)
		return;
	last_href = data->anchor->href;

	if(y > dpy->start_y + dpy->Pixels_This_Page)
		return;	/* won't fit on page! */

	/* add a footnote marker */
	PSprintf(dpy, "%d %d M\n", x, -(y - dpy->start_y));
	offset = dpy->font_size - 6; /* 6 point font for footnote number */
	fnum = fnAdd(dpy->footnotes, data->anchor->href);
	PSprintf(dpy, "/helvetica 6 SF\n");
	PSprintf(dpy, "2 %d R\n(%d)S\n", offset, fnum+1);
	PSprintf(dpy, "%s %d SF\n", dpy->font_style, dpy->font_size);

	/* reserve room for this footnote */
	dpy->Pixels_This_Page -= FOOTNOTE_ROW_HEIGHT+2;
}

/*****
* Name:			pstkDrawLine
* Return Type: 	int
* Description: 	renders a single line
* In: 
*	too many
* Returns:
*	always 1 (ignored by caller)
*****/
static int
pstkDrawLine(Display *disp, WINDOW win, XGC gc, int x1, int y1, int x2, int y2)
{
	PSDisplay *dpy = (PSDisplay*)disp;
	int yd = y2 - y1;
	int xd = x2 - x1;

	PSprintf(dpy, "%% pstkDrawLine (%d, %d) (%d, %d)\n", x1, y1, x2, y2);
	PSmoveto(dpy, x1, y1);
	PSprintf(dpy, "%d %d RL stroke\n", xd, yd);

	return(1);
}

/*****
* Name:			pstkDrawLines
* Return Type: 	void
* Description: 	draws a collection of lines
* In: 
*	too many
* Returns:
*	always 1 (ignored by caller)
*****/
static int
pstkDrawLines(Display *disp, DRAWABLE win, XGC gc, XPoint *points,
	int num_points, int mode)
{
	PSDisplay *dpy = (PSDisplay*)disp;
	int i;

	PSprintf(dpy, "%% pstkDrawLines\n");
	for(i = 0; i < num_points - 1; ++i)
	{
		pstkDrawLine(disp, win, gc,
			points[i].x, points[i].y, points[i+1].x, points[i+1].y);
	}
	return(1);
}

/*****
* Name:			pstkDrawRectangle
* Return Type: 	int
* Description: 	renders a plain rectangle
* In: 
*	too many
* Returns:
*	always 1 (ignored by caller)
*****/
static int
pstkDrawRectangle(Display *disp, WINDOW win, XGC gc, int x, int y,
	unsigned int width, unsigned int height)
{
	PSDisplay *dpy = (PSDisplay*)disp;
	int ny = y + height;

	PSprintf(dpy, "%% pstkDrawRectangle\n");
	PScheckPage(dpy, x, ny);
	PSprintf(dpy, "newpath %d %d M %u 0 RL 0 -%u RL -%u 0 RL 0 %u RL "
		"closepath stroke\n", x, -(y - dpy->start_y),
		width, height, width, height);

	return(1);
}

/*****
* Name:			pstkFillRectangle
* Return Type: 	int
* Description: 	renders a filled rectangle.
* In: 
*	too many
* Returns:
*	always 1 (ignored by caller)
*****/
static int
pstkFillRectangle(Display *disp, WINDOW win, XGC gc, int x, int y,
	unsigned int width, unsigned int height)
{
	PSDisplay *dpy = (PSDisplay*)disp;
	int ny = y + height;

	PSprintf(dpy, "%% pstkFillRectangle\n");
	PScheckPage(dpy, x, ny);
	PSprintf(dpy, "newpath %d %d M %d 0 RL 0 -%d RL -%d 0 RL 0 %d RL "
		"closepath fill stroke\n", x, -(y - dpy->start_y),
		width, height, width, height);

	return(1);
}

/*****
* Name:			pstkDrawShadows
* Return Type: 	void
* Description: 	draws a shadow rectangle
* In: 
*	too many
* Returns:
*	nothing
* Note:
*	Uses setgray instead of setrgbcolor for the shadow colors. This ensures
*	that shadows will always be drawn.
*****/
static void
pstkDrawShadows(Display *disp, DRAWABLE drawable, 
	XGC top_shadow_GC, XGC bottom_shadow_GC,
#if NeedWidePrototypes
	int x, int y, int width, int height, int shadow_thickness,
#else
	Position x, Position y, Dimension width, Dimension height,
	Dimension shadow_thickness,
#endif
	unsigned int shadow_type)
{
	PSDisplay *dpy = (PSDisplay*)disp;
	int ts=8, bs=4;

	switch(shadow_type)
	{
		case XmSHADOW_IN:
			/* top & left border */
			PSprintf(dpy, ".%d setgray\n", bs);
			pstkFillRectangle(disp, drawable, bottom_shadow_GC, x, y,
				width, 1);
			pstkFillRectangle(disp, drawable, bottom_shadow_GC, x, y,
				1, height-1);

			/* bottom & right border */
			PSprintf(dpy, ".%d setgray\n", ts);
			pstkFillRectangle(disp, drawable, top_shadow_GC, x + 1,
				y + height - 1, width - 1, 1);
			pstkFillRectangle(disp, drawable, top_shadow_GC, x - 1, y + 1, 1,
				height - 2);
			break;
		case XmSHADOW_OUT:
			/* top & left border */
			PSprintf(dpy, ".%d setgray\n", ts);
			pstkFillRectangle(disp, drawable, top_shadow_GC, x, y, width, 1);
			pstkFillRectangle(disp, drawable, top_shadow_GC, x, y, 1, height-1);

			/* bottom & right border */
			PSprintf(dpy, ".%d setgray\n", bs);
			pstkFillRectangle(disp, drawable, bottom_shadow_GC, x + 1,
				y + height - 1, width - 1, 1);
			pstkFillRectangle(disp, drawable, bottom_shadow_GC, x - 1,
				y + 1, 1, height - 2);
			break;
		default:
			break;
	}
	PSprintf(dpy, "1 setgray\n");
}

/*****
* Name:			pstkDrawArc
* Return Type: 	int
* Description: 	renders an (unfilled) arc.
* In: 
*	too many
* Returns:
*	always 1 (ignored by caller)
*****/
static int
pstkDrawArc(Display *disp, WINDOW win, XGC gc, int x, int y,
	unsigned int width, unsigned int height, int angle1, int angle2)
{
	int ny = y+height;
	int x0, y0;		/* center */
	int r;			/* radius */
	PSDisplay *dpy = (PSDisplay*)disp;

	PSprintf(dpy, "%% pstkDrawArc (%ux%u+%d+%d)\n", width, height, x, y);
	PScheckPage(dpy, x, ny);

	r = height/2;
	x0 = x + r;
	y0 = -(y - dpy->start_y + r);

	PSprintf(dpy, "newpath %d %d M %d %d %d %d %d arc closepath\n",
		x0, y0, x, y, r, angle1, angle2);

	return(1);
}

/*****
* Name:			pstkFillArc
* Return Type: 	int
* Description: 	renders a filled arc.
* In: 
*	too many args
* Returns:
*	always 1 (ignored by caller)
*****/
static int
pstkFillArc(Display *disp, WINDOW win, XGC gc, int x, int y,
	unsigned int width, unsigned int height, int angle1, int angle2)
{
	int ny = y+height;
	int x0, y0;		/* center */
	int r;			/* radius */
	PSDisplay *dpy = (PSDisplay*)disp;

	PSprintf(dpy, "%% pstkFillArc (%ux%u+%d+%d) %d\n", width, height, x, y,
		dpy->start_y);

	PScheckPage(dpy, x, ny);

	r = height/2;
	x0 = x + r;
	y0 = -(y - dpy->start_y + r);

	PSprintf(dpy, "newpath %d %d M %d %d %d %d %d arc fill closepath\n",
		x0, y0, x0, y0, r, angle1, angle2);

	return(1);
}

static void
pstkDrawImage(XmHTMLWidget html, XmHTMLImage *image, XGC gc,
	int src_x, int src_y, unsigned int width, unsigned int height,
	int dest_x, int dest_y)
{
	ToolkitAbstraction *tka = HTML_ATTR(tka);
	PSDisplay *dpy = (PSDisplay*)tka->dpy;

	PSImage(dpy, image, dest_x, dest_y);
}

/*****
* Name:			_CreatePostscriptTka
* Return Type: 	ToolkitAbstraction
* Description: 	Creates the tka required for Postscript output
* In: 
*	html:		current XmHTMLWidget id
* Returns:
*	A new tka, based upon the current tka as found in the widget
*****/
static ToolkitAbstraction*
_CreatePostscriptTka(XmHTMLWidget html)
{
	static ToolkitAbstraction *tka;

	/* copy current tka and override the necessary functions */
	tka = XmHTMLTkaCopy(HTML_ATTR(tka));

	/* GC functions */
	tka->CreateGC      = pstkCreateGC;
	tka->FreeGC        = pstkFreeGC;
	tka->CopyGC        = pstkCopyGC;
	tka->SetFunction   = pstkSetFunction;
	tka->SetClipMask   = pstkSetClipMask;
	tka->SetClipOrigin = pstkSetClipOrigin;
	tka->SetTile       = pstkSetTile;
	tka->SetTSOrigin   = pstkSetTSOrigin;
	tka->SetFillStyle  = pstkSetFillStyle;
	tka->SetFont       = pstkSetFont;
	tka->SetForeground = pstkSetForeground;
	tka->SetBackground = pstkSetBackground;
	tka->SetLineAttributes = pstkSetLineAttributes;

	/* Font Allocation functions are not used by postscript output */

	/* Cursor & pointer functions are not used by postscript output */

	/* Color functions are not used by postscript output */

	/* Pixmap functions are not used by postscript output */

	/* XImage functions are not used by postscript output */

	/* misc. render functions */
	tka->DrawImage     = pstkDrawImage;
	tka->DrawAnchorData= pstkDrawAnchorData;

	/* string/text functions are not used by postscript output */

	/* Render functions */
	tka->DrawString     = pstkDrawString;
	tka->DrawLine       = pstkDrawLine;
	tka->DrawLines      = pstkDrawLines;
	tka->DrawRectangle  = pstkDrawRectangle;
	tka->FillRectangle  = pstkFillRectangle;
	tka->DrawArc        = pstkDrawArc;
	tka->FillArc        = pstkFillArc;

	/* misc. functions are not used by postscript output */

	/* X Intrinsic wrappers are not used by postscript output */

	/* Motif Wrappers */
	tka->DrawShadows    = pstkDrawShadows;

	return(tka);
}

/*****
* Name:			_XmHTMLTextGetPS
* Return Type:	String
* Description:	Formats the current HTML into postscript output.
* In:
*	html:		XmHTML Widget id;
*	pdef:		defines the paper size;
*	start/end:	ignored until XmHTML can do selections;
*   options:	An OR of:
*				  XmHTMLTEXT_ADDFOOTER - prints the page number.
*				  XmHTMLTEXT_ANCHORFOOTNOTES - footnotes each anchor on the
*					 the page and provides their HREF's at the bottom.
* Returns:
*	A malloc'd buffer containing postscript output. Calling routine must
*   free the returned buffer.
*****/	
String
_XmHTMLTextGetPS(XmHTMLWidget html, XmHTMLPaperSize *pdef,
	XmHTMLObjectTableElement start, XmHTMLObjectTableElement end,
	Byte options)
{
	ToolkitAbstraction *tka_orig, *tka;
	XmHTMLObjectTableElement pstart, pend;
	int pagewidth;
	Dimension paint_w, paint_h, margin_w, margin_h, work_w;
	Position paint_x, paint_y, scroll_x, scroll_y;
	Boolean anchorb;
	String title;
	PSDisplay *dpy;
	unsigned short red, green, blue;
	String psbuf;

	/*****
	* Sanity check, Postscript output requires a papersize definition 
	* in points.
	*****/
	if(pdef->unit_type != XmHTML_POINT)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLTextGetPS"),
			XMHTML_MSG_88, "POINT");
		return(NULL);
	}

	/* Create a Postscript output area */
	dpy = (PSDisplay *)calloc(1, sizeof(PSDisplay));

	/* initialize the output area */
	dpy->html = html;
	dpy->options = options;

	/* copy paper definition into output screen */
	memcpy((void*)&dpy->screen, pdef, sizeof(XmHTMLPaperSize));

	/*****
	* Postscript top margin seems to behave somewhat differently...
	*
	* Scott, I replaced 11*72 (which I assume is the height of Letter in
	* inches) by the height of the provided papersize (which is already
	* in points). -- kdh.
	*****/
	dpy->screen.top_margin = (pdef->height - pdef->top_margin);
	dpy->screen.height = (dpy->screen.top_margin - dpy->screen.bottom_margin);

	/*****
	* Calculate the number of Postscript points per pixel of current screen,
	* and the height of the page in pixels (used in figuring when we've hit
	* the bottom of the page) and for computing correct text widths.
	*****/
	dpy->Points_Pixel = 72.0 / GetDpi(html);

	pagewidth = pdef->width;

	/*****
	* Reduce the scaling if the width used for formatting is greater than
	* 8 * 72 pixels (8 inch). In theory, this is not what you want for A4
	* paper (only 8.27 inch wide), but I guess that the hw->html.doc_width
	* includes some left and right margins, so it seems to work in practice.
	*
	* Scott, does this test work? Above you set pagewidth to pdef->width
	* and upon entry of this routine, screen.width was set to pdef->width?
	*****/
	if(pagewidth > PAGE_WIDTH)
		dpy->Points_Pixel = dpy->Points_Pixel * (float)PAGE_WIDTH/pagewidth;

	dpy->Pixels_This_Page = (int)(PAGE_HEIGHT/dpy->Points_Pixel);
	dpy->Pixels_Page = dpy->Pixels_This_Page;

	/*****
	* Get the foreground and background colors so we can check later
	* for black&white documents
	*****/
	XCCGetColor(HTML_ATTR(xcc), HTML_ATTR(body_fg), &red, &green, &blue);
	dpy->fg[0] = red;
	dpy->fg[1] = green;
	dpy->fg[2] = blue;
	XCCGetColor(HTML_ATTR(xcc), HTML_ATTR(body_bg), &red, &green, &blue);
	dpy->bg[0] = red;
	dpy->bg[1] = green;
	dpy->bg[2] = blue;

	fnDestroy(dpy->footnotes); /* clear footnotes */

	PSinit(dpy);

	title = XmHTMLGetTitle((Widget)html);

	PSheader(dpy, title ? title : "", 0);

	PSnewpage(dpy);

	/* save all settings that will get altered */
	scroll_x = HTML_ATTR(scroll_x);
	scroll_y = HTML_ATTR(scroll_y);
	paint_y  = HTML_ATTR(paint_y);
	paint_h  = HTML_ATTR(paint_height);
	paint_x  = HTML_ATTR(paint_x);
	paint_w  = HTML_ATTR(paint_width);
	pstart   = HTML_ATTR(paint_start);
	pend	 = HTML_ATTR(paint_end);
	margin_w = HTML_ATTR(margin_width);
	margin_h = HTML_ATTR(margin_height);
	work_w   = HTML_ATTR(work_width);
	anchorb  = HTML_ATTR(anchor_buttons);

	/* reset and set paper properties */
	HTML_ATTR(scroll_x) = 0;
	HTML_ATTR(scroll_y) = 0;
	HTML_ATTR(paint_y) = 0;
	HTML_ATTR(paint_x) = 0;
	HTML_ATTR(paint_width) = pdef->width;
	HTML_ATTR(paint_height) = pdef->height - pdef->bottom_margin;
	HTML_ATTR(paint_start)  = NULL;
	HTML_ATTR(paint_end)  = NULL;
	HTML_ATTR(margin_width) = pdef->left_margin;
	HTML_ATTR(margin_height) = pdef->top_margin;
	HTML_ATTR(work_width) = pdef->width - pdef->left_margin;
	HTML_ATTR(anchor_buttons) = False;		/* no button anchors, looks ugly */

	dpy->start_y = 0;

	/* Recompute layout for new paper definition before we set tka */
	_XmHTMLComputeLayout(html);

	HTML_ATTR(paint_height) = max(HTML_ATTR(paint_height),
		HTML_ATTR(formatted_height));

	/* temporarily set the ToolkitAbstractions for postscript */

	/* save current tka */
	tka_orig = HTML_ATTR(tka);
	tka = _CreatePostscriptTka(html);
	HTML_ATTR(tka) = tka;

	/* save original display pointer */
	dpy->dpy = tka->dpy;

	/* store new one */
	tka->dpy = (Display*)dpy;
	tka->win = tka_orig->win;

	/* render as postscript */
	_XmHTMLPaint(html, HTML_ATTR(formatted), NULL);

	PSshowpage(dpy);
	PStrailer(dpy);

	/* All done. Get return buffer */
	psbuf = dpy->string;

	/* all done, release dpy */
	fnDestroy(dpy->footnotes);
	free(dpy);
	XmHTMLTkaDestroy(tka);

	HTML_ATTR(tka) = tka_orig;

	/* reset everything */
	HTML_ATTR(scroll_x) = scroll_x;
	HTML_ATTR(scroll_y) = scroll_y;
	HTML_ATTR(paint_y) = paint_y;
	HTML_ATTR(paint_x) = paint_x;
	HTML_ATTR(paint_width) = paint_w;
	HTML_ATTR(paint_height) = paint_h;
	HTML_ATTR(paint_start)  = pstart;
	HTML_ATTR(paint_end)  = pend;
	HTML_ATTR(margin_width) = margin_w;
	HTML_ATTR(margin_height) = margin_h;
	HTML_ATTR(work_width) = work_w;
	HTML_ATTR(anchor_buttons) = anchorb;

	/* Redisplay to restore everything correctly */
	XmHTMLRedisplay((Widget)html);

	return(psbuf);
}
