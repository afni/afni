#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* format.c : XmHTML formatting routines: translates parsed HTML to 	info
*			required for displaying a HTML page.
*
* This file Version	$Revision$
*
* Creation date:		Tue Nov 26 17:03:09 GMT+0100 1996
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
* ChangeLog
* $Log$
* Revision 1.1  2011/06/30 16:10:38  rwcox
* Cadd
*
* Revision 1.19  1998/04/27 06:59:23  newt
* tka stuff
*
* Revision 1.18  1998/04/04 06:28:07  newt
* XmHTML Beta 1.1.3
*
* Revision 1.17  1997/10/23 00:24:56  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.16  1997/08/31 17:34:24  newt
* renamed _rec structures to Rec.
*
* Revision 1.15  1997/08/30 00:55:24  newt
* Completed <form></form> support.
* Bugfix in _XmHTMLInitializeFontSizeLists, the default font is now properly
* changed.
* Made the font loading routines again robuster.
* ParseBodyTags now always attempts to load a body image.
* Made XmHTMLGetURLType a bit stricter.
*
* Revision 1.14  1997/08/01 13:00:21  newt
* Bugfixes in font switching (<b>...<font><i>...</i></font>...</b>) now
* properly handled. Enhanced form support.
*
* Revision 1.13  1997/05/28 01:46:35  newt
* Added support for the XmNbodyImage resource: it's now used but only if no
* bgcolor resource has been set.
*
* Revision 1.12  1997/04/29 14:26:18  newt
* HTML forms changes
*
* Revision 1.11  1997/04/03 05:34:25  newt
* _XmHTMLLoadBodyImage added.
* Placed a large number of warnings between a #ifdef PEDANTIC/#endif
*
* Revision 1.10  1997/03/28 07:12:43  newt
* Fixed buffer overrun in TexToPre.
* Fixed font resolution: x and y resolution are now always equal.
* XmHTML now ignores the ending body tag.
*
* Revision 1.9  1997/03/20 08:10:04  newt
* Split font cache in a true cache and a font stack.
* Added stack checks when document has been formatted.
*
* Revision 1.8  1997/03/11 19:52:17  newt
* added ImageToWord
*
* Revision 1.7  1997/03/04 00:59:26  newt
* ?
*
* Revision 1.6  1997/03/02 23:17:46  newt
* Way too many changes. Most important: font loading/switching scheme; anchor
* treatment; image/imagemap treatment
*
* Revision 1.5  1997/02/11 02:08:44  newt
* Way to many. Anchor treatment has been changed completely.
* Bugfixes in anchor parsing. Potential buffer overruns eliminated.
*
* Revision 1.4  1997/02/04 02:56:49  newt
* Bugfix in LoadQueryFont.
* Added code to deal with the basefont element.
* Changed the font element handling.
*
* Revision 1.3  1997/01/09 06:55:39  newt
* expanded copyright marker
*
* Revision 1.2  1997/01/09 06:44:42  newt
* lots of changes: linebreaking and changes related to changed XmHTMLWord
*
* Revision 1.1  1996/12/19 02:17:10  newt
* Initial Revision
*
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>	/* isspace, tolower */

/* Local includes */
#include "toolkit.h"
#include XmHTMLPrivateHeader
#include "stack.h"

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

typedef struct{
	String name;
	Marker type;	/* Marker is an enumeration type defined in XmHTMLP.h */
}listMarkers;

/* for nesting of ordered and unordered lists */
typedef struct{
	Boolean isindex;	/* propagate index numbers? */
	int level;			/* item number */
	htmlEnum type;		/* ol or ul, used for custom markers */
	Marker marker;		/* marker to use */
}listStack;

/*** Private Function Prototype Declarations ****/

/****
* Formatting routines
*****/

/* Release the formatted element table */
static void FreeObjectTable(XmHTMLObjectTable *list);

/* Free the given list of tables */
static void freeTables(XmHTMLTable *table);

/* Initialize the formatted element table */
static void InitObjectTable(XmHTMLObjectTable *list, XmHTMLAnchor *anchors);

/* copy given text into an internal buffer */
static String CopyText(XmHTMLWidget html, String text, Boolean formatted,
	Byte *text_data, Boolean expand_escapes, Boolean *i18n);

/* collapse all consecutive whitespace into a single space */
static void CollapseWhiteSpace(String text);

/* Split raw text into an array of words */
static XmHTMLWord* TextToWords(String text, int *num_words, Dimension *height,
	XmHTMLfont *font, Byte line_data, Byte text_data,
	XmHTMLObjectTableElement owner, ToolkitAbstraction *tka);

/* Split an image into an array of words ;-) */
static XmHTMLWord *ImageToWord(XmHTMLWidget html, String attributes,
	int *num_words, Dimension *height, XmHTMLObjectTableElement owner,
	Boolean formatted, ToolkitAbstraction *tka, Boolean is_anchor,
	Byte text_data);

static XmHTMLWord *allocFormWord(XmHTMLWidget html, XmHTMLForm *form,
	Dimension *width, Dimension *height, XmHTMLObjectTableElement owner,
	Boolean formatted);

static XmHTMLWord *InputToWord(XmHTMLWidget html, String attributes,
	int *num_words, Dimension *width, Dimension *height,
	XmHTMLObjectTableElement owner, Boolean formatted);

static XmHTMLWord *SelectToWord(XmHTMLWidget html, XmHTMLObject *start,
	int *num_words, Dimension *width, Dimension *height,
	XmHTMLObjectTableElement owner, Boolean formatted);

static XmHTMLWord *TextAreaToWord(XmHTMLWidget html, XmHTMLObject *start,
	int *num_words, Dimension *width, Dimension *height,
	XmHTMLObjectTableElement owner, Boolean formatted);

static XmHTMLWord *BreakToWord(Dimension *height, XmHTMLfont *font,
	int linefeed, XmHTMLObjectTableElement owner);

static XmHTMLWord *MakeDummyWord(Dimension *height, XmHTMLfont *font,
	Byte line_data, XmHTMLObjectTableElement owner);

/* Split raw text into a chunk of preformatted lines */
static XmHTMLWord *TextToPre(String text, int *num_words, XmHTMLfont *font,
	Byte line_data, XmHTMLObjectTableElement owner, ToolkitAbstraction *tka,
	int tabwidth);

/* Insert a horizontal tab */
static XmHTMLWord* SetTab(int size, Dimension *height, XmHTMLfont *font,
	XmHTMLObjectTableElement owner, ToolkitAbstraction *tka);

/* Initialize a bullet (a real bullet or some number) */
static void FillBullet(XmHTMLWidget html, XmHTMLObjectTableElement owner,
	ToolkitAbstraction *tka);

/* get properties of a table element */
static TableProperties *tableCheckProperties(XmHTMLWidget html,
	String attributes, TableProperties *parent, Alignment halign, Pixel bg);

/* open a new table */
static XmHTMLTable *tableOpen(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement start, XmHTMLObject *obj, Alignment *halign,
	Pixel *bg);

/* close the current table */
static XmHTMLTable *tableClose(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement end);

/* open a caption in the current table */
static void tableOpenCaption(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement start, XmHTMLObject *obj,
	Pixel *bg);

/* close the current caption */
static void tableCloseCaption(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement end);

/* open a row in the current table */
static void tableOpenRow(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement start, XmHTMLObject *obj, Alignment *halign,
	Pixel *bg);

/* close the current row */
static void tableCloseRow(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement end);

/* open a cell in the current row */
static void tableOpenCell(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement start, XmHTMLObject *obj, Alignment *halign,
	Pixel *bg);

/* close the current cell in the current row */
static void tableCloseCell(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement end);

/* Parse body tags and update the widget */
static void ParseBodyTags(XmHTMLWidget html, XmHTMLObject *data);

/* Check whether a linefeed is required or not */
static int CheckLineFeed(int op, Boolean force, Byte *text_data);

/* split the given anchor spec into a href, target and other stuff */
static void parseHref(String text, XmHTMLAnchor *anchor);

/*** Private Variable Declarations ***/
/* Element data bits */
#define ELE_ANCHOR				(1<<1)	/* an anchor						*/
#define ELE_ANCHOR_TARGET		(1<<2)	/* with a target attribute			*/
#define ELE_ANCHOR_VISITED		(1<<3)	/* it's a previously visited one	*/
#define ELE_ANCHOR_INTERN		(1<<4)	/* with a name attribute			*/
#define ELE_UNDERLINE			(1<<5)	/* underlined object				*/
#define ELE_UNDERLINE_TEXT		(1<<6)	/* underlined text					*/
#define ELE_STRIKEOUT			(1<<7)	/* strikeout object					*/
#define ELE_STRIKEOUT_TEXT		(1<<8)	/* strikeout text					*/

/* Private formatted element table data */
static struct{
#ifdef DEBUG
	unsigned long num_elements;
	unsigned long num_anchors;
#endif
	XmHTMLObjectTableElement head;
	XmHTMLObjectTableElement current;
	XmHTMLAnchor *anchor_head;
	XmHTMLAnchor *anchor_current;
}list_data;

/* Marker information for HTML lists, ordered list. */
#define OL_ARRAYSIZE	5
static listMarkers ol_markers[OL_ARRAYSIZE] = {
	{"1", XmMARKER_ARABIC},
	{"a", XmMARKER_ALPHA_LOWER},
	{"A", XmMARKER_ALPHA_UPPER},
	{"i", XmMARKER_ROMAN_LOWER},
	{"I", XmMARKER_ROMAN_UPPER},
};

/* Unordered list. */
#define UL_ARRAYSIZE	3
static listMarkers ul_markers[UL_ARRAYSIZE] = {
	{"disc", XmMARKER_DISC},
	{"square", XmMARKER_SQUARE},
	{"circle", XmMARKER_CIRCLE},
};

#ifdef DEBUG
static int allocated;
#endif

/*****
* Name: 		NewTableElement
* Return Type: 	MACRO
* Description: 	creates a XmHTMLObjectTableElement and fills it.
*				Macro for obvious perfomance reasons.
* In:
*	ELEMENT:	element to be allocated
*	data:		raw data for this element.
* Returns:
*	the newly created element.
*****/
#ifdef DEBUG

#define NewTableElement(ELEMENT,DATA) do{ \
	(ELEMENT) = (XmHTMLObjectTableElement)malloc(sizeof(XmHTMLObjectTable)); \
	(void)memset((ELEMENT), 0, sizeof(XmHTMLObjectTable)); \
	(ELEMENT)->object = DATA; \
	allocated++; \
}while(0)

#else

#define NewTableElement(ELEMENT,DATA) do{ \
	ELEMENT = (XmHTMLObjectTableElement)malloc(sizeof(XmHTMLObjectTable)); \
	(void)memset(ELEMENT, 0, sizeof(XmHTMLObjectTable)); \
	ELEMENT->object = DATA; \
}while(0)

#endif /* DEBUG */

/*****
* Name: 		InsertTableElement
* Return Type: 	MACRO
* Description: 	inserts a given formatted element in the list of elements.
*				Macro for obvious perfomance reasons.
* In:
*	element:	element to add
*	is_anchor:	true if this element is an anchor.
* Returns:
*	NA
*****/
#ifdef DEBUG

#define InsertTableElement(ELEMENT,IS_ANCHOR) do { \
	ELEMENT->prev = list_data.current; \
	list_data.current->next = ELEMENT; \
	list_data.current = ELEMENT; \
	list_data.num_elements++; \
	if((IS_ANCHOR)) list_data.num_anchors++; \
}while(0)

#else

#define InsertTableElement(ELEMENT,IS_ANCHOR) do { \
	ELEMENT->prev = list_data.current; \
	list_data.current->next = ELEMENT; \
	list_data.current = ELEMENT; \
}while(0)

#endif /* DEBUG */

/*****
* Name: 		parseHref
* Return Type: 	void
* Description: 	returns the url specification found in the given anchor.
* In:
*	text:		full anchor spec.
*	href:		url found in given anchor. Filled upon return.
*	target:		any target attribute found. Filled upon return.
*	extra:		any additional attributes found. Filled upon return.
* Returns:
*	nothing.
*****/
static void
parseHref(String text, XmHTMLAnchor *anchor)
{
	if(text == NULL ||
		(anchor->href = _XmHTMLTagGetValue(text, "href")) == NULL)
	{
		/* allocate empty href field so later strcmps won't explode */
		anchor->href = (char *)malloc(1);
		anchor->href[0] = '\0'; /* fix 02/03/97-05, kdh */
		/*
		* Could be a named anchor with a target spec. Rather impossible but
		* allow for it anyway (I can imagine this to be true for a
		* split-screen display).
		*/
		if(text == NULL)
			return;
	}

	/* Check if there is a target specification */
	anchor->target= _XmHTMLTagGetValue(text, "target");

	/* Also check for rel, rev and title */
	anchor->rel = _XmHTMLTagGetValue(text, "rel");
	anchor->rev = _XmHTMLTagGetValue(text, "rev");
	anchor->title  = _XmHTMLTagGetValue(text, "title");
}

/*****
* Name: 		FreeObjectTable
* Return Type: 	void
* Description: 	releases all memory occupied by the formatted list of elements.
* In:
*	list:		previous list to be released.
* Returns:
*	nothing.
* Note:
*	Images are freed in XmHTML.c, which calls XmHTMLFreeAllImages to do the
*	job.
*****/
static void
FreeObjectTable(XmHTMLObjectTable *list)
{
	XmHTMLObjectTableElement temp;

#ifdef DEBUG
	int i = 0, j = 0;
#endif

	/* free all parsed objects */
	while(list != NULL)
	{
		temp = list->next;
		if(list->text)	/* space occupied by text to display */
			free(list->text);

		/* free list of words. Can't be done above, <pre> doesn't have this! */
		if(list->n_words)
		{
			/*
			* only the first word contains a valid ptr, all others point to
			* some char in this buffer, so freeing them *will* cause a
			* segmentation fault eventually.
			*/
			free(list->words[0].word);
			/* Free raw word data */
			free(list->words);
		}
		free(list);
		list = temp;
#ifdef DEBUG
		i++;
#endif
	}
	_XmHTMLDebug(2, ("format.c: FreeObjectTable End, freed %i elements and "
		"%i anchors.\n", i, j));
}

/*****
* Name:			freeTables
* Return Type: 	void
* Description: 	frees all data allocated for HTML table support.
* In:
*	table:		list of tables to be freed.
* Returns:
*	nothing.
*****/
static void
freeTables(XmHTMLTable *table)
{
	XmHTMLTable *tab, *tmp = table;
	TableRow *row;
	int i, j, k;

	while(table)
	{
		tmp = table->next;

		/*****
		* Free all child tables (first table in the childs array is the
		* table itself)
		*****/
		for(i = 0; i < table->nchilds; i++)
		{
			tab = &table->childs[i];

			/* free all rows */
			for(j = 0; j < tab->nrows; j++)
			{
				row = &tab->rows[j];
				/* free all cells in this row */
				for(k = 0; k < row->ncells; k++)
				{
					free(row->cells[k].props);
				}
				free(row->cells);
				free(row->props);
			}
			free(tab->rows);
			free(tab->props);
		}
		free(table->childs);
		free(table);
		table = tmp;
	}
}

/*****
* Name: 		FreeAnchors
* Return Type: 	void
* Description: 	frees the memory occupied by the anchor data
* In:
*	anchors:	list of anchors to be freed
* Returns:
*	nothing.
*****/
static void
FreeAnchors(XmHTMLAnchor *anchors)
{
	XmHTMLAnchor *tmp;
	int i = 0;

	while(anchors)
	{
		tmp = anchors->next;
		/* href field is always allocated */
		free(anchors->href);
		if(anchors->target)
			free(anchors->target);
		if(anchors->rel)
			free(anchors->rel);
		if(anchors->rev)
			free(anchors->rev);
		if(anchors->title)
			free(anchors->title);
		if(anchors->name)		/* fix 07/09/97-01, kdh */
			free(anchors->name);
		if(anchors->events)
			free(anchors->events);
		free(anchors);
		anchors = NULL;
		anchors = tmp;
		i++;
	}
	_XmHTMLDebug(2, ("format.c: FreeAnchors, freed %i XmHTMLAnchor objects\n",
		i));
}

/*****
* Name: 		InitObjectTable
* Return Type: 	void
* Description: 	initializes the list of formatted elements.
* In:
*	list:		previous list to be released.
* Returns:
*	nothing
* Note:
*	The list head is a dummy element and is never used. It is done to gain
*	some performance (a test on an empty head is not required now in the
*	InsertTableElement routine).
*****/
static void
InitObjectTable(XmHTMLObjectTable *list, XmHTMLAnchor *anchors)
{
	if(list != NULL)
	{
		FreeObjectTable(list);
		list = NULL;
	}

	if(anchors != NULL)
	{
		FreeAnchors(anchors);
		anchors = NULL;
	}
	if(list_data.head)
		free(list_data.head);
	NewTableElement(list_data.head,NULL);
	list_data.current = list_data.head;
	list_data.anchor_head = (XmHTMLAnchor*)NULL;
	list_data.anchor_current = (XmHTMLAnchor*)NULL;
#ifdef DEBUG
	list_data.num_elements = 1;
	list_data.num_anchors  = 0;
#endif
}

/*****
* Name: 		CollapseWhiteSpace
* Return Type: 	void
* Description: 	collapses whitespace in the given text
* In:
*	text:		text for which multiple whitespace has to be collapsed.
* Returns:
*	nothing, but text is updated when this function returns.
*****/
static void
CollapseWhiteSpace(String text)
{
	register char *outPtr = text;
#ifdef I18N
	int n = 1;
#endif	/* I18N */

	/*
	* We only collapse valid text and text that contains more than whitespace
	* only. This should never be true since CopyText will filter these
	* things out. It's just here for sanity.
	*/
	if(*text == '\0' || !strlen(text))
		return;

	_XmHTMLDebug(2, ("format.c: CollapseWhiteSpace, text in is:\n%s\n", text));

	/*
	* Now collapse each occurance of multiple whitespaces.
	* This may produce different results on different systems since
	* isspace() might not produce the same on each and every platform.
	*/
	while(True)
	{

#ifdef I18N
		if((n = mblen((char*)text, (size_t)(strlen(text)))) == 1)
		{
#endif	/* I18N */
		switch(*text)
		{
			case '\f':
			case '\n':
			case '\r':
			case '\t':
			case '\v':
				*text = ' ';	/* replace by a single space */
				/* fall through */
			case ' ':
				/* skip past first space */
				*(outPtr++) = *(text++);
#ifdef I18N
				n = mblen((char*)text, (size_t)(strlen(text)));
				while(n == 1 && *text != '\0' && isspace(*text))
					*text++ = '\0';
#else
				/* collapse every space following */
				while(*text != '\0' && isspace(*text))
					*text++ = '\0';
#endif /* I18N */
				break;
			default:
				*(outPtr++) = *(text++);
				break;
		}
		if(*text == 0)
		{
			*outPtr = '\0';
			return;
		}
#ifdef I18N
		}
		else if(n > 1)
		{
			/*****
			* Multibyte char, copy it.
			*****/
			int i;
			for(i = 0; i < n; i++)
				*(outPtr++) = *(text++);
		}
		else
			break;
#endif	/* I18N */
	}
}

/*****
* Name: 		TextToWords
* Return Type: 	XmHTMLWord*
* Description: 	splits the given text into an array of words.
* In:
*	text:		text to split
*	num_words:	number of words in the given text. Filled upon return;
*	font:		font to use for this text.
* Returns:
*	an array of words. When allocation fails, this routine exits.
*****/
static XmHTMLWord*
TextToWords(String text, int *num_words, Dimension *height, XmHTMLfont *font,
	Byte line_data, Byte text_data, XmHTMLObjectTableElement owner,
	ToolkitAbstraction *tka)
{
	int n_words, len, i;
	char *start;
	static XmHTMLWord *words;
	static char *raw;
	register int j;
	register char *chPtr;

	/* sanity check */
	if(text == NULL)
	{
		*height = *num_words = 0;
		return(NULL);
	}

	_XmHTMLFullDebug(2, ("format.c: TextToWords, text in is:\n%s\n", text));

	/* compute how many words we have */
	n_words = 0;
	for(chPtr = text; *chPtr != '\0'; chPtr++)
		if(*chPtr == ' ')
			n_words++;
	/* also pick up the last word */
	n_words++;

	/* copy text */
	raw = strdup(text);

	/* allocate memory for all words */
	words = (XmHTMLWord*)calloc(n_words, sizeof(XmHTMLWord));

	/* Split the text in words and fill in the appropriate fields */
	*height = font->height;

	chPtr = start = raw;

	for(i = 0, j = 0, len = 0; ; chPtr++, len++, j++)
	{
		/* also pick up the last word! */
		if(*chPtr == ' ' || *chPtr == '\0')
		{
			if(*chPtr)
			{
				chPtr++;			/* nuke the space */
				raw[j++] = '\0';
			}
			/* fill in required fields */
			words[i].self      = &words[i];
			words[i].word      = start;
			words[i].len       = len;
#ifdef I18N

			/*****
			* Possible multibyte input, use multibyte functions to
			* determine correct size of this word.
			*****/
			if(font->type == XmHTML_FONTSET)
			{
				int nbytes = 0;
				Byte *mbPtr = start;
				int n = mblen((char*)mbPtr, (size_t)len);

				/*****
				* Compute number of multibyte characters this word
				* occupies.
				*****/
				while(n > 0)
				{
					nbytes += n;
					mbPtr  += n;
					len    -= n;
					n       = mblen((char*)mbPtr, (size_t)len);
				}
				len = nbytes;	/* used by TextWidth computation */
				words[i].len = len;
			}
			else
				words[i].len   = len;
#else
			words[i].len       = len;
#endif	/* I18N */

			words[i].height    = *height;
			words[i].width     = tka->TextWidth(font, words[i].word, len);
			words[i].owner     = owner;
			words[i].font      = font;
			words[i].spacing   = TEXT_SPACE_LEAD | TEXT_SPACE_TRAIL;
			words[i].type      = OBJ_TEXT;
			words[i].line_data = line_data;

			_XmHTMLFullDebug(2, ("format.c: TextToWords, word is %s, len is "
				"%i, width is %i, height is %i\n", words[i].word, words[i].len,
				words[i].width, words[i].height));

			start = chPtr;
			i++;
			len = 0;
		}
		if(*chPtr == '\0')
			break;
	}
	/*
	* when there is more than one word in this block, the first word
	* _always_ has a trailing space.
	* Likewise, the last word always has a leading space.
	*/
	if(n_words > 1)
	{
		/* unset nospace bit */
		Byte spacing = text_data & ~TEXT_SPACE_NONE;
		words[0].spacing = spacing | TEXT_SPACE_TRAIL;
		words[n_words-1].spacing = spacing | TEXT_SPACE_LEAD;
	}
	else
		words[0].spacing = text_data;

	_XmHTMLFullDebug(2, ("format.c: TextToWords counted %i words\n", n_words));

	*num_words = i; /* n_words */;
	return(words);
}

/*****
* Name: 		ImageToWord
* Return Type: 	XmHTMLWord*
* Description: 	converts an image to a word
* In:
*	w:			XmHTMLWidget id
*	attributes:	raw <img> specification
*	height:		object height, updated upon return
*	owner:		owning object
*	formatted:	True when this image is part of a block of <pre></pre> text.
*	ToolkitAbstraction:	renderer to be used;
*	is_anchor:	True when image is part of an anchor, False if not.
* Returns:
*	a word representing the image
*****/
static XmHTMLWord*
ImageToWord(XmHTMLWidget html, String attributes, int *num_words,
	Dimension *height, XmHTMLObjectTableElement owner, Boolean formatted,
	ToolkitAbstraction *tka, Boolean is_anchor, Byte text_data)
{
	static XmHTMLWord *word;
	static XmHTMLImage *image;
	Dimension width = 0;

	*num_words = 0;

	/* sanity check */
	if(attributes == NULL ||
		(image = _XmHTMLNewImage(html, attributes, &width, height)) == NULL)
	{
		*height = 0;
		return(NULL);
	}

	/*****
	* Check for border, isn't done in the image loading routines since
	* the default border is context-dependent: if it's an anchor the default
	* is to add a border, if it's plain text default is no border.
	*****/
	image->border = _XmHTMLTagGetNumber(attributes, "border", (int)is_anchor);

	_XmHTMLFullDebug(2, ("format.c: ImageToWord, image in is: %s\n",
		image->url));

	word = (XmHTMLWord*)calloc(1, sizeof(XmHTMLWord));

	/* required for image anchoring/replace/update */
	image->owner = owner;

	/* fill in required fields */
	word->self   = word;
	word->word   = strdup(image->alt);	/* we always have this */

#ifdef I18N
	if((HTML_ATTR(default_font))->type == XmHTML_FONTSET)
		_XmHTMLWarning(__WFUNC__(html, "ImageToWord"),
			"I18N not (yet) supported for <IMG> alt attribute.\n");
#else
	word->len    = strlen(image->alt);
#endif	/* I18N */

	word->width  = width + 2*image->hspace + 2*image->border;
	word->height = *height + 2*image->vspace + 2*image->border;
	word->owner  = owner;
	word->font   = HTML_ATTR(default_font); /* always use the default font */
	/*****
	* if image support is disabled, add width of the alt text to the
	* image width (either from default image or specified in the doc).
	* This is required for proper exposure handling when images are disabled.
	*****/
	if(!HTML_ATTR(images_enabled))
		word->width += tka->TextWidth(word->font, word->word, word->len);

	/*****
	* No spacing if part of a chunk of <pre></pre> text
	* Fix 07/24/97, kdh
	*****/
	word->spacing = formatted ? 0 : text_data;
	word->type = OBJ_IMG;
	word->line_data = NO_LINE;	/* no underlining for images */
	word->image = image;

	_XmHTMLFullDebug(2, ("format.c: TextToWords, word is %s, len is %i, "
		"width is %i, height is %i\n", word->word, word->len,
		word->width, word->height));

	*num_words = 1;
	return(word);
}

/*****
* Name:			allocFormWord
* Return Type: 	XmHTMLWord*
* Description: 	allocates a default XmHTMLWord for use within a HTML form.
* In:
*	html:		XmHTMLWidget id
*	form:		form entry for which this word should be allocated;
*	*width:		object's width, updated upon return;
*	*height:	object's height, updated upon return;
*	owner:		owning object.
*	formatted:	true when allocating a form component present in <pre></pre>
* Returns:
*	a newly allocated word.
*****/
static XmHTMLWord*
allocFormWord(XmHTMLWidget html, XmHTMLForm *form, Dimension *width,
	Dimension *height, XmHTMLObjectTableElement owner, Boolean formatted)
{
	static XmHTMLWord *word;

	/* allocate new entry */
	word = (XmHTMLWord*)calloc(1, sizeof(XmHTMLWord));

	/* fill in required fields */
	word->self    = word;
	word->word    = strdup(form->name); 	/* we always have this */
	word->len     = strlen(form->name);
	word->height  = *height = form->height;
	word->width   = *width  = form->width;
	word->owner   = owner;
	word->font    = HTML_ATTR(default_font); 	/* always use default font */
	word->spacing = formatted ? 0 : TEXT_SPACE_LEAD | TEXT_SPACE_TRAIL;
	word->type    = OBJ_FORM;
	word->form    = form;

	return(word);
}

/*****
* Name: 		InputToWord
* Return Type: 	XmHTMLWord*
* Description: 	converts a HTML form <input> element to a word
* In:
*	w:			XmHTMLWidget id
*	attributes:	raw form element specification
*	width:		object width, updated upon return
*	height:		object height, updated upon return
*	owner:		owning object
*	formatted:	true when this form component is placed in a <pre></pre> tag.
* Returns:
*	a word representing the image
*****/
static XmHTMLWord*
InputToWord(XmHTMLWidget html, String attributes, int *num_words,
	Dimension *width, Dimension *height, XmHTMLObjectTableElement owner,
	Boolean formatted)
{
	static XmHTMLForm *form_entry;
	XmHTMLWord *word;

	*num_words = 0;

	/* sanity check */
	if(attributes == NULL ||
		(form_entry = _XmHTMLFormAddInput(html, attributes)) == NULL)
		return(NULL);

	/* save owner, we need it in the paint routines */
	form_entry->data = owner;

	/* image buttons are treated as anchored images */
	if(form_entry->type == FORM_IMAGE)
	{
		word = ImageToWord(html, attributes, num_words, height, owner,
				formatted, HTML_ATTR(tka), True,
				TEXT_SPACE_LEAD|TEXT_SPACE_TRAIL);
		/* remove alt text */
		free(word->word);
		/* use form member name instead */
		word->word = strdup(form_entry->name);
		word->len  = strlen(form_entry->name);
		word->form = form_entry;

		_XmHTMLFullDebug(2, ("format.c: InputToWord, word is %s, len is %i, "
			"width is %i, height is %i (type = image)\n", word->word,
			word->len, word->width, word->height));

		return(word);
	}

	/* allocate new word for this form member */
	word = allocFormWord(html, form_entry, width, height, owner, formatted);

	_XmHTMLFullDebug(2, ("format.c: InputToWord, word is %s, len is %i, "
		"width is %i, height is %i\n", word->word, word->len,
		word->width, word->height));

	*num_words = 1;
	return(word);
}

/*****
* Name:			SelectToWord
* Return Type: 	XmHTMLWord*
* Description:	converts a HTML form <select></select> to a HTMLWord.
*				Also processes any <option></option> items within this select.
* In:
*	html:		XmHTMLWidget id;
*	start:		object at which <select> starts;
*	*num_words:	no of words allocated. Updated upon return;
*	*width:		width of returned object. Updated upon return;
*	*height:	height of returned object. Updated upon return;
*	owner:		owning element.
*	formatted:	true when this form component is placed in a <pre></pre> tag.
* Returns:
*	a newly allocated word upon success. NULL on failure.
*****/
static XmHTMLWord*
SelectToWord(XmHTMLWidget html, XmHTMLObject *start, int *num_words,
	Dimension *width, Dimension *height, XmHTMLObjectTableElement owner,
	Boolean formatted)
{
	static XmHTMLForm *form_entry;
	XmHTMLWord *word;
	XmHTMLObject *tmp = start;
	Boolean i18n = False;

	*num_words = 0;

	/* sanity check */
	if(start->attributes == NULL ||
		(form_entry = _XmHTMLFormAddSelect(html, start->attributes)) == NULL)
		return(NULL);

	/* save owner */
	form_entry->data = owner;

	/* move to next element */
	tmp = tmp->next;

	/* add all option tags */
	for(; tmp != NULL && tmp->id != HT_SELECT; tmp = tmp->next)
	{
		if(tmp->id == HT_OPTION && !tmp->is_end)
		{
			XmHTMLObject *sel_start = tmp;
			Byte foo;
			String text = NULL;

			/*
			* The next object should be plain text, if not it's an
			* error and we should ignore it
			*/
			tmp = tmp->next;
			if(tmp->id != HT_ZTEXT)
			{
				if(HTML_ATTR(bad_html_warnings))
				{
					/* empty option tag, ignore it */
					if(tmp->id == HT_OPTION)
						_XmHTMLWarning(__WFUNC__(html, "SelectToWord"),
							XMHTML_MSG_40, tmp->line);
					else
						_XmHTMLWarning(__WFUNC__(html, "SelectToWord"),
							XMHTML_MSG_41, html_tokens[tmp->id],
							html_tokens[HT_OPTION], tmp->line);
				}
				continue;
			}
			/* get text */
			if((text = CopyText(html, tmp->element, False, &foo, True,
				&i18n)) == NULL)
				continue;

			CollapseWhiteSpace(text);
			if(strlen(text))
			{
				_XmHTMLFormSelectAddOption(html, form_entry,
					sel_start->attributes, text);
				/* no longer needed */
				free(text);
			}
		}
	}
	/* close this selection and get width and height */
	_XmHTMLFormSelectClose(html, form_entry);

	/* allocate new word for this form member */
	word = allocFormWord(html, form_entry, width, height, owner, formatted);

	_XmHTMLFullDebug(2, ("format.c: SelectToWord, word is %s, len is %i, "
		"width is %i, height is %i\n", word->word, word->len,
		word->width, word->height));

	*num_words = 1;
	return(word);
}

/*****
* Name:			TextAreaToWord
* Return Type: 	XmHTMLWord*
* Description:	converts a HTML form <textarea> to a HTMLWord.
* In:
*	html:		XmHTMLWidget id;
*	start:		object at which <textarea> starts;
*	*num_words:	no of words allocated. Updated upon return;
*	*width:		width of returned object. Updated upon return;
*	*height:	height of returned object. Updated upon return;
*	owner:		owning element.
*	formatted:	true when this form component is placed in a <pre></pre> tag.
* Returns:
*	a newly allocated word upon success. NULL on failure.
*****/
static XmHTMLWord*
TextAreaToWord(XmHTMLWidget html, XmHTMLObject *start, int *num_words,
	Dimension *width, Dimension *height, XmHTMLObjectTableElement owner,
	Boolean formatted)
{
	static XmHTMLForm *form_entry;
	XmHTMLWord *word;
	String text = NULL;
	Byte foo;
	Boolean i18n;

	*num_words = 0;
	*height = *width = 0;

	/* sanity check */
	if(start->attributes == NULL)
		return(NULL);

	/* get text between opening and closing <textarea>, if any */
	if(start->next->id == HT_ZTEXT)
		text = CopyText(html, start->next->element, True, &foo, False, &i18n);

	/* create new form entry. text will serve as the default content */
	if((form_entry = _XmHTMLFormAddTextArea(html, start->attributes,
		text)) == NULL)
	{
		if(text)
			free(text);
		return(NULL);
	}
	form_entry->data = owner;

	/* allocate new word for this form member */
	word = allocFormWord(html, form_entry, width, height, owner, formatted);

	_XmHTMLFullDebug(2, ("format.c: TextAreaToWord, word is %s, len is %i, "
		"width is %i, height is %i\n", word->word, word->len,
		word->width, word->height));

	*num_words = 1;
	return(word);
}

/*****
* Name:			indexToWord
* Return Type: 	XmHTMLWord
* Description: 	creates a prefix for numbered lists with the ISINDEX
*				attribute set.
* In:
*	html:		XmHTMLWidget id;
*	list_stack:	stack of all lists;
*	current...:	current list id;
*	owner:		owning element.
*	formatted:	true when this form component is placed in a <pre></pre> tag.
* Returns:
*	a newly allocated word.
* Note:
*	This routine creates the prefix based on the type and depth of the
*	current list. All types can be intermixed, so this routine is capable
*	of returning something like 1.A.IV.c.iii for a list nested five levels,
*	the first with type `1', second with type `A', third with type `I',
*	fourth with type `a' and fifth with type `i'.
*****/
static XmHTMLWord*
indexToWord(XmHTMLWidget html, listStack list_stack[], int current_list,
	XmHTMLObjectTableElement owner, Boolean formatted)
{
	static XmHTMLWord *word;
	int i;
	char index[128], number[128];	/* enough for a zillion numbers & depths */

	word = (XmHTMLWord*)calloc(1, sizeof(XmHTMLWord));

	(void)memset(&index, '\0', 128);
	for(i = 0; i < current_list; i++)
	{
		if(list_stack[i].type == HT_OL)
		{
			switch(list_stack[i].marker)
			{
				case XmMARKER_ALPHA_LOWER:
					sprintf(number, "%s.", ToAsciiLower(list_stack[i].level));
					break;
				case XmMARKER_ALPHA_UPPER:
					sprintf(number, "%s.", ToAsciiUpper(list_stack[i].level));
					break;
				case XmMARKER_ROMAN_LOWER:
					sprintf(number, "%s.", ToRomanLower(list_stack[i].level));
					break;
				case XmMARKER_ROMAN_UPPER:
					sprintf(number, "%s.", ToRomanUpper(list_stack[i].level));
					break;
				case XmMARKER_ARABIC:
				default:
					sprintf(number, "%i.", list_stack[i].level);
					break;
			}
			/* no buffer overflow */
			if(strlen(index) + strlen(number) > 128)
				break;
			strcat(index, number);
		}
	}

	/* fill in required fields */
	word->word = strdup(index);
	word->len  = strlen(index);
	word->self      = word;							/* unused */
	word->owner     = owner;						/* unused */
	word->font      = HTML_ATTR(default_font);		/* unused */
	word->spacing   = formatted ? 0 : TEXT_SPACE_NONE;
	word->type      = OBJ_TEXT;						/* unused */
	word->line_data = NO_LINE;						/* unused */

	return(word);
}

/*****
* Name: 		TextToPre
* Return Type: 	XmHTMLWord*
* Description: 	splits the given text into an array of preformatted lines
* In:
*	text:		text to split
*	num_words:	number of words in the given text. Filled upon return;
*	font:		font to use for this text.
* Returns:
*	an array of words. When allocation fails, this routine exits.
* Note:
*	the static var nchars is used to propagate the tab index to another
*	chunk of preformatted text if the current text is a block of preformatted
*	text with whatever formatting. It is only reset if an explicit newline
*	is encountered.
*****/
static XmHTMLWord*
TextToPre(String text, int *num_words, XmHTMLfont *font, Byte line_data,
	XmHTMLObjectTableElement owner, ToolkitAbstraction *tka, int tabwidth)
{
	int nwords, len, i, j, ntabs, max_width, in_word, size, nfeeds;
	static char *raw;
	static XmHTMLWord *words;
	static int nchars = 0;
	register char *chPtr, *start, *end;
#ifdef DEBUG
	int used;
#endif

	/* sanity check */
	if(text == NULL)
	{
		*num_words = 0;
		return(NULL);
	}

	_XmHTMLFullDebug(2, ("format.c: TextToPre, text in is:\n%s\n", text));

	chPtr = text;
	raw = NULL;

	/*****
	* compute how many words we have. A preformatted word is started
	* with a printing char and is terminated by either a newline or a
	* sequence of whitespaces. Multiple newlines are collapsed into a
	* single word where the height of the word indicates the number of
	* newlines to insert.
	*****/
	in_word = nwords = ntabs = 1;	/* fix 01/30/97-02, kdh */
	while(True)
	{
		switch(*chPtr)
		{
			/* tabs and single spaces are collapsed */
			case '\t':	/* horizontal tab */
			case ' ':
				if(in_word)
				{
					while(*chPtr != '\0' && (*chPtr == ' ' || *chPtr == '\t'))
					{
						if(*chPtr == '\t')
							ntabs++;	/* need to know how many to expand */
						chPtr++;
					}
					nwords++;
					in_word = False;
				}
				else
				{
					/* fix 03/23/97-01, kdh */
					if(*chPtr == '\t')
						ntabs++;	/* need to know how many to expand */
					chPtr++;
				}
				break;
			/* newlines reset the tab index and are collapsed */
			case '\n':
				while(*chPtr != '\0' && *chPtr == '\n')
					chPtr++;
				nwords++;	/* current word is terminated */
				nchars = 1;
				break;
			default:
				chPtr++;
				in_word = True;
				break;
		}
		if(*chPtr == '\0')
			break;
	}

	/* sanity check */
	if(nwords == 0)
	{
		*num_words = 0;
		return(NULL);
	}

	/* add an extra word and tab for safety */
	nwords++;	/* preformatted text with other formatting *needs* this */
	ntabs++;

	/* compute amount of memory to allocate */
	size = ((ntabs*tabwidth)+strlen(text)+1)*sizeof(char);

	raw = (char*)malloc(size);

	_XmHTMLDebug(2, ("format.c: TextToPre, allocated %i bytes\n", size));

	/* allocate memory for all words */
	words = (XmHTMLWord*)calloc(nwords, sizeof(XmHTMLWord));

	chPtr = text;
	end = raw;

	/*****
	* If the previous element is also part of a pre-formatted object
	* that doesn't contain a newline, we need to get it's length.
	* This is required for proper continuation of horizontal tabs.
	* If we do not do this, tabulation will be incorrect.
	*****/
	if(owner->prev->object_type == OBJ_PRE_TEXT)
	{
		XmHTMLWord lastw = owner->prev->words[owner->prev->n_words-1];

		if(!lastw.spacing)
		{
			/* prevent divide by zero */
			int sw = lastw.font->isp ? lastw.font->isp : 3;

			if(lastw.type == OBJ_IMG && lastw.image && lastw.image->width)
					nchars = lastw.image->width / sw;
			else if(lastw.type == OBJ_FORM && lastw.form && lastw.form->width)
					nchars = lastw.form->width / sw;
			else
				nchars = lastw.len;
		}
	}

#ifdef DEBUG
	used = 0;
#endif
	/* first filter out all whitespace and other non-printing characters */
	while(True)
	{
		switch(*chPtr)
		{
			case '\f':	/* formfeed, ignore */
			case '\r':	/* carriage return, ignore */
			case '\v':	/* vertical tab, ignore */
				chPtr++;
#ifdef DEBUG
				used++;
#endif
				break;
			case '\t':	/* horizontal tab */
				/* no of ``floating spaces'' to emulate a tab */
				len = ((nchars / tabwidth) + 1) * tabwidth;
				for(j = 0; j < (len - nchars); j++)
				{
					*end++ = ' ';		/* insert a tab */
#ifdef DEBUG
					used++;
#endif
				}
				nchars = len;
#ifdef DEBUG
				used++;
#endif
				chPtr++;
				break;
			/* newlines reset the tab index */
			case '\n':
				nchars = 0;	/* reset tab spacing index */
				/* fall thru */
			default:
				nchars++;
				*end++ = *chPtr++;
#ifdef DEBUG
				used++;
#endif
				break;
		}
		if(*chPtr == '\0')	/* terminate loop */
		{
			*end = '\0';
			break;
		}
	}
	_XmHTMLDebug(2, ("format.c: TextToPre, %i bytes actually used\n", used));

	/* Now go and fill all words */
	start = end = raw;
	max_width = i = len = 0;
	nfeeds = 0;

	while(True)
	{
		/* also pick up the last word! */
		if(*end == ' ' || *end == '\n' || *end == '\0')
		{
			if(*end)
			{
				/* skip past all spaces */
				while(*end != '\0' && *end != '\n' && *end == ' ')
				{
					end++;
					len++;
				}

				/*****
				* if this word is ended by a newline, remove the newline.
				* X doesn't know how to interpret them.
				* We also want to recognize multiple newlines, so we must
				* skip past them.
				*****/
				if(*end == '\n')
				{
					while(*end != '\0' && *end == '\n')
					{
						nfeeds++;
						*end++ = '\0';
					}
					/*****
					* Since the no of newlines to add is stored in a
					* Byte, we need to limit the no of newlines to the
					* max. value a Byte can have: 255 (= 2^8)
					*****/
					if(nfeeds > 255)
						nfeeds = 255;
				}
			}

			words[i].type      = OBJ_TEXT;
			words[i].self      = &words[i];
			words[i].word      = start;
			words[i].height    = font->height;
			words[i].owner     = owner;
			words[i].spacing   = (Byte)nfeeds;	/* no of newlines */
			words[i].font      = font;
			words[i].line_data = line_data;
			words[i].len       = len;
			words[i].width     = tka->TextWidth(font, words[i].word, len);
			start = end;
			i++;
			len = 0;
			nfeeds = 0;
		}
		if(*end == '\0')	/* terminate loop */
			break;
		end++;	/* move to the next char */
		len++;
	}

	_XmHTMLDebug(2, ("format.c: TexToPre, allocated %i words, %i actually "
		"used\n", nwords, i));

	/* total no of words */
	*num_words = i;
	return(words);
}

static XmHTMLWord*
BreakToWord(Dimension *height, XmHTMLfont *font, int linefeed,
	XmHTMLObjectTableElement owner)
{
	static XmHTMLWord *word;

	word = (XmHTMLWord*)calloc(1, sizeof(XmHTMLWord));

	word->type      = OBJ_BLOCK;
	word->self      = word;
	word->word      = (String)malloc(1);	/* needs an empty word */
	word->word[0]   = '\0';
	word->len       = 0;
	word->height    = font->height;			/* height of current font */
	word->owner     = owner;
	word->spacing   = TEXT_SPACE_NONE;		/* obviously no spacing */
	word->line_data = linefeed+1;	/* how much lines to break */
	word->font      = font;

	return(word);
}

static XmHTMLWord*
MakeDummyWord(Dimension *height, XmHTMLfont *font, Byte line_data,
	XmHTMLObjectTableElement owner)
{
	static XmHTMLWord *word;

	word = (XmHTMLWord*)calloc(1, sizeof(XmHTMLWord));

	word->type      = OBJ_TEXT;
	word->self      = word;
	word->word      = (String)malloc(1);		/* needs an empty word */
	word->word[0]   = '\0';
	word->len       = 0;
	word->height    = *height = font->height;	/* height of current font */
	word->owner     = owner;
	word->line_data = line_data;
	word->font      = font;
	/* an empty word acts as a single space */
	word->spacing   = TEXT_SPACE_LEAD|TEXT_SPACE_TRAIL;

	return(word);
}


/*****
* Name: 		SetTab
* Return Type: 	XmHTMLWord*
* Description: 	returns a XmHTMLWord with spaces required for a tab.
* In:
* Returns:
*	the tab.
*****/
static XmHTMLWord*
SetTab(int size, Dimension *height, XmHTMLfont *font,
	XmHTMLObjectTableElement owner, ToolkitAbstraction *tka)
{
	static XmHTMLWord *tab;
	static char *raw;

	/* The tab itself */
	raw = (char*)malloc((size+1)*sizeof(char));

	/* fill with spaces */
	(void)memset(raw, ' ', size);
	raw[size] = '\0'; /* NULL terminate */

	tab = (XmHTMLWord*)calloc(1, sizeof(XmHTMLWord));

	/* Set all text fields for this tab */
	tab->self      = tab;
	tab->word      = raw;
	tab->len       = size;
	tab->height    = *height = font->height;
	tab->width     = tka->TextWidth(font, raw, size);
	tab->owner     = owner;
	tab->spacing   = TEXT_SPACE_NONE;	/* a tab is already spacing */
	tab->font      = font;
	tab->type      = OBJ_TEXT;
	tab->line_data = NO_LINE;

	return(tab);
}

/*****
* Name: 		CopyText
* Return Type: 	String
* Description: 	copies the given text to a newly malloc'd buffer.
* In:
*	html:		XmHTMLWidget id;
*	text:		text to clean out.
*	formatted:	true when this text occurs inside <pre></pre>
*	text_data:	text option bits, spacing and such
*	expand_escapes:
*				True -> expand escape sequences in text. Only viable when
*				copying pre-formatted text (plain text documents are handled
*				internally as consisting completely of preformatted text for
*				which the escapes may not be expanded).
* Returns:
*	cleaned up text. Terminates if malloc fails.
*****/
static String
CopyText(XmHTMLWidget html, String text, Boolean formatted, Byte *text_data,
	Boolean expand_escapes, Boolean *i18n)
{
	static String ret_val;
	char *start = text;
	int len;
#ifdef I18N
	char *chPtr, *end;
	int n;
	*i18n = False;			/* assume no multibyte text yet */
#endif	/* I18N */

	/* sanity check */
	if(*text == '\0' || !strlen(text))
		return(NULL);

	/* preformatted text, just copy and return */
	if(formatted)
	{
		/* formatted text resets spacing */
		*text_data = TEXT_SPACE_NONE;
		ret_val    = strdup(text);
		/* expand all escape sequences in this text */
		if(expand_escapes)
			_XmHTMLExpandEscapes(ret_val, HTML_ATTR(bad_html_warnings));
		return(ret_val);
	}

	_XmHTMLFullDebug(2, ("format.c: CopyText, text in is:\n%s\n", text));

	/* initial length of full text */
	len = strlen(text);

	/*****
	* Check interword spacing.
	* If we've just had an explicit break, only check for possible trailing
	* spaces.
	*****/
	if(*text_data & TEXT_BREAK && isspace(text[len-1]))
		*text_data |= TEXT_SPACE_TRAIL;
	else
	{
		/* unset no-space bit */
		*text_data &= ~TEXT_SPACE_NONE;
		/*****
		* If first char is a space or the previous chunk had a trailing space,
		* this chunk gets a leading space. Else it's glueud to the previous
		* chunk.
		*****/
		if((isspace(*text) || *text_data & TEXT_SPACE_TRAIL))
			*text_data = TEXT_SPACE_LEAD;
		else
			*text_data &= ~TEXT_SPACE_LEAD;

		/* do we have a trailing space? */
		if(isspace(text[len-1]))
			*text_data |= TEXT_SPACE_TRAIL;
		else
			*text_data &= ~TEXT_SPACE_TRAIL;

		/* set no-space bit if we don't have any spaces */
		if(!(*text_data & TEXT_SPACE_TRAIL) && !(*text_data & TEXT_SPACE_LEAD))
			*text_data |= TEXT_SPACE_NONE;
	}

	/*****
	* Now remove leading/trailing spaces
	* very special case: spaces between different text formatting
	* elements must be retained
	*****/
	/* remove all leading space */
#ifdef I18N
	while(*start != '\0' && isspace(*start))
	{
		/* compute size of this character */
		n = mblen((char*)text, (size_t)(strlen(start)));
		if(n > 1)
			break;				/* not a space */
		if(n > 0)
			start = start + n;	/* a space, skip to next multibyte char */
		else
			break;				/* break on error */
	}

	/* remove all trailing space */
	chPtr = start;
	n     = mblen((char*)start, (size_t)(strlen(start)));
	end   = (char*)NULL;

	while(n > 0)
	{
		if(n == 1)
		{
			/* not a multibyte character */
			if(!isspace(*chPtr))
				end = (char*)NULL;	/* last char not a space, move to next */
			else if(end == NULL)
				end = chPtr;		/* possible end space */
		}
		else if(n > 1)
		{
			int z;
			end = (char*)NULL;		/* multibyte char, not a space */
			*i18n = True;			/* we have found multibyte text */
		}
		else
			break;					/* break on error */

		/* compute size of next char */
		n = mblen((char*)chPtr, (size_t)(strlen(chPtr)));
		if(n > 0)
		{
			chPtr += n;				/* move to next char */
		}
	}
#else
	while(*start != '\0' && isspace(*start))
		start++;

	/* remove all trailing space */
	len = strlen(start);
	while(len > 0 && isspace(start[len-1]))
		len--;

#endif /* I18N */

	/*****
	* Spaces *can* appear between different text formatting elements.
	* We want to retain this spacing since the above whitespace checking
	* only yields the current element, and does not take the previous text
	* element into account, *UNLESS* we just had a break.
	*
	* So when the current element doesn't have any leading or trailing spaces,
	* we use the spacing from the previous full whitespace element.
	*****/
	if(!len)
	{
		if(*text_data & TEXT_BREAK)
		{
			*text_data &= (~TEXT_BREAK & ~TEXT_SPACE_TRAIL);
			*text_data |= TEXT_SPACE_NONE;
		}
		return(NULL);
	}

	/* release break bit, no longer needed */
	*text_data &= ~TEXT_BREAK;

	/*****
	* We are a little bit to generous here: consecutive multiple whitespace
	* will be collapsed into a single space, so we may over-allocate.
	* Hey, better to overdo this than to have one byte to short ;-)
	*****/
	ret_val = (String)malloc((len+1)*sizeof(char));
	strncpy(ret_val, start, len);	/* copy it */
	ret_val[len] = '\0';			/* NULL terminate */

	/* expand all escape sequences in this text */
	if(expand_escapes)
		_XmHTMLExpandEscapes(ret_val, HTML_ATTR(bad_html_warnings));

#ifdef I18N
	if(*i18n == True)
		fprintf(stderr, "found multibyte text!\n");
#endif
	return(ret_val);
}

/*****
* Name: 		ParseBodyTags
* Return Type: 	void
* Description: 	checks the <BODY> element for additional tags
* In:
*	w:			HTML widget to check
*	data:		body element data.
* Returns:
*	nothing, but the HTML widget is updated to reflect the body stuff.
*****/
static void
ParseBodyTags(XmHTMLWidget html, XmHTMLObject *data)
{
	char *chPtr;
	Boolean bg_color_set = False;	/* flag for bodyImage substitution */

	/* Check for HTML4.0 body events */
	HTML_ATTR(body_events) = _XmHTMLCheckBodyEvents(html, data->attributes,
								&(HTML_ATTR(event_mask)));

	/* check all body color tags */
	if(HTML_ATTR(body_colors_enabled) && data->attributes != NULL)
	{
		Boolean doit = True;

		if((chPtr = _XmHTMLTagGetValue(data->attributes, "text")))
		{
			if(HTML_ATTR(strict_checking))
				doit = _XmHTMLConfirmColor32(chPtr);

			if(doit)
				HTML_ATTR(body_fg) = _XmHTMLGetPixelByName(html, chPtr,
					HTML_ATTR(body_fg_save));
			free(chPtr);

			/* also set as foreground for the entire widget */
			MGR_ATTR(foreground) = HTML_ATTR(body_fg);
		}

		if(doit && (chPtr = _XmHTMLTagGetValue(data->attributes, "bgcolor")))
		{
			bg_color_set = True;

			if(HTML_ATTR(strict_checking))
				doit = _XmHTMLConfirmColor32(chPtr);

			/* only change if we had success */
			if(doit)
			{
				HTML_ATTR(body_bg) = _XmHTMLGetPixelByName(html, chPtr,
					HTML_ATTR(body_bg_save));

				/* also set as background for the entire widget */
				CORE_ATTR(background_pixel) = HTML_ATTR(body_bg);

				/* get new values for top, bottom & highlight */
				XmHTMLTkaRecomputeColors(html, HTML_ATTR(body_bg));
			}

			free(chPtr);
		}

		if(doit && (chPtr = _XmHTMLTagGetValue(data->attributes, "link")))
		{
			if(HTML_ATTR(strict_checking))
				doit = _XmHTMLConfirmColor32(chPtr);

			if(doit)
				HTML_ATTR(anchor_fg) = _XmHTMLGetPixelByName(html, chPtr,
					HTML_ATTR(anchor_fg_save));
			free(chPtr);
		}

		if(doit && (chPtr = _XmHTMLTagGetValue(data->attributes, "vlink")))
		{
			if(HTML_ATTR(strict_checking))
				doit = _XmHTMLConfirmColor32(chPtr);
			if(doit)
				HTML_ATTR(anchor_visited_fg) = _XmHTMLGetPixelByName(html,
					chPtr, HTML_ATTR(anchor_visited_fg_save));
			free(chPtr);
		}

		if(doit && (chPtr = _XmHTMLTagGetValue(data->attributes, "alink")))
		{
			if(HTML_ATTR(strict_checking))
				doit = _XmHTMLConfirmColor32(chPtr);
			if(doit)
				HTML_ATTR(anchor_activated_fg) = _XmHTMLGetPixelByName(html,
					chPtr, HTML_ATTR(anchor_activated_fg_save));
			free(chPtr);
		}
		/*****
		* an invalid color spec, ignore them all together and revert to
		* saved settings
		*****/
		if(doit == False)
		{
			/* first check if we changed the background color */
			if(CORE_ATTR(background_pixel) != HTML_ATTR(body_bg_save))
			{
				HTML_ATTR(body_fg)          = HTML_ATTR(body_fg_save);
				HTML_ATTR(body_bg)          = HTML_ATTR(body_bg_save);
				MGR_ATTR(foreground)	    = HTML_ATTR(body_fg);
				CORE_ATTR(background_pixel) = HTML_ATTR(body_bg);

				/* restore values for top, bottom & highlight */
				XmHTMLTkaRecomputeColors(html, HTML_ATTR(body_bg));
			}

			HTML_ATTR(body_fg)            = HTML_ATTR(body_fg_save);
			HTML_ATTR(body_bg)            = HTML_ATTR(body_bg_save);
			HTML_ATTR(anchor_fg)          = HTML_ATTR(anchor_fg_save);
			HTML_ATTR(anchor_visited_fg)  = HTML_ATTR(anchor_visited_fg_save);
			HTML_ATTR(anchor_activated_fg)=HTML_ATTR(anchor_activated_fg_save);
			MGR_ATTR(foreground)          = HTML_ATTR(body_fg);
			bg_color_set = False;
		}
	}

	/* Check background image spec. First invalidate any existing body image */
	if(HTML_ATTR(body_image))
		HTML_ATTR(body_image->options) |= IMG_ORPHANED;
	HTML_ATTR(body_image) = (XmHTMLImage*)NULL;
	HTML_ATTR(body_image_url) = NULL;

	/*
	* *ALWAYS* load the body image if we want the SetValues method to
	* behave itself.
	*/
	if(data->attributes &&
		(chPtr = _XmHTMLTagGetValue(data->attributes, "background")))
	{
		_XmHTMLLoadBodyImage(html, chPtr);
		/* store document's body image location */
		if(HTML_ATTR(body_image))
			HTML_ATTR(body_image_url) = HTML_ATTR(body_image->url);
		free(chPtr);
	}
	/*
	* Use default body image if present *and* if no background color
	* has been set.
	*/
	else if(!bg_color_set && HTML_ATTR(def_body_image_url))
		_XmHTMLLoadBodyImage(html, HTML_ATTR(def_body_image_url));

	/*****
	* Now nullify it if we aren't to show the background image.
	* makes sense huh? (the list of images is a global widget resource so the
	* storage occupied by this unused image is freed when all document
	* data is freed).
	*****/
	if(!HTML_ATTR(images_enabled) || !HTML_ATTR(body_images_enabled))
	{
		if(HTML_ATTR(body_image))
			HTML_ATTR(body_image->options) |= IMG_ORPHANED;
		HTML_ATTR(body_image) = NULL;
	}
	/*****
	* When a body image is present it is very likely that a highlight
	* color based upon the current background actually makes an anchor
	* invisible when highlighting is selected. Therefore we base the
	* highlight color on the activated anchor background when we have a body
	* image, and on the document background when no body image is present.
	*****/
	if(HTML_ATTR(body_image))
		XmHTMLTkaRecomputeHighlightColor(html, HTML_ATTR(anchor_activated_fg));
	else
		XmHTMLTkaRecomputeHighlightColor(html, HTML_ATTR(body_bg));
}

static void
FillBullet(XmHTMLWidget html, XmHTMLObjectTableElement owner,
	ToolkitAbstraction *tka)
{
	Dimension radius;
	char number[128];	/* large enough buffer for a zillion numbers */
	XmHTMLfont *font = HTML_ATTR(default_font);
	String prefix;

	/*****
	* x-offset for any marker and radius for a bullet or length of a
	* side for a square marker.
	*****/
	radius = (Dimension)(0.5*(font->m_lbearing + font->m_rbearing));

	if(owner->marker == XmMARKER_DISC || owner->marker == XmMARKER_SQUARE ||
		owner->marker == XmMARKER_CIRCLE)
	{
		/* y-offset for this marker */
		owner->height = (Dimension)(0.5*font->lineheight + 0.25*radius);
		owner->width = radius;
	}
	else
	{
		/*****
		* If we have a word, this is an ordered list for which the index
		* should be propageted.
		*****/
		if(owner->words)
			prefix = owner->words[0].word;
		else
			prefix = "";
		switch(owner->marker)
		{
			case XmMARKER_ALPHA_LOWER:
				sprintf(number, "%s%s.", prefix,
					ToAsciiLower(owner->list_level));
				break;
			case XmMARKER_ALPHA_UPPER:
				sprintf(number, "%s%s.", prefix,
					ToAsciiUpper(owner->list_level));
				break;
			case XmMARKER_ROMAN_LOWER:
				sprintf(number, "%s%s.", prefix,
					ToRomanLower(owner->list_level));
				break;
			case XmMARKER_ROMAN_UPPER:
				sprintf(number, "%s%s.", prefix,
					ToRomanUpper(owner->list_level));
				break;
			case XmMARKER_ARABIC:
			default:
				sprintf(number, "%s%i.", prefix, owner->list_level);
				break;
		}
		owner->text  = strdup(number);
		owner->len   = strlen(number);
		owner->width = radius + tka->TextWidth(font, owner->text, owner->len);
		owner->height = HTML_ATTR(default_font)->height;
	}
}

/*****
* Name: 		CheckLineFeed
* Return Type: 	int
* Description: 	checks wether the requested newline is honored.
* In:
*	op:		newline to add.
*	force:		add the requested newline anyway
*	text_data:	current trailing/leading spacing
* Returns:
*	computed vertical pixel offset.
* Note:
*	any of CLEAR_NONE, CLEAR_SOFT or CLEAR_HARD
*****/
static int
CheckLineFeed(int op, Boolean force, Byte *text_data)
{
	static int prev_state = CLEAR_NONE;
	int ret_val = op;

	/* anything except no-op clears spacing */
	if(op != CLEAR_NONE)
	{
		*text_data &= (~TEXT_SPACE_LEAD & ~TEXT_SPACE_TRAIL);
		*text_data |= TEXT_SPACE_NONE;
	}

	if(force)
	{
		prev_state = op;
		return(ret_val);
	}

	/* multiple soft and hard returns are never honored */
	switch(op)
	{
		case CLEAR_HARD:
			if(prev_state == CLEAR_SOFT)
			{
				ret_val = CLEAR_SOFT;
				prev_state = CLEAR_HARD;
				break;
			}
			if(prev_state == CLEAR_HARD)
			{
				/* unchanged */
				ret_val = CLEAR_NONE;
				break;
			}
			prev_state = ret_val = op;
			break;
		case CLEAR_SOFT:
			if(prev_state == CLEAR_SOFT)
			{
				ret_val = CLEAR_NONE;
				prev_state = CLEAR_SOFT;
				break;
			}
			if(prev_state == CLEAR_HARD)
			{
				/* unchanged */
				ret_val = CLEAR_NONE;
				break;
			}
			ret_val = prev_state = op;
			break;
		case CLEAR_NONE:
			ret_val = prev_state = op;
			break;
	}
	return(ret_val);
}

/*****
* Name:			tableCheckProperties
* Return Type: 	TableProperties
* Description: 	scans a table element for common properties (border,
*				alignment, background and border styles).
* In:
*	html:		XmHTMLWidget id;
*	attributes:	attributes to be checked;
*	parent:		properties of parent table element. Properties not found
*				in the attributes are inherited from this parent.
* Returns:
*	a new property.
*****/
static TableProperties*
tableCheckProperties(XmHTMLWidget html, String attributes,
	TableProperties *parent, Alignment halign, Pixel bg)
{
	TableProperties prop;
	static TableProperties *prop_ret;
	String chPtr;

	prop_ret = (TableProperties*)malloc(sizeof(TableProperties));
	memset(prop_ret, 0, sizeof(TableProperties));

	if(parent)
		memcpy(&prop, parent, sizeof(TableProperties));
	else
	{
		/* defaults assume a table without any borders */
		prop.border   = -1;
		prop.halign   = halign;		 		/* unused */
		prop.valign   = XmVALIGN_TOP;		/* contents on top */
		prop.bg       = bg;					/* propagate */
		prop.bg_image = NULL;
		prop.framing  = TFRAME_VOID;		/* no border */
		prop.ruling   = TRULE_NONE;			/* no ruling */
		prop.nowrap   = False;				/* wrap long lines */
	}

	/* Check for possible attributes */
	if(attributes)
	{
		/*****
		* Horizontal alignment is only inherited through the halign argument:
		* the align attribute on the table tag applies to the table in a whole,
		* not to any of it's members.
		*****/
		prop_ret->halign   = _XmHTMLGetHorizontalAlignment(attributes, halign);
		prop_ret->valign   = _XmHTMLGetVerticalAlignment(attributes,
								prop.valign);
		prop_ret->nowrap   = _XmHTMLTagCheck(attributes, "nowrap");

		/*****
		* Border value. If -1 is returned, check for the presence of the word
		* ``border'' in the attributes. If it exists, we assume a non-zero
		* border width.
		*****/
		prop_ret->border   = _XmHTMLTagGetNumber(attributes, "border",
								prop.border);
		/* if the word ``border'' is present, use default border width */
		if(prop_ret->border == -1 && _XmHTMLTagCheck(attributes, "border"))
			prop_ret->border = XmHTML_DEFAULT_TABLE_BORDERWIDTH;

		/*****
		* Framing applies per-table. If border is non-zero, the default is
		* to render a fully framed table.
		* If a TFRAME_VOID is returned, discard the border width.
		*****/
		prop_ret->framing  = _XmHTMLGetFraming(attributes,
							prop_ret->border > 0 ? TFRAME_BOX : prop.framing);
		if(prop_ret->framing == TFRAME_VOID)
			prop_ret->border = 0;

		/*****
		* Ruling applies per-cell. If border is non-zero, the default is to
		* render full borders around this cell.
		* If a TRULE_NONE is returned, discard the border width.
		*****/
		prop_ret->ruling   = _XmHTMLGetRuling(attributes,
							prop_ret->border ? TRULE_ALL : prop.ruling);

		if(prop_ret->ruling == TRULE_NONE)
			prop_ret->border = 0;

		/*****
		* only pick up background color if we are allowed to honor this attrib
		*****/
		if(HTML_ATTR(allow_color_switching) &&
			(chPtr = _XmHTMLTagGetValue(attributes, "bgcolor")) != NULL)
		{
			Boolean doit = True;
			if(HTML_ATTR(strict_checking))
				doit = _XmHTMLConfirmColor32(chPtr);
			if(doit)
				prop_ret->bg = _XmHTMLGetPixelByName(html, chPtr, prop.bg);
			free(chPtr);
		}
		else
			prop_ret->bg = prop.bg;

		/* table background image? */
		if((chPtr = _XmHTMLTagGetValue(attributes, "background")))
		{
			String buf;
			Dimension width, height;
			XmHTMLImage *image;

			/* kludge so _XmHTMLNewImage recognizes it */
			buf = malloc(strlen(chPtr)+7);
			sprintf(buf, "src=\"%s\"", chPtr);

			/* load it */
			if((image = _XmHTMLNewImage(html, buf, &width, &height)) != NULL)
			{
				/* animations are not allowed as background images */
				if(ImageIsAnim(image))
					image = NULL;
				/* and we sure won't have the default image as background */
				else if(ImageIsInternal(image))
					image = NULL;
			}
			prop_ret->bg_image = image;
			free(buf);
			free(chPtr);
		}
		else
			prop_ret->bg_image = prop.bg_image;
	}
	else
	{
		/* nop, no attributes, inherit from parent */
		prop_ret->halign   = halign;
		prop_ret->valign   = prop.valign;
		prop_ret->nowrap   = False;
		prop_ret->border   = prop.border;
		prop_ret->bg       = prop.bg;
		prop_ret->bg_image = prop.bg_image;
		if((prop_ret->framing = prop_ret->border > 0 ?
			TFRAME_BOX : prop.framing) == TFRAME_VOID)
			prop_ret->border = 0;
		if((prop_ret->ruling = prop_ret->border ?
			TRULE_ALL : prop.ruling) == TRULE_NONE)
			prop_ret->border = 0;
	}
	return(prop_ret);
}

/*****
* Name:			tableOpen
* Return Type: 	XmHTMLTable
* Description: 	opens a new table.
* In:
*	html;		XmHTMLWidget id;
*	parent:		parent table. Storage for nested tables is always allocated
*				in this table. It is NULL when a truely new table is needed.
*	start:		start of objects contained in this table.
*	obj:		XmHTMLObject starting this table.
* Returns:
*	a newly opened table.
*****/
static XmHTMLTable*
tableOpen(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement start, XmHTMLObject *obj, Alignment *halign,
	Pixel *bg)
{
	static XmHTMLTable *table;
	XmHTMLTable *parent_table;
	XmHTMLObject *tmp;
	int nrows = 0;				/* no of rows in table				*/
	int depth = 0;
	int nchilds = 0;			/* no of table childs in this table */
	Alignment caption_position = XmVALIGN_TOP; /* where is the caption?	*/
	Boolean have_caption = False;

	if(parent)
	{
		/* get to the absolute parent of this table */
		parent_table = parent;
		while(parent_table->parent != NULL)
			parent_table = parent_table->parent;

		/* get correct ptr */
		parent_table = &(parent_table->childs[0]);

		/* sanity check */
		if(parent_table->lastchild+1 == parent_table->nchilds)
		{
			_XmHTMLError(__WFUNC__(html, "tableOpen"),
				"Bad table count!!!");
		}
		/* get next available table */
		parent_table->lastchild++;
		table = &(parent_table->childs[parent_table->lastchild]);
	}
	else
	{
		table = (XmHTMLTable*)calloc(1, sizeof(XmHTMLTable));
	}

	/*****
	* Get table attributes.
	* If attributes are present, set all default values to zero.
	* If no attributes are present, use the default values specified in
	* include/common/XmHTMLconf.h
	*****/
	if(obj->attributes)
	{
		table->width    = _XmHTMLTagCheckNumber(obj->attributes, "width", 0);
		table->hmargin  = _XmHTMLTagGetNumber(obj->attributes, "cellspacing",
							0);
		table->hpadding = _XmHTMLTagGetNumber(obj->attributes, "cellpadding",
							0);
		table->ncols    = _XmHTMLTagGetNumber(obj->attributes, "cols", 0);

		/* no negative margin, padding or columns */
		if(table->hmargin < 0)
			table->hmargin = 0;
		if(table->hpadding < 0)
			table->hpadding = 0;
		if(table->ncols < 0)
			table->ncols = 0;

		/*****
		* Rowspacing and rowpadding are non-standard attributes. Use
		* them if they're present, use cell margins otherwise.
		*****/
		if(_XmHTMLTagCheck(obj->attributes, "rowspacing"))
			table->vmargin  = _XmHTMLTagGetNumber(obj->attributes,
								"rowspacing", 0);
		else
			table->vmargin  = table->hmargin;
		if(_XmHTMLTagCheck(obj->attributes, "rowpadding"))
			table->vpadding = _XmHTMLTagGetNumber(obj->attributes,
								"rowpadding", 0);
		else
			table->vpadding = table->hpadding;
	}
	else
	{
		table->width    = 0;
		table->hmargin  = XmHTML_DEFAULT_CELLSPACING;
		table->vmargin  = XmHTML_DEFAULT_ROWSPACING;
		table->hpadding = XmHTML_DEFAULT_CELLPADDING;
		table->vpadding = XmHTML_DEFAULT_ROWPADDING;
		table->ncols    = 0;
	}
	table->start    = start;	/* starting object */
	table->owner    = start;	/* owning object */
	table->parent	= NULL;		/* parent table */

	/* Nested tables do not inherit their parent table properties */
	table->props = tableCheckProperties(html, obj->attributes,
		NULL, *halign, *bg);

	/* set return alignment */
	*halign = table->props->halign;

	/* set return background */
	*bg = table->props->bg;

	/* count how many rows this table has */
	for(tmp = obj->next; tmp != NULL; tmp = tmp->next)
	{
		/* check for end of table and child tables */
		if(tmp->id == HT_TABLE)
		{
			if(tmp->is_end)
			{
				if(depth == 0)
					break;
				else
					depth--;
			}
			else	/* new table opens */
			{
				depth++;
				nchilds++;
			}
		}
		/*****
		* only count a row when it belongs to the top-level table.
		* A caption is considered a special row that spans the entire table
		* and has only one cell: the row itself.
		*****/
		if((tmp->id == HT_TR || tmp->id == HT_CAPTION) && depth == 0 &&
			!tmp->is_end)
		{
			if(tmp->id == HT_CAPTION)
			{
				/*****
				* see where the caption should be inserted: as the first
				* or last row for this table.
				*****/
				String chPtr;
				if(tmp->attributes == NULL ||(chPtr =
					_XmHTMLTagGetValue(tmp->attributes, "align")) == NULL)
					caption_position = XmVALIGN_TOP;
				else
				{
					if(!(strcasecmp(chPtr, "bottom")))
						caption_position = XmVALIGN_BOTTOM;
					else
						caption_position = XmVALIGN_TOP;
					free(chPtr);
				}
				have_caption = True;
			}
			nrows++;
		}
	}
	/* sanity, should never happen */
	if(!nrows)
	{
		_XmHTMLWarning(__WFUNC__(html, "tableOpen"),
			"Got an empty table: no rows found");
		free(table->props);
		free(table);
		return(NULL);
	}

	/* allocate all rows for this table */
	table->rows = (TableRow*)calloc(nrows, sizeof(TableRow));
	table->nrows = nrows;
	table->lastrow = 0;

	/* set caption ptr. */
	if(have_caption)
	{
		if(caption_position == XmVALIGN_TOP)
		{
			table->caption = &(table->rows[0]);
			table->lastrow = 1;
		}
		else
			table->caption = &(table->rows[nrows-1]);
	}

	/* The master table contains all tables */
	if(parent == NULL)
	{
		nchilds++;
		table->childs = (XmHTMLTable*)calloc(nchilds, sizeof(XmHTMLTable));
		table->nchilds = nchilds;
		table->childs[0] = *table;		/* we are the first table */
		table->lastchild = 0;
	}
	else
	{
		table->childs = (XmHTMLTable*)NULL;
		table->nchilds = 0;
		table->lastchild = 0;
		/* set parent table */
		table->parent = parent;
	}

	/* and set as table in the element given to us */
	start->table = table;

	return(table);
}

/*****
* Name:			tableClose
* Return Type: 	int
* Description: 	performs required table wrapup actions
* In:
*	html:		XmHTMLWidget id;
*	parent:		parent table;
*	end:		last object to be used by this table;
* Returns:
*	-1 when this was the last table to be closed, a 0 or a positive integer
*	when there are still tables open.
*****/
static XmHTMLTable*
tableClose(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement end)
{
	/* current table */
	XmHTMLTable *real_table, *table = parent;
	int i, ncols = 0;

	/* sanity */
	if(parent == NULL)
		return(NULL);

#if 0
	/* bad hack */
	real_table = parent->owner->table;
	real_table->start = parent->owner->next;
	real_table->end   = end;
#endif

	/* pick up correct ptr */
	if(table->parent == NULL)
		table = &(parent->childs[0]);

	/* deal with empty tables */
#if 0
	table->start = table->start->next ? table->start->next : end;
#endif
	table->start = table->start->next ? table->start : end;
	table->end   = end;

	/*****
	* Sanity Check: check all cells in search of a rowspan attribute. If
	* we detect a rowspan in the *last* cell of a row, we must add a bogus
	* cell to this row. If we don't do this, any cells falling in this row
	* will be skipped, causing text to disappear (at the least, in the worst
	* case it will cause a crash).
	*****/

	/* See how many columns we have (if not already set by the COLS attr.) */
	for(i = 0; i < table->nrows; i++)
	{
		if(ncols < table->rows[i].ncells)
			ncols = table->rows[i].ncells;
	}
	if(ncols > table->ncols)
		table->ncols = ncols;

	/* move to current table */
	return(table->parent);
}

/*****
* Name:			tableOpenCaption
* Return Type: 	void
* Description: 	adds a caption to the given table.
* In:
*	html:		XmHTMLWidget id;
*	parent:		parent table;
*	start:		start of objects contained in this caption.
*	obj:		XmHTMLObject starting this caption.
* Returns:
*	nothing, but upon return the current table will have a caption.
*****/
static void
tableOpenCaption(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement start, XmHTMLObject *obj,
	Pixel *bg)
{
	XmHTMLTable *table = parent;
	TableRow *caption;
	TableCell *cell;

	/* sanity */
	if(parent == NULL)
		return;

	if(table->parent == NULL)
		table = &(parent->childs[0]);

	/* get caption */
	caption = table->caption;

	/* only one caption allowed */
	if(caption->lastcell)
		return;

	/*****
	* Get properties for this caption.
	* The global table properties are *never* propagated since, officially,
	* a Caption doesn't have any attributes...
	*****/
	caption->props = tableCheckProperties(html, obj->attributes,
						NULL, HTML_ATTR(default_halign), *bg);

	/* starting object */
	caption->start = start;
	caption->owner = start;

	/* set parent table */
	caption->parent = table;

	/* one cell: the caption itself */
	caption->cells = (TableCell*)calloc(1, sizeof(TableCell));
	caption->ncells   = 1;
	caption->lastcell = 1;

	/* fill in used fields */
	cell = &(caption->cells[0]);

	cell->header = False;	/* unused */
	cell->width = 0;		/* unused */
	cell->height = 0;		/* unused */
	cell->rowspan = 1;		/* unused */
	cell->colspan = 0;		/* spans the full table width */

	/* get properties for this cell */
	cell->props = tableCheckProperties(html, obj->attributes,
						NULL, caption->props->halign,
						caption->props->bg);
	/* set return background */
	*bg = caption->props->bg;

	/* starting object */
	cell->start = start;
	cell->owner = start;

	/* ending object unknown */
	cell->end = NULL;

	/* set parent caption */
	cell->parent = caption;

	/* all done */
}

/*****
* Name:			tableCloseCaption
* Return Type: 	int
* Description: 	performs required caption wrapup actions
* In:
*	html:		XmHTMLWidget id;
*	parent:		parent table;
*	end:		last object to be used by this caption;
* Returns:
*	nothing.
*****/
static void
tableCloseCaption(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement end)
{
	XmHTMLTable *table = parent;
	TableRow *caption;
	TableCell *cell;

	/* sanity */
	if(parent == NULL)
		return;

	if(table->parent == NULL)
		table = &(parent->childs[0]);

	caption = table->caption;

	/* sanity */
	if(caption->ncells == 0)
		return;

	cell = &(caption->cells[0]);

	/* deal with empty cells */
	cell->start = cell->start->next ? cell->start->next : end;
	cell->end   = end;
}

/*****
* Name:			tableOpenRow
* Return Type: 	void
* Description: 	adds a row to the current table.
* In:
*	html:		XmHTMLWidget id;
*	parent:		parent table;
*	start:		start of objects contained in this row.
*	obj:		XmHTMLObject starting this row, used to compute no of cells
*				in a row.
* Returns:
*	Nothing, but a new row has been prepared in the current table.
*****/
static void
tableOpenRow(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement start, XmHTMLObject *obj, Alignment *halign,
	Pixel *bg)
{
	XmHTMLTable *table = parent;
	TableRow *row;
	XmHTMLObject *tmp;
	int ncells = 0;
	int depth  = 0;

	/* sanity */
	if(parent == NULL)
		return;

	if(table->parent == NULL)
		table = &(parent->childs[0]);

	/* sanity */
	if(table->lastrow == table->nrows)
	{
		_XmHTMLError(__WFUNC__(html, "tableRowOpen"),
			"Bad tablerow count!!!");
	}

	/* get next available row in this table */
	row = &(table->rows[table->lastrow]);

	/* get properties for this row */
	row->props = tableCheckProperties(html, obj->attributes,
						table->props, HTML_ATTR(default_halign), *bg);
	/* set return alignment */
	*halign = row->props->halign;

	/* set return background */
	*bg = row->props->bg;

	/* starting object */
	row->start = start;
	row->owner = start;

	/* set parent table */
	row->parent = table;

	/* count how many cells this row has */
	for(tmp = obj->next; tmp != NULL; tmp = tmp->next)
	{
		/* check for end of row and child rows (in child tables) */
		if(tmp->id == HT_TR)
		{
			if(tmp->is_end)
			{
				if(depth == 0)
					break;
				else
					depth--;
			}
			else	/* new row opens */
				depth++;
		}
		/* only count a cell when it belongs to the top-level row */
		if((tmp->id == HT_TH || tmp->id == HT_TD) && depth == 0 && !tmp->is_end)
			ncells++;
	}
	/* empty rows don't have cells */
	if(ncells)
		row->cells = (TableCell*)calloc(ncells, sizeof(TableCell));
	else	/* allocate an empty cell */
		row->cells = (TableCell*)calloc(1, sizeof(TableCell));
	row->ncells   = ncells;
	row->lastcell = 0;

	/* move to next available row */
	table->lastrow++;
}

/*****
* Name:			tableCloseRow
* Return Type: 	int
* Description: 	performs required row wrapup actions
* In:
*	html:		XmHTMLWidget id;
*	parent:		parent table;
*	end:		last object to be used by this row;
* Returns:
*	nothing.
*****/
static void
tableCloseRow(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement end)
{
	XmHTMLTable *table = parent;
	TableRow *row;
	int i, ncols = 0;

	/* sanity */
	if(parent == NULL)
		return;

	if(table->parent == NULL)
		table = &(parent->childs[0]);

	/* sanity */
	if(table->lastrow == 0)
	{
		_XmHTMLWarning(__WFUNC__(html, "tableCloseRow"),
			"Internal Error: zero row count in table");
		row = &(table->rows[table->lastrow]);
	}
	else /* get current row in this table */
		row = &(table->rows[table->lastrow-1]);

	/*****
	* Count how many columns this row has (including cells spanning multiple
	* columns).
	*****/
	for(i = 0; i < row->ncells; i++)
		ncols += row->cells[i].colspan;

	if(ncols > table->ncols)
		table->ncols = ncols;

	/*****
	* Empty rows (<tr> followed by another <tr> and only whitespace between
	* them) will loose their starting point as start->next will still be
	* empty: end has not yet been inserted in the ObjectTable.
	*****/
	row->start = (row->start->next ? row->start->next : end);
	row->end   = end;

#ifdef DEBUG
	if(row->start == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "tableCloseRow"), "Row %i lost"
			"it's starting point. (near line %i in input)\n",
			table->lastrow - 1, row->owner->object->line);
	}
#endif
}

/*****
* Name:			tableOpenCell
* Return Type: 	void
* Description: 	adds a cell to the current row in the current table.
* In:
*	html:		XmHTMLWidget id;
*	parent:		parent table;
*	start:		start of objects contained in this cell;
*	obj:		XmHTMLObject starting this cell;
* Returns:
*	Nothing, but a new cell has been prepared in the current row.
*****/
static void
tableOpenCell(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement start, XmHTMLObject *obj, Alignment *halign,
	Pixel *bg)
{
	XmHTMLTable *table = parent;
	TableRow *row;
	TableCell *cell;

	/* sanity */
	if(parent == NULL)
		return;

	if(table->parent == NULL)
		table = &(parent->childs[0]);

	/* more sanity */
	if(table->lastrow == 0)
	{
		_XmHTMLWarning(__WFUNC__(html, "tableOpenCell"),
			XMHTML_MSG_42, obj->line);
		return;
	}

	/* get current row in this table */
	row = &(table->rows[table->lastrow-1]);

	/* sanity */
	if(row->lastcell == row->ncells)
	{
		_XmHTMLError(__WFUNC__(html, "tableCellOpen"),
			"Internal Error: Bad tablerow cell count!!!");
	}

	/* get next available cell in this row */
	cell = &(row->cells[row->lastcell]);

	/* get cell-specific properties */
	cell->header = (obj->id == HT_TH ? True : False);
	if(obj->attributes)
	{
		cell->width   = _XmHTMLTagCheckNumber(obj->attributes, "width", 0);
		cell->height  = _XmHTMLTagCheckNumber(obj->attributes, "height", 0);
		cell->rowspan = _XmHTMLTagGetNumber(obj->attributes, "rowspan", 1);
		cell->colspan = _XmHTMLTagGetNumber(obj->attributes, "colspan", 1);
	}
	else
	{
		cell->width   = 0;
		cell->height  = 0;
		cell->rowspan = 1;
		cell->colspan = 1;
	}

	/* [row/cell]span = 0 : span entire table in requested direction */
	if(cell->rowspan <= 0 || cell->rowspan > table->nrows)
		cell->rowspan = table->nrows;

	/*****
	* colspan <= 0 gets handled in SetTable when we now how many columns
	* this table has
	*****/

	/* get global properties for this cell */
	cell->props = tableCheckProperties(html, obj->attributes,
						row->props, row->props->halign, *bg);
	/* set return alignment */
	*halign = cell->props->halign;

	/* set return background */
	*bg = cell->props->bg;

	/* starting object */
	cell->start = start;
	cell->owner = start;

	/* set parent row */
	cell->parent = row;

	/* move to next available cell */
	row->lastcell++;
}

/*****
* Name:			tableCloseCell
* Return Type: 	int
* Description: 	performs required cell wrapup actions
* In:
*	html:		XmHTMLWidget id;
*	parent:		parent table;
*	end:		last object to be used by this cell;
* Returns:
*	nothing.
*****/
static void
tableCloseCell(XmHTMLWidget html, XmHTMLTable *parent,
	XmHTMLObjectTableElement end)
{
	XmHTMLTable *table = parent;
	TableRow *row;
	TableCell *cell;

	/* sanity */
	if(parent == NULL)
		return;

	if(table->parent == NULL)
		table = &(parent->childs[0]);

	/* sanity */
	if(table->lastrow == 0)
	{
		_XmHTMLWarning(__WFUNC__(html, "tableCloseCell"),
			"Internal Error: zero row count in table");
		row = &(table->rows[table->lastrow]);
	}
	else /* get current row in this table */
		row = &(table->rows[table->lastrow-1]);

	/* sanity */
	if(row->lastcell == 0)
	{
		_XmHTMLWarning(__WFUNC__(html, "tableCloseCell"),
			"Internal Error: zero row cell count (row %i in table)",
			table->lastrow-1);
		if((cell = &(row->cells[row->lastcell])) == NULL)
			return;
	}
	else /* get current cell in this row */
		cell = &(row->cells[row->lastcell-1]);

	/*****
	* Empty cells (<td> followed by another <td> and only whitespace between
	* them) will loose their starting point as start->next will still be
	* empty: end has not yet been inserted in the ObjectTable.
	*****/
	cell->start = (cell->start->next ? cell->start->next : end);
	cell->end   = end;

#ifdef DEBUG
	if(cell->start == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "tableCloseCell"), "Cell %i (row %i) "
			"lost it's start point. (near line %i in input)\n",
			row->lastcell - 1, table->lastrow - 1, row->owner->object->line);
	}
#endif
}

#define PUSH_COLOR(WIDGET) { \
	char *chPtr; \
	/* check for color spec */ \
	StackPush(fg_color_stack, fg); \
	if(temp->attributes && \
		(chPtr = _XmHTMLTagGetValue(temp->attributes, "color")) != NULL) \
	{ \
		Boolean doit = True; \
		if(HTML_ATTR(strict_checking)) \
			doit = _XmHTMLConfirmColor32(chPtr); \
		if(doit) fg = _XmHTMLGetPixelByName(html, chPtr, fg); \
		free(chPtr); \
	} \
}

#define CHECK_LINE { \
	if(element_data & ELE_ANCHOR) { \
		if(element_data & ELE_ANCHOR_TARGET) \
			line_data = HTML_ATTR(anchor_target_line); \
		else if(element_data & ELE_ANCHOR_VISITED) \
			line_data = HTML_ATTR(anchor_visited_line); \
		else \
			line_data = HTML_ATTR(anchor_line); \
	} \
	/* ignore <u> for anchors */ \
	else { \
		if(element_data & ELE_UNDERLINE) \
			line_data  = LINE_SOLID | LINE_UNDER; \
	} \
	/* check strikeout flag */ \
	if(element_data & ELE_STRIKEOUT) \
		line_data |= LINE_STRIKE; \
}

/********
****** Private XmHTML Functions
********/

/*****
* Name: 		_XmHTMLNewAnchor
* Return Type:	XmHTMLAnchor *
* Description: 	allocates and fills an anchor object
* In:
*	html:		owning widget
*	object:		raw anchor data
* Returns:
*	the allocated object
* Note:
*	this routine assumes that the <a> tag has attributes.
*****/
XmHTMLAnchor*
_XmHTMLNewAnchor(XmHTMLWidget html, XmHTMLObject *object)
{
	static XmHTMLAnchor *anchor;

	anchor = (XmHTMLAnchor*)malloc(sizeof(XmHTMLAnchor));

	/* set all fields to zero */
	(void)memset(anchor, 0, sizeof(XmHTMLAnchor));

	/* anchors can be both named and href'd at the same time */
	anchor->name = _XmHTMLTagGetValue(object->attributes, "name");

	/* get the url specs */
	parseHref(object->attributes, anchor);

	/* get the url type */
	anchor->url_type = XmHTMLGetURLType(anchor->href);

	/* promote to named if necessary */
	if(anchor->url_type == ANCHOR_UNKNOWN && anchor->name)
		anchor->url_type = ANCHOR_NAMED;

	/* see if we need to watch any events for this anchor */
	if(object->attributes &&
		(HTML_ATTR(event_proc) || HTML_ATTR(event_callback)))
	{
		/* pick up possible HTML4.0 events */
		anchor->events = _XmHTMLCheckCoreEvents(html, object->attributes,
			&anchor->event_mask);
	}

#ifdef PEDANTIC
	if(anchor->url_type == ANCHOR_UNKNOWN)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLNewAnchor"),
			XMHTML_MSG_43, object->attributes, object->line);
	}
#endif /* PEDANTIC */

	/*
	* If we have a proc available for anchor testing, call it and
	* set the visited field.
	*/
	if(HTML_ATTR(anchor_visited_proc))
		anchor->visited = HTML_ATTR(anchor_visited_proc)((Widget)html,
				anchor->href, HTML_ATTR(client_data));

	/* insert in the anchor list */
	if(list_data.anchor_head)
	{
		/*****
		* We can't do anything about duplicate anchors. Removing them
		* would mess up the named anchor lookup table.
		*****/
		list_data.anchor_current->next = anchor;
		list_data.anchor_current = anchor;
	}
	else
	{
		list_data.anchor_head = list_data.anchor_current = anchor;
	}
	return(anchor);
}

/*****
* Name:			_XmHTMLformatObjects
* Return Type:	XmHTMLObjectTable*
* Description:	creates a list of formatted HTML objects.
* In:
*	old:		previous XmHTMLWidget id (contains all items to be freed);
*	html:		current XmHTMLWidget id, containing raw (parser) html object
*				data and will receive new formatted data.
* Returns:
*	nothing, but list of formatted objects, anchors, tables (and images)
*	is updated upon return.
*****/
void
_XmHTMLformatObjects(XmHTMLWidget old, XmHTMLWidget html)
{
	/* text level variables */
	String text;
	int linefeed, n_words, anchor_words, named_anchors;
	int x_offset = 0, y_offset = 0;
	Byte text_data, line_data;		/* text and line data bits */
	unsigned long element_data = 0;
	XmHTMLWord *words;
	XmHTMLAnchor *anchor_data, *form_anchor_data;
	Boolean i18n = False;			/* multibyte input text flag */

#ifdef DEBUG
	int num_ignore = 0;
	static String func = "_XmHTMLformatObjects";
#endif

	/* list variables */
	int ul_level, ol_level, ident_level, current_list;
	listStack list_stack[XmHTML_NESTED_LISTS_MAX];
	int in_dt = 0;

	/* remaining object variables */
	Pixel fg, bg;
	Dimension width, height;
	Alignment halign, valign;
	ObjectType object_type;
	XmHTMLfont *font;
#ifdef I18N
	XmHTMLfont *i18nfont;
#endif	/* I18N */

	static XmHTMLObjectTableElement element;
	int basefont;

	/* imagemap and area stuff */
	XmHTMLImageMap *imageMap = NULL;

	/* local flags */
	Boolean ignore = False, in_pre = False;

	/* HTML tables */
	XmHTMLTable *table = NULL;
	XmHTMLTable *current_table = NULL;

	/* misc. variables */
	XmHTMLObject *temp;
	int i, new_anchors = 0;
	Boolean anchor_data_used = False;
	ToolkitAbstraction *tka = HTML_ATTR(tka);
	int fnheight;		/* current font height, for linefeeding */

	/* stacks */
	Stack align_stack, fg_color_stack, bg_color_stack, font_stack;

#ifdef DEBUG
	allocated = 0;
#endif

	/* Free any previous lists and initialize it */
	InitObjectTable(ATTR_HTML(old, formatted), ATTR_HTML(old, anchor_data));
	HTML_ATTR(formatted)   = (XmHTMLObjectTable*)NULL;
	HTML_ATTR(anchor_data) = (XmHTMLAnchor*)NULL;

	/* free table data */
	freeTables(ATTR_HTML(old, tables));
	HTML_ATTR(tables) = (XmHTMLTable*)NULL;

	/*
	* Nothing to do, just return. Should only happen if we get called
	* from Destroy().
	*/
	if(HTML_ATTR(elements) == NULL)
	{
		/* free top of list */
		if(list_data.head)
			free(list_data.head);
		list_data.head = NULL;
		return;
	}

	/* Move to the body element */
	for(temp = HTML_ATTR(elements); temp != NULL && temp->id != HT_BODY;
		temp = temp->next);

	/*
	* No <body> element found. This is an error since the parser will
	* *always* add a <body> element if none is present in the source
	* document.
	*/
	if(temp == NULL)
	{
		/*****
		* The only exception is a document only containing plain text and no
		* BODY tag was present in the input. In this case, check the input and
		* start outputting at the end of the first text element not belonging
		* to the head of a document.
		* Fix 01/04/98-01, kdh
		*****/
		XmHTMLObject *last_obj = NULL;

		for(temp = HTML_ATTR(elements); temp != NULL; temp = temp->next)
		{
			switch(temp->id)
			{
				case HT_DOCTYPE:
				case HT_BASE:
					/* these two have no ending tag */
					last_obj = temp;
					break;
				case HT_HTML:
					/* don't use the closing tag for this, it's the end... */
					if(!temp->is_end)
						last_obj = temp;
					break;
				case HT_HEAD:
				case HT_TITLE:
				case HT_SCRIPT:
				case HT_STYLE:
				case HT_PAGE:
					/* pick up the last closing tag */
					if(temp->is_end)
						last_obj = temp;
				default:
					break;
			}
		}

		if(last_obj == NULL || last_obj->next == NULL)
		{
			_XmHTMLWarning(__WFUNC__(html, func), XMHTML_MSG_44);
			return;
		}
		/* we move to the correct object a bit further down */
		temp = last_obj;
	}

	/* initialize font stack */
	font       = _XmHTMLSelectFontCache(html, False);
	basefont   = 4;
	font_stack = StackCreateDouble((void*)font, (void*)basefont, NULL, NULL);

	/* Reset anchor count */
	anchor_words = 0;
	named_anchors = 0;
	anchor_data = form_anchor_data = NULL;

	/* initialize list variables */
	ul_level = 0;
	ol_level = 0;
	ident_level = 0;
	current_list = 0;

	/* reset stacks */
	for(i = 0; i < XmHTML_NESTED_LISTS_MAX; i++)
	{
		list_stack[i].isindex = False;
		list_stack[i].marker  = XmMARKER_NONE;
		list_stack[i].level   = 0;
		list_stack[i].type    = HT_ZTEXT;
	}

	/* Initialize linefeeding mechanism */
	text_data = TEXT_SPACE_NONE;
	linefeed = CheckLineFeed(CLEAR_SOFT, True, &text_data);

	/* Initialize alignment */
	halign = HTML_ATTR(default_halign);
	align_stack = StackCreate((void*)halign, NULL);
	valign = XmVALIGN_NONE;
	object_type = OBJ_NONE;

	/* check for background stuff */
	ParseBodyTags(html, temp);

	/* foreground color to use */
	fg = HTML_ATTR(body_fg);
	/* background color to use */
	bg = HTML_ATTR(body_bg);

	/* Initialize color stacks */
	fg_color_stack = StackCreate((void*)fg, NULL);
	bg_color_stack = StackCreate((void*)bg, NULL);

	/* move to the next element */
	temp = temp->next;

	/*
	* Insert a dummy element at the head of the list to prevent incorrect
	* handling of the first real element to be rendered.
	* fix 12/14/97-01, kdh
	*/
	NewTableElement(element,temp);
	element->object_type = OBJ_NONE;
	element->font = HTML_ATTR(default_font);
	InsertTableElement(element, False);
	text_data = TEXT_SPACE_NONE;

	/*
	* Only elements between <BODY></BODY> elements are really interesting.
	* BUT: if the HTML verification/reparation routines in parse.c should
	* fail, we might have a premature </body> element, so we don't check on
	* it but walk thru every item found.
	*/
	while(temp != HTML_ATTR(last_element))
	{
		/* create new element */
		if(!ignore)
			NewTableElement(element,temp);
		else
		{
			/*****
			* reuse current element if it wasn't needed on the previous
			* pass.
			* fix 11/12/97-01, kdh
			*****/
			(void)memset(element, 0, sizeof(XmHTMLObjectTable));
			/* fill in appropriate fields */
			element->object = temp;
		}

		element->prev = list_data.current;

		/* Initialize all fields changed in here */
		text = NULL;
		ignore = False;
		object_type = OBJ_NONE;
		n_words = 0;
		width = height = 0;
		words = NULL;
		linefeed = CLEAR_NONE;
		line_data = NO_LINE;
		fnheight = font->lineheight;

		_XmHTMLDebug(2, ("format.c, _XmHTMLformatObjects, object data:\n"
			"\telement id: %s\n\tattributes: %s\n\tis_end: %s\n",
			html_tokens[temp->id],
			temp->attributes ? temp->attributes : "<none>",
			temp->is_end ? "Yes" : "No"));

		switch(temp->id)
		{
			/* plain text */
			case HT_ZTEXT:
				object_type = OBJ_TEXT;
				/*****
				* CopyText
				* We do not want escape expansion if we are loading a plain
				* text document.
				*****/
				if((text = CopyText(html, temp->element, in_pre, &text_data,
					HTML_ATTR(mime_id) != XmNONE, &i18n)) == NULL)
				{
					/*****
					* named anchors can be empty, so keep them by inserting
					* a dummy word, but only do that once (prevents a margin
					* reset as well).
					*****/
					if((element_data & ELE_ANCHOR_INTERN) && !anchor_data_used)
					{
						words = MakeDummyWord(&height, font, line_data,
									element);
						n_words = 1;
					}
					else
						ignore = True; /* ignore empty text fields */
					break;
				}
				if(!in_pre)
				{
					CollapseWhiteSpace(text);
					/*****
					* If this turns out to be an empty block, ignore it,
					* but only if it's not an empty named anchor.
					*****/
					if(strlen(text) == 0)
					{
						if((element_data & ELE_ANCHOR_INTERN) &&
							!anchor_data_used)
						{
							object_type = OBJ_NONE;
							words = MakeDummyWord(&height, font, line_data,
										element);
							n_words = 1;
						}
						else
							ignore = True;
						free(text);
						break;
					}
					/* check line data */
					CHECK_LINE;

					/* convert text to a number of words */
					words = TextToWords(text, &n_words, &height, font,
						line_data, text_data, element, tka);

					/* Plain text does a hard reset */
					linefeed = CheckLineFeed(CLEAR_NONE, True, &text_data);
				}
				else
				{
					object_type = OBJ_PRE_TEXT;
					/* check line data */
					CHECK_LINE;
					/* convert text to a number of words, keep formatting. */
					words = TextToPre(text, &n_words, font, line_data, element,
								tka, HTML_ATTR(tabwidth));

					/* Preformatted text does a soft reset */
					linefeed = CheckLineFeed(CLEAR_NONE, False, &text_data);
				}
				break;

			/* images */
			case HT_IMG:
				/* release break bit */
				text_data &= ~TEXT_BREAK;
				text_data |= TEXT_IMAGE;
				if((words = ImageToWord(html, temp->attributes, &n_words,
					&height, element, in_pre, tka,
					(Boolean)(anchor_data != NULL), text_data)) == NULL)
				{
					/* loose image bit */
					text_data &= ~TEXT_IMAGE;
					ignore = True;
					break;
				}
				object_type = in_pre ? OBJ_PRE_TEXT : OBJ_TEXT;

				/* No explicit returns for images, reset */
				linefeed = CheckLineFeed(CLEAR_NONE, False, &text_data);

				break;

			/* anchors */
			case HT_A:
				if(temp->is_end)
				{
					/*
					* this is a very sneaky hack: since empty named anchors
					* are allowed, we must store it somehow. And this is how
					* we do it: insert a dummy word (to prevent margin reset)
					* and back up one element.
					*/
					if(!anchor_data_used && (element_data & ELE_ANCHOR_INTERN))
					{
						_XmHTMLDebug(2, ("format.c: _XmHTMLformatObjects, "
							"adding bogus named anchor %s\n",
							anchor_data->name));

						/* insert a dummy word to prevent margin reset */
						words = MakeDummyWord(&height, font, line_data,
									element);
						n_words = 1;
						object_type = OBJ_TEXT;
						anchor_data_used = True;
						temp = temp->prev;
						break;
					}
					/* unset anchor bitfields */
					element_data &= ( ~ELE_ANCHOR & ~ELE_ANCHOR_TARGET &
							~ELE_ANCHOR_VISITED & ~ELE_ANCHOR_INTERN);
					fg = (Pixel)StackPop(fg_color_stack);

					anchor_data = NULL;

					ignore = True;	/* only need anchor data */
					_XmHTMLDebug(2,("format.c: _XmHTMLformatObjects: anchor "
						"end\n"));
				}
				else
				{
					/* allocate a new anchor */
					anchor_data = (temp->attributes ?
							_XmHTMLNewAnchor(html, temp) : NULL);

					/* sanity check */
					if(!anchor_data)
					{
						ignore = True;
						break;
					}
					/* save current color */
					StackPush(fg_color_stack, fg);

					new_anchors++;
					anchor_data_used = False;

					/* set proper element bits */

					/* maybe it's a named one */
					if(anchor_data->name)
						element_data |= ELE_ANCHOR_INTERN;

					/*
					* maybe it's also a plain anchor. If so, see what
					* foreground color we have to use to render this
					* anchor.
					*/
					if(anchor_data->href[0] != '\0')
					{
						element_data |= ELE_ANCHOR;
						fg = HTML_ATTR(anchor_fg);

						/* maybe it's been visited */
						if(anchor_data->visited)
						{
							element_data |= ELE_ANCHOR_VISITED;
							fg = HTML_ATTR(anchor_visited_fg);
						}
						/* maybe it's a target */
						else if(anchor_data->target)
						{
							element_data |= ELE_ANCHOR_TARGET;
							fg = HTML_ATTR(anchor_target_fg);
						}
					}
					_XmHTMLDebug(2,("format.c: _XmHTMLformatObjects: anchor "
						"start\n"));
					ignore = True;	/* only need anchor data */
				}
				break;

			/* font changes */
			case HT_CITE:		/* italic */
			case HT_I:			/* italic */
			case HT_EM:			/* italic */
			case HT_DFN:		/* italic */
			case HT_STRONG:		/* bold */
			case HT_B:			/* bold */
			case HT_SAMP:		/* fixed width */
			case HT_TT:			/* fixed width */
			case HT_VAR:		/* fixed width */
			case HT_CODE:		/* fixed width */
			case HT_KBD:		/* fixed width */
				if(temp->is_end)
				{
					if(HTML_ATTR(allow_color_switching))
						fg = (Pixel)StackPop(fg_color_stack);
					font = (XmHTMLfont*)StackPopDouble(font_stack, basefont);
				}
				else
				{
					if(HTML_ATTR(allow_color_switching))
						PUSH_COLOR(html);
					StackPushDouble(font_stack, font, basefont);
					font = _XmHTMLLoadFont(html, temp->id, basefont, font);

					/* new height for linefeeds */
					fnheight = font->lineheight;
				}
				ignore = True; /* only need font data */
				break;

			case HT_U:
				if(temp->is_end) /* unset underline bitfields */
					element_data &= (~ELE_UNDERLINE & ~ELE_UNDERLINE_TEXT);
				else
					element_data |= ELE_UNDERLINE;
				ignore = True; /* only need underline data */
				break;

			case HT_STRIKE:
				if(temp->is_end) /* unset strikeout bitfields */
					element_data &= (~ELE_STRIKEOUT & ~ELE_STRIKEOUT_TEXT);
				else
					element_data |= ELE_STRIKEOUT;
				ignore = True; /* only need strikeout data */
				break;

			case HT_BASEFONT:
				{
					basefont = temp->attributes ?
						_XmHTMLTagGetNumber(temp->attributes, "size", 0) : 0;
					/* take absolute value */
					basefont = Abs(basefont);
					if(basefont < 1 || basefont > 7)
					{
						if(HTML_ATTR(bad_html_warnings))
							_XmHTMLWarning(__WFUNC__(html, func),
								XMHTML_MSG_45, basefont, temp->line);
						basefont = 4;
					}
				}
				ignore = True;	/* only need font data */
				break;

			/*****
			* <font> is a big performance hit. We always need to push & pop
			* the font *even* if only the font color has been changed as we
			* can't keep track of what has actually been changed.
			*****/
			case HT_FONT:
				if(temp->is_end)
				{
					if(HTML_ATTR(allow_font_switching))
						font =(XmHTMLfont*)StackPopDouble(font_stack, basefont);
					if(HTML_ATTR(allow_color_switching))
						fg = (Pixel)StackPop(fg_color_stack);
				}
				else
				{
					char *chPtr;
					int size = xmhtml_basefont_sizes[basefont - 1];

					if(HTML_ATTR(allow_color_switching))
						PUSH_COLOR(html);

					if(HTML_ATTR(allow_font_switching))
						StackPushDouble(font_stack, font, basefont);
					else
						break;

					/* can't use TagGetNumber: fontchange can be relative */
					chPtr = temp->attributes ?
						_XmHTMLTagGetValue(temp->attributes, "size") : NULL;
					if(chPtr != NULL)
					{
						int f_inc = atoi(chPtr);

						/* check wether size is relative or not */
						if(chPtr[0] == '-' || chPtr[0] == '+')
						{
							f_inc = basefont + f_inc; /* + 1; */
						}
						/* sanity check */
						if(f_inc < 1 || f_inc > 7)
						{
							if(f_inc < 1)
								f_inc = 1;
							else
								f_inc = 7;
						}

						basefont = f_inc;
						/* minus one: zero based array */
						size = xmhtml_basefont_sizes[f_inc-1];
						free(chPtr); /* fix 01/28/98-02, kdh */
						chPtr = NULL;
					}
					/*****
					* Font face changes only allowed when not in preformatted
					* text.
					* Only check when not being pedantic.
					*****/
#ifndef PEDANTIC
					if(!in_pre)
#endif
						chPtr = temp->attributes ?
							_XmHTMLTagGetValue(temp->attributes, "face") : NULL;

					if(chPtr != NULL)
					{
#ifdef PEDANTIC
						if(in_pre)
						{
							_XmHTMLWarning(__WFUNC__(html, func),
								XMHTML_MSG_41, "FONT FACE=xxx",
								html_tokens[HT_PRE], temp->line);
							/*****
							* Ignore face but must allow for size change.
							* (Font stack will get unbalanced otherwise!)
							*****/
							font = _XmHTMLLoadFont(html, HT_FONT, size, font);
						}
						else
#endif
							font = _XmHTMLLoadFontWithFace(html, size, chPtr,
										font);

						free(chPtr);
					}
					else
						font = _XmHTMLLoadFont(html, HT_FONT, size, font);
				}
				ignore = True; /* only need font data */
				break;

			case HT_BIG:
				if(temp->is_end)
					font = (XmHTMLfont*)StackPopDouble(font_stack, basefont);
				else /* multiple big elements are not honoured */
				{
					StackPushDouble(font_stack, font, basefont);
					font = _XmHTMLLoadFont(html, HT_FONT,
								xmhtml_basefont_sizes[4], font);
				}
				ignore = True; /* only need font data */
				break;

			case HT_SMALL:
				if(temp->is_end)
					font = (XmHTMLfont*)StackPopDouble(font_stack, basefont);
				else /* multiple small elements are not honoured */
				{
					StackPushDouble(font_stack, font, basefont);
					font = _XmHTMLLoadFont(html, HT_FONT,
								xmhtml_basefont_sizes[2], font);
				}
				ignore = True; /* only need font data */
				break;

			case HT_SUB:
			case HT_SUP:
				if(temp->is_end)
				{
					/* restore vertical offset */
					y_offset = 0;
					x_offset = 0;
					font = (XmHTMLfont*)StackPopDouble(font_stack, basefont);
				}
				else /* multiple small elements are not honoured */
				{
					StackPushDouble(font_stack, font, basefont);
					font = _XmHTMLLoadFont(html, HT_FONT,
								xmhtml_basefont_sizes[2], font);
					y_offset = (temp->id == HT_SUB ?
						font->sub_yoffset : font->sup_yoffset);
					x_offset = (temp->id == HT_SUB ?
						font->sub_xoffset : font->sup_xoffset);
				}
				ignore = True; /* only need font data */
				break;

			case HT_H1:
			case HT_H2:
			case HT_H3:
			case HT_H4:
			case HT_H5:
			case HT_H6:
				if(temp->is_end)
				{
					if(HTML_ATTR(allow_color_switching))
						fg = (Pixel)StackPop(fg_color_stack);
					halign = (Alignment)StackPop(align_stack);
					font = (XmHTMLfont*)StackPopDouble(font_stack, basefont);
				}
				else
				{
					/*****
					* <H> elements are sometimes used as spacers. This gives
					* *horrible* rendering. Look forward to the first text
					* element and check if it contains text. If it does, allow
					* it, else ignore this element.
					*****/
					XmHTMLObject *elePtr = temp->next;
					for(; elePtr != NULL && elePtr->id != HT_ZTEXT &&
						elePtr->id != temp->id; elePtr = elePtr->next);

					/* this <h> only contains text, check it */
					if(elePtr && elePtr->id == HT_ZTEXT &&
						elePtr->next->id == temp->id)
					{
						String chPtr = elePtr->element;
						/*
						* found a text element. Check if it has valid
						* content.
						*/
						while(*chPtr != '\0' && isspace(*chPtr))
							chPtr++;
						/* empty head. Ignore it */
						if(*chPtr == '\0')
						{
							_XmHTMLDebug(2, ("format.c: _XmHTMLformatObjects; "
								"empty element %s found at line %i in input, "
								"ignoring.\n",
								html_tokens[temp->id], temp->line));

							/* set to closing <h> and ignore it */
							temp = elePtr->next;
							object_type = OBJ_NONE;
							ignore = True;
							break;
						}
					}
					if(HTML_ATTR(allow_color_switching))
						PUSH_COLOR(html);

					StackPush(align_stack, halign);
					halign = (temp->attributes ?
						_XmHTMLGetHorizontalAlignment(temp->attributes, halign)
						: halign);

					StackPushDouble(font_stack, font, basefont);
					font = _XmHTMLLoadFont(html, temp->id, basefont, font);

					/* new height for linefeeds */
					fnheight = font->lineheight;

					/*****
					* Need to update basefont size as well so font face changes
					* changes *inside* these elements use the correct font
					* size as well.
					* The sizes used by the headers are in reverse order.
					*****/
					basefont = (int)(HT_H6 - temp->id) + 1;
				}
				linefeed = CheckLineFeed(CLEAR_HARD, False, &text_data);
				object_type = OBJ_BLOCK;
				break;

			/* lists. The COMPACT tag is ignored */
			case HT_UL:
			case HT_DIR:
			case HT_MENU:
				if(temp->is_end)
				{

					ul_level--;
					ident_level--;
					if(ident_level < 0)
					{
						if(HTML_ATTR(bad_html_warnings))
							_XmHTMLWarning(__WFUNC__(html, func),
								XMHTML_MSG_46, temp->line);
						ident_level = 0;
					}
					current_list = (ident_level ? ident_level - 1 : 0);
				}
				else
				{
					int mark_id;
					/* avoid overflow of mark id array */
					mark_id = ul_level % UL_ARRAYSIZE;

					/* set default marker & list start */
					list_stack[ident_level].marker = ul_markers[mark_id].type;
					list_stack[ident_level].level = 0;
					list_stack[ident_level].type = temp->id;

					if(ident_level == XmHTML_NESTED_LISTS_MAX)
					{
  						_XmHTMLWarning(__WFUNC__(html, func), XMHTML_MSG_47,
							XmHTML_NESTED_LISTS_MAX, temp->line);
						ident_level = XmHTML_NESTED_LISTS_MAX-1;
					}
					current_list = ident_level;

					if(temp->id == HT_UL)
					{
						char *chPtr;
						/* check if user specified a custom marker */
						chPtr = temp->attributes ?
							_XmHTMLTagGetValue(temp->attributes, "type") : NULL;
						if(chPtr != NULL)
						{
							/*
							* Walk thru the list of possible markers. If a
							* match is found, store it so we can switch back
							* to the correct marker once this list terminates.
							*/
							for(i = 0 ; i < UL_ARRAYSIZE; i++)
							{
								if(!(strcasecmp(ul_markers[i].name, chPtr)))
								{
									list_stack[ident_level].marker =
										ul_markers[i].type;
									break;
								}
							}
							free(chPtr);
						}
					}
					ul_level++;
					ident_level++;
				}
				linefeed = CheckLineFeed(CLEAR_SOFT, False, &text_data);
				object_type = OBJ_BLOCK;
				break;

			case HT_OL:
				if(temp->is_end)
				{
					/* must be reset properly, only possible for <ol> lists. */
					list_stack[current_list].isindex = False;

					ol_level--;
					ident_level--;
					if(ident_level < 0)
					{
						if(HTML_ATTR(bad_html_warnings))
							_XmHTMLWarning(__WFUNC__(html, func),
								XMHTML_MSG_46, temp->line);
						ident_level = 0;
					}
					current_list = (ident_level ? ident_level - 1 : 0);
				}
				else
				{
					int mark_id;
					char *chPtr;

					/* avoid overflow of mark id array */
					mark_id = ol_level % OL_ARRAYSIZE;

					/* set default marker & list start */
					list_stack[ident_level].marker = ol_markers[mark_id].type;
					list_stack[ident_level].level = 0;
					list_stack[ident_level].type = temp->id;

					if(ident_level == XmHTML_NESTED_LISTS_MAX)
					{
  						_XmHTMLWarning(__WFUNC__(html, func), XMHTML_MSG_47,
							XmHTML_NESTED_LISTS_MAX, temp->line);
						ident_level = XmHTML_NESTED_LISTS_MAX-1;
					}
					current_list = ident_level;

					/* check if user specified a custom marker */
					chPtr = temp->attributes ?
							_XmHTMLTagGetValue(temp->attributes, "type") : NULL;
					if(chPtr != NULL)
					{
						/*
						* Walk thru the list of possible markers. If a
						* match is found, store it so we can switch back
						* to the correct marker once this list terminates.
						*/
						for(i = 0 ; i < OL_ARRAYSIZE; i++)
						{
							if(!(strcmp(ol_markers[i].name, chPtr)))
							{
								list_stack[ident_level].marker =
									ol_markers[i].type;
								break;
							}
						}
						free(chPtr);
					}

					/* see if a start tag exists */
					if(temp->attributes &&
						_XmHTMLTagCheck(temp->attributes, "start"))
					{
						/* pick up a start spec */
						list_stack[ident_level].level =
							_XmHTMLTagGetNumber(temp->attributes, "start", 0);
						list_stack[ident_level].level--;
					}

					/* see if we have to propage the current index number */
					list_stack[ident_level].isindex = temp->attributes ?
						_XmHTMLTagCheck(temp->attributes, "isindex") : False;
					ol_level++;
					ident_level++;
				}
				linefeed = CheckLineFeed(CLEAR_SOFT, False, &text_data);
				object_type = OBJ_BLOCK;
				break;

			case HT_LI:
				if(temp->is_end)	/* optional termination */
					object_type = OBJ_BLOCK;
				else
				{
					char *chPtr;

					/* increase list counter */
					list_stack[current_list].level++;

					/* check if user specified a custom marker */
					chPtr = temp->attributes ?
						_XmHTMLTagGetValue(temp->attributes, "type") : NULL;
					if(chPtr != NULL)
					{
						/*
						* depending on current list type, check and set
						* the marker.
						*/
						if(list_stack[current_list].type == HT_OL)
						{
							for(i = 0 ; i < OL_ARRAYSIZE; i++)
							{
								if(!(strcmp(ol_markers[i].name, chPtr)))
								{
									list_stack[current_list].marker =
										ol_markers[i].type;
									break;
								}
							}
						}
						else if(list_stack[current_list].type == HT_UL)
						{
							for(i = 0 ; i < UL_ARRAYSIZE; i++)
							{
								if(!(strcmp(ul_markers[i].name, chPtr)))
								{
									list_stack[current_list].marker =
										ul_markers[i].type;
									break;
								}
							}
						}
						else	/* dir, menu, dt elements */
							list_stack[current_list].marker = XmMARKER_NONE;
						free(chPtr);
					}
					/* check if user specified a custom number for ol lists */
					if(list_stack[current_list].type == HT_OL &&
						temp->attributes &&
						_XmHTMLTagCheck(temp->attributes, "value"))
					{
						list_stack[current_list].level =
							_XmHTMLTagGetNumber(temp->attributes, "value", 0);
					}
					/*
					* If the current list is an index, create a prefix for
					* the current item
					*/
					if(list_stack[current_list].isindex)
					{
						words = indexToWord(html, list_stack, current_list,
									element, in_pre);
						n_words = 1;
					}
					object_type = OBJ_BULLET;
				}
				linefeed = CheckLineFeed(CLEAR_SOFT, False, &text_data);
				break;

			case HT_DL:
				if(temp->is_end)
					ident_level--;
				else
					ident_level++;
				linefeed = CheckLineFeed(CLEAR_SOFT, False, &text_data);
				object_type = OBJ_BLOCK;
				break;

			case HT_DT:
				if(temp->is_end)
					in_dt--;
				else
					in_dt++;
			case HT_DD:
				object_type = OBJ_BLOCK;
				linefeed = CheckLineFeed(CLEAR_SOFT, False, &text_data);
				break;

			/* block commands */
			case HT_ADDRESS:
				if(temp->is_end)
				{
					if(HTML_ATTR(allow_color_switching))
						fg = (Pixel)StackPop(fg_color_stack);
					font = (XmHTMLfont*)StackPopDouble(font_stack, basefont);
				}
				else
				{
					if(HTML_ATTR(allow_color_switching))
						PUSH_COLOR(html);
					StackPushDouble(font_stack, font, basefont);
					font = _XmHTMLLoadFont(html, temp->id, basefont, font);

					/* new height for linefeeds */
					fnheight = font->lineheight;
				}
				linefeed = CheckLineFeed(CLEAR_SOFT, False, &text_data);
				object_type = OBJ_BLOCK;
				break;

			/* <BR> is a special word */
			case HT_BR:
#if 0
				{
					String chPtr;

					if((chPtr = _XmHTMLTagGetValue(temp->attributes,
						"clear")) != NULL && locase(chPtr[0]) != 'n')
					{
						/* all clear attribs but none reset the linefeeder */
						(void)CheckLineFeed(CLEAR_HARD, True);
						linefeed = CLEAR_ALL;

						if(locase(chPtr[0]) == 'l')	/* clear = left */
							halign = XmHALIGN_LEFT;
						else if(locase(chPtr[0]) == 'r')	/* clear = right */
							halign = XmHALIGN_RIGHT;
						/* no default */

						free(chPtr);
					}
					else /* fix 01/20/97-02, kdh */
						linefeed = CheckLineFeed(CLEAR_SOFT, False);
				}
#endif
				if((linefeed = CheckLineFeed(CLEAR_SOFT, False,
					&text_data)) != CLEAR_NONE)
				{
					words = BreakToWord(&height, font, linefeed, element);
					n_words = 1;
					object_type = OBJ_TEXT;
					text_data &= ~TEXT_SPACE_LEAD & ~TEXT_SPACE_TRAIL;
					text_data |= TEXT_BREAK;	/* it's a linebreak */
				}
				else
				{
					/* linebreak follows a stronger linebreak, ignore */
					object_type = OBJ_NONE;
					ignore = True;
				}
				break;

			case HT_TAB:
				{
					char *chPtr;

					object_type = OBJ_TEXT;

					element->len = 8; /* default tabsize */

					/* see if we have a width spec */
					chPtr = temp->attributes ?
						_XmHTMLTagGetValue(temp->attributes, "size") : NULL;
					if(chPtr != NULL)
					{
						element->len = atoi(chPtr);
						free(chPtr);
					}
					n_words = 1;
					words = SetTab(element->len, &height, font, element, tka);
				}
				break;

			case HT_PRE:
				if(temp->is_end)
				{
					if(HTML_ATTR(allow_color_switching))
						fg = (Pixel)StackPop(fg_color_stack);
					font = (XmHTMLfont*)StackPopDouble(font_stack, basefont);
					in_pre = False;
				}
				else
				{
					if(HTML_ATTR(allow_color_switching))
						PUSH_COLOR(html);
					StackPushDouble(font_stack, font, basefont);
					font = _XmHTMLLoadFont(html, temp->id, basefont, font);
					in_pre = True;

					/* new height for linefeeds */
					fnheight = font->lineheight;
				}
				linefeed = CheckLineFeed(CLEAR_SOFT, False, &text_data);
				object_type = OBJ_BLOCK;
				break;

			case HT_BLOCKQUOTE:
				if(temp->is_end)
				{
					ident_level--;
					if(HTML_ATTR(allow_color_switching))
						fg = (Pixel)StackPop(fg_color_stack);
				}
				else
				{
					ident_level++;
					if(HTML_ATTR(allow_color_switching))
						PUSH_COLOR(html);
				}
				linefeed = CheckLineFeed(CLEAR_SOFT, False, &text_data);
				object_type = OBJ_BLOCK;
				break;

			/*****
			* <P> and <DIV> are equal all except for the amount of
			* vertical whitespace added: <P> causes a hard linebreak whereas
			* <DIV> causes a soft linebreak.
			*****/
			case HT_P:
			case HT_DIV:
				if(temp->is_end)
				{
					halign = (Alignment)StackPop(align_stack);
					/*
					* Paragraph ending also adds linespacing (natural flow
					* of text between paragraphs).
					*/
					linefeed = CheckLineFeed(
						(temp->id == HT_P ? CLEAR_HARD : CLEAR_SOFT), False,
						&text_data);

					/* do we have a color attrib? */
					if(HTML_ATTR(allow_color_switching))
						fg = (Pixel)StackPop(fg_color_stack);
				}
				else
				{
					StackPush(align_stack, halign);
					halign = temp->attributes ?
						_XmHTMLGetHorizontalAlignment(temp->attributes, halign)
						: halign;
					linefeed = CheckLineFeed(
						(temp->id == HT_P ? CLEAR_HARD : CLEAR_SOFT), False,
						&text_data);
					/* do we have a color attrib? */
					if(HTML_ATTR(allow_color_switching))
						PUSH_COLOR(html);
				}
				object_type = OBJ_BLOCK;
				break;

			case HT_CENTER:
				if(temp->is_end)
				{
					halign = (Alignment)StackPop(align_stack);
					/* do we have a color attrib? */
					if(HTML_ATTR(allow_color_switching))
						fg = (Pixel)StackPop(fg_color_stack);
				}
				else
				{
					StackPush(align_stack, halign);
					halign = XmHALIGN_CENTER;
					/* do we have a color attrib? */
					if(HTML_ATTR(allow_color_switching))
						PUSH_COLOR(html);
				}
				linefeed = CheckLineFeed(CLEAR_SOFT, False, &text_data);
				object_type = OBJ_BLOCK;
				break;

			case HT_HR:
				{
					/*
					* horizontal rules don't have an ending counterpart,
					* so the alignment is never pushed. If we should do that,
					* we would get an unbalanced stack.
					*/
					if(temp->attributes)
					{
						element->halign =
							_XmHTMLGetHorizontalAlignment(temp->attributes,
								halign);
						/* see if we have a width spec */
						element->len = _XmHTMLTagCheckNumber(temp->attributes,
										"width", 0);

						/* check height */
						height = _XmHTMLTagGetNumber(temp->attributes, "size",
								0);
						/* sanity check */
						if(height <= 0 )
							height = 2;
						/* y_offset is used as a flag for the NOSHADE attr. */
						element->y_offset =
							(int)_XmHTMLTagCheck(temp->attributes, "noshade");
					}
					else
					{
						element->halign   = halign;
						element->len      = 0;
						element->y_offset = 0;
						height            = 0;
					}

					/* do we have a color attrib? */
					if(HTML_ATTR(allow_color_switching) &&
						!HTML_ATTR(strict_checking))
						PUSH_COLOR(html);
				}
				/* horizontal rules always have a soft return */
				linefeed = CheckLineFeed(CLEAR_SOFT, False, &text_data);
				object_type = OBJ_HRULE;
				break;

			/* forms */
			case HT_FORM:
				if(temp->is_end)
					_XmHTMLEndForm(html);
				else
					_XmHTMLStartForm(html, temp->attributes);

				/* only need form data */
				ignore = True;
				break;

			case HT_SELECT:
				/* this form component can only contain option tags */
				if((words = SelectToWord(html, temp, &n_words, &width, &height,
					element, in_pre)) == NULL)
				{
					ignore = True;
					break;
				}
				/* walk to the end of this select */
				temp = temp->next;
				for(; temp != NULL && temp->id != HT_SELECT;
					temp = temp->next);

				text_data |= TEXT_FORM;
				object_type = in_pre ? OBJ_PRE_TEXT : OBJ_TEXT;
				break;

			/*****
			* It's an error if we get this, SelectToWord deals with these
			* tags.
			*****/
			case HT_OPTION:
				if(!temp->is_end)
				{
					if(HTML_ATTR(bad_html_warnings))
						_XmHTMLWarning(__WFUNC__(html, func), XMHTML_MSG_48,
							html_tokens[HT_OPTION], html_tokens[HT_SELECT],
							temp->line);
				}
				ignore = True;
				break;

			case HT_TEXTAREA:
				if((words = TextAreaToWord(html, temp, &n_words, &width,
					&height, element, in_pre)) == NULL)
				{
					ignore = True;
					break;
				}
				/*
				* Walk to the end of this textarea. If there was any text
				* provided, we've already picked it up.
				*/
				temp = temp->next;
				for(; temp != NULL && temp->id != HT_TEXTAREA;
					temp = temp->next);

				text_data |= TEXT_FORM;
				object_type = in_pre ? OBJ_PRE_TEXT : OBJ_TEXT;
				break;

			case HT_INPUT:
				if((words = InputToWord(html, temp->attributes, &n_words,
					&width, &height, element, in_pre)) == NULL)
				{
					ignore = True;
					break;
				}
				/* type=image is promoted to a true image */
				if(words->form->type == FORM_IMAGE)
				{
					text_data |= TEXT_IMAGE;

					/* allocate a new anchor */
					if((form_anchor_data = temp->attributes ?
						_XmHTMLNewAnchor(html, temp): NULL) == NULL)
						break;

					/* promote to internal form anchor */
					form_anchor_data->url_type = ANCHOR_FORM_IMAGE;

					new_anchors++;

					/* set proper element bits, we assume it's a plain one */
					element_data |= ELE_ANCHOR;
				}
				else
				{
					text_data |= TEXT_FORM;
				}
				object_type = in_pre ? OBJ_PRE_TEXT : OBJ_TEXT;
				break;

			/* applets */
			case HT_APPLET:
				if(temp->is_end)
				{
					/*
					* INSERT CODE
					* to end this applet
					*/
				}
				else
				{
					if(HTML_ATTR(bad_html_warnings))
						_XmHTMLWarning(__WFUNC__(html, func), XMHTML_MSG_49,
							html_tokens[HT_APPLET]);
					/*
					* INSERT CODE
					* to start this applet
					*/
				}
				object_type = OBJ_APPLET;
				ignore = True;
				break;

			case HT_PARAM:		/* applet params */
				if(HTML_ATTR(bad_html_warnings))
					_XmHTMLWarning(__WFUNC__(html, func), XMHTML_MSG_49,
						html_tokens[HT_PARAM]);
				object_type = OBJ_APPLET;
				ignore = True;
				break;

			case HT_MAP:
				if(temp->is_end)
				{
					_XmHTMLStoreImagemap(html, imageMap);
					imageMap = NULL;
				}
				else
				{
					String chPtr;

					chPtr = temp->attributes ?
						_XmHTMLTagGetValue(temp->attributes, "name") : NULL;
					if(chPtr != NULL)
					{
						imageMap = _XmHTMLCreateImagemap(chPtr);
						free(chPtr);
					}
					else if(HTML_ATTR(bad_html_warnings))
						_XmHTMLWarning(__WFUNC__(html, func), XMHTML_MSG_50,
							temp->line);
				}
				ignore = True;	/* only need imagemap name */
				break;

			case HT_AREA:
				if(imageMap)
					_XmHTMLAddAreaToMap(html, imageMap, temp);
				else if(HTML_ATTR(bad_html_warnings))
					_XmHTMLWarning(__WFUNC__(html, func), XMHTML_MSG_48,
						html_tokens[HT_AREA], html_tokens[HT_MAP], temp->line);
				ignore = True;	/* only need area data */
				break;

			/* tables */
			case HT_TABLE:
				if(temp->is_end)
				{
					/* wrapup current table */
					table = tableClose(html, table, element);

					halign = (Alignment)StackPop(align_stack);
					bg = (Pixel)StackPop(bg_color_stack);
					object_type = OBJ_NONE;
				}
				else
				{
					/*****
					* Tables start cause a hard break if it has no
					* parent, and a soft one if it has.
					*****/
					if(table)
						linefeed = CheckLineFeed(CLEAR_SOFT, False, &text_data);
					else
						linefeed = CheckLineFeed(CLEAR_HARD, True, &text_data);

					StackPush(align_stack, halign);
					StackPush(bg_color_stack, bg);

					/*****
					* Open a new table. Returns a parent or a child table.
					*****/
					table = tableOpen(html, table, element, temp, &halign, &bg);

					if(table == NULL)
						break;

					/* new master table, insert */
					if(table->parent == NULL)
					{
						/* insert this table in the list of tables */
						if(HTML_ATTR(tables))
						{
							current_table->next = table;
							current_table = table;
						}
						else
						{
							HTML_ATTR(tables) = table;
							current_table = table;
						}
					}
					object_type = OBJ_TABLE;
				}
				break;

			case HT_CAPTION:		/* table caption */
				if(temp->is_end)
				{
					/* close the caption */
					tableCloseCaption(html, table, element);

					halign = (Alignment)StackPop(align_stack);
					bg = (Pixel)StackPop(bg_color_stack);

					object_type = OBJ_NONE;
				}
				else
				{
					StackPush(align_stack, halign);
					StackPush(bg_color_stack, bg);

					/* captions are always centered */
					halign = XmHALIGN_CENTER;

					/* open the caption */
					tableOpenCaption(html, table, element, temp, &bg);

					object_type = OBJ_TABLE_FRAME;
				}
				break;

			case HT_TR:		/* table row */
				if(temp->is_end)	/* optional termination */
				{
					/* close current row */
					tableCloseRow(html, table, element);

					halign = (Alignment)StackPop(align_stack);
					bg = (Pixel)StackPop(bg_color_stack);
					object_type = OBJ_NONE;
				}
				else
				{
					/* open a row */
					StackPush(align_stack, halign);
					StackPush(bg_color_stack, bg);

					tableOpenRow(html, table, element, temp, &halign, &bg);

					object_type = OBJ_TABLE_FRAME;
				}
				break;

			case HT_TH:		/* header cell */
			case HT_TD:		/* regular cell */
				if(temp->is_end)	/* optional termination */
				{
					/* header cell used a bold font, restore */
					if(temp->id == HT_TH)
						font = (XmHTMLfont*)StackPopDouble(font_stack,basefont);

					/* close current cell */
					tableCloseCell(html, table, element);

					halign = (Alignment)StackPop(align_stack);
					bg = (Pixel)StackPop(bg_color_stack);

					object_type = OBJ_NONE;
				}
				else
				{
					/* header cell uses a bold font */
					if(temp->id == HT_TH)
					{
						StackPushDouble(font_stack, font, basefont);
						font = _XmHTMLLoadFont(html, HT_B, basefont, font);

						/* new height for linefeeds */
						fnheight = font->lineheight;
					}

					StackPush(align_stack, halign);
					StackPush(bg_color_stack, bg);

					/* open a cell */
					tableOpenCell(html, table, element, temp, &halign, &bg);

					/* table cell always resets linefeeding */
					(void)CheckLineFeed(CLEAR_SOFT, True, &text_data);

					object_type = OBJ_TABLE_FRAME;
				}
				break;

			/*****
			* According to HTML3.2, the following elements may not occur
			* inside the body content, but a *lot* of HTML documents are
			* in direct violation with this and the parser isn't always
			* successfully in removing them. So we need to handle these
			* elements as well and skip all data between the opening and
			* closing element.
			*****/
			case HT_STYLE:
				{
					htmlEnum end_id = temp->id;
					/* move past element */
					temp = temp->next;
					/* skip it entirely */
					for(; temp != NULL; temp = temp->next)
						if(temp->id == end_id && temp->is_end)
							break;
					ignore = True;
				}
				break;
			case HT_SCRIPT:
				{
					htmlEnum end_id = temp->id;

					if(HTML_ATTR(script_proc))
					{
						static XmHTMLScriptData script;
						String data;

						/* initialize */
						memset((void*)&script, 0, sizeof(script));

						/* set script attributes */
						script.type = _XmHTMLTagGetValue(temp->attributes, "type");
						script.lang = _XmHTMLTagGetValue(temp->attributes, "language");
						script.src = _XmHTMLTagGetValue(temp->attributes, "src");

						/* move past element */
						temp = temp->next;

						data = malloc(sizeof(char));
						data[0] = '\0';

						/* collect all enclosed text elements */
						for(; temp != NULL; temp = temp->next)
						{
							if(temp->id == end_id && temp->is_end)
								break;

							if(temp->element != NULL)
							{
								data = realloc(data,
									(strlen(data) +
									strlen(temp->element))*sizeof(char));
								strcat(data, temp->element);
							}
						}
						script.script = data;

						/* call user proc for this script */
						(void)HTML_ATTR(script_proc)((Widget)html,
							&script, HTML_ATTR(client_data));

						/* release again */
						if(script.type)
							free(script.type);
						if(script.lang)
							free(script.lang);
						if(script.src)
							free(script.src);
						free(script.script);
					}
					else
					{
						/* move past element */
						temp = temp->next;
						/* skip it entirely */
						for(; temp != NULL; temp = temp->next)
							if(temp->id == end_id && temp->is_end)
								break;
					}
					ignore = True;
				}
				break;

			default:
				_XmHTMLDebug(2, ("format.c: _XmHTMLformatObjects; "
					"Unused element %s.\n", temp->element));
				ignore = True;
		}
		if(!ignore)
		{
			/* unset break bit if this is not a text object */
			if(object_type != OBJ_TEXT)
				text_data &= ~TEXT_BREAK;
			else if(object_type == OBJ_BLOCK)
			{
				/* clear all spacing */
				text_data &= ~TEXT_SPACE_LEAD & ~TEXT_SPACE_TRAIL;
				text_data |= TEXT_SPACE_NONE;
			}

			/* adjust anchor count */
			if(element_data & ELE_ANCHOR)
			{
				text_data |= TEXT_ANCHOR;
				anchor_words += n_words;
				anchor_data_used = True;
			}
			/* mark object as internal anchor */
			if(element_data & ELE_ANCHOR_INTERN)
			{
				text_data |= TEXT_ANCHOR_INTERN;
				anchor_data_used = True;
				/* add an anchor id */
				element->id = named_anchors;
				named_anchors++;
			}
			element->text = text;
			element->text_data = text_data;
			element->words = words;
			element->n_words = n_words;
			element->width = width;
			element->height = height;
			element->fg = fg;
			element->bg = bg;
			element->font = font;
			element->marker = list_stack[current_list].marker;
			element->list_level = list_stack[current_list].level;
			element->table = table;
			/*
			* <dt> elements have an identation one less than the current.
			* all identation must use the default font (consistency).
			*/
			if(in_dt && ident_level)
				element->ident = (ident_level-1) * XmHTML_INDENT_SPACES *
					HTML_ATTR(default_font)->m_width;
			else
				element->ident = ident_level * XmHTML_INDENT_SPACES *
					HTML_ATTR(default_font)->m_width;

			element->linefeed = (int)((1+linefeed)*fnheight);

			/* stupid hack so HT_HR won't mess up alignment and color stack */
			if(temp->id != HT_HR)
			{
				element->halign = halign;
				element->y_offset = y_offset;
				element->x_offset = x_offset;
			}
			else if(HTML_ATTR(allow_color_switching) &&
						!HTML_ATTR(strict_checking))
				fg = (Pixel)StackPop(fg_color_stack);

			element->object_type = object_type;

			if(object_type == OBJ_BULLET)
				FillBullet(html, element, tka);

			/*****
			* If we have a form component of type <input type="image">, we
			* have promoted it to an anchor. Set this anchor data as the
			* anchor for this element and, as it is used only once, reset
			* it to NULL. In all other case we have a plain anchor.
			*
			* Note: as form components are allowed inside anchors, this is
			* the only place in which we can possibly have nested anchors.
			* This is a problem we will have to live with...
			*****/
			if(form_anchor_data)
			{
				element->anchor = form_anchor_data;
				form_anchor_data = NULL;
				element_data &= ~ELE_ANCHOR;
			}
			else
				element->anchor = anchor_data;

			/*****
			* Discard non-spacing bits so we only carry current spacing
			* to the next chunk of text. Break bit must be kept to carry
			* lack of leading space over to the next chunk of text.
			*****/
			text_data &= (~TEXT_ANCHOR & ~TEXT_ANCHOR_INTERN &
							~TEXT_IMAGE & ~TEXT_FORM); /* & ~TEXT_BREAK); */

			InsertTableElement(element, element_data & ELE_ANCHOR);
		}
#ifdef DEBUG
		else /* element will be reused if it was ignored */
			num_ignore++;
#endif

		/* move to next element */
		temp = temp->next;
	}
	/* if there still is an open table, close it now */
	if(table)
		tableClose(html, table, element);

	/*****
	* Insert a dummy element at the end of the list, saves some NULL tests
	* in the layout routines.
	*****/
	if(ignore)
	{
		/* reuse ignored element */
		memset(element, 0, sizeof(XmHTMLObjectTable));
		element->object_type = OBJ_NONE;
	}
	else
		NewTableElement(element,NULL);

	/* each element must have a font with it */
	element->font = HTML_ATTR(default_font);

	InsertTableElement(element, False);

	/*
	* Some sucker forget to terminate a list and parser failed to repair it.
	* Spit out a warning.
	*/
	if(HTML_ATTR(bad_html_warnings) && ident_level != 0)
		_XmHTMLWarning(__WFUNC__(html, func), XMHTML_MSG_51);

	/* clear all allocated stacks */
#ifdef DEBUG
	if((i = StackDestroy(fg_color_stack)) != 0)
		_XmHTMLWarning(__WFUNC__(html, func), XMHTML_MSG_52,
			"Foreground color", i);
	if((i = StackDestroy(bg_color_stack)) != 0)
		_XmHTMLWarning(__WFUNC__(html, func), XMHTML_MSG_52,
			"Background color", i);
	if((i = StackDestroy(align_stack)) != 0)
		_XmHTMLWarning(__WFUNC__(html, func), XMHTML_MSG_52,
			"Horizontal Alignment", i);
	if((i = StackDestroy(font_stack)) != 0)
		_XmHTMLWarning(__WFUNC__(html, func), XMHTML_MSG_52,
			"Font", i);
#else
	(void)StackDestroy(fg_color_stack);
	(void)StackDestroy(bg_color_stack);
	(void)StackDestroy(align_stack);
	(void)StackDestroy(font_stack);
#endif

	/*
	* allocate memory for all anchor words in this document, gets filled in
	* paint.c
	*/
	if(anchor_words)
	{
		HTML_ATTR(anchors)= (XmHTMLWord*)calloc(anchor_words+1,
			sizeof(XmHTMLWord));

		_XmHTMLDebug(2,("_XmHTMLFormatObjects: anchors contain %i words\n",
			anchor_words));
		HTML_ATTR(anchor_words) = anchor_words;
	}

	/* allocated memory for all named anchors. Gets filled in paint.c */
	if(named_anchors)
	{
		HTML_ATTR(named_anchors) =
			(XmHTMLObjectTable*)calloc(named_anchors+1,
				sizeof(XmHTMLObjectTable));

		HTML_ATTR(num_named_anchors) = named_anchors;
	}

	_XmHTMLDebug(2,("_XmHTMLFormatObjects: formatted %li elements of which %li"
		" anchors.\n", list_data.num_elements, list_data.num_anchors));
	_XmHTMLDebug(2,("_XmHTMLFormatObjects: found %i named anchors.\n",
		named_anchors));
	_XmHTMLDebug(2, ("_XmHTMLformatObjects, allocated %i elements and "
		"ignored %i objects.\n", allocated, num_ignore));

	_XmHTMLDebug(2, ("_XmHTMLformatObjects, allocated %i XmHTMLAnchor "
		"objects\n", new_anchors));

	/* store the anchor list */
	HTML_ATTR(anchor_data) = list_data.anchor_head;

	/* Since the table head is a dummy element, we return the next one */
	list_data.current = list_data.head->next;

	/* this is *required* */
	if(list_data.current)
		list_data.current->prev = NULL;

	/* free top of the list */
	free(list_data.head);
	list_data.head = NULL;

	/* store it */
	HTML_ATTR(formatted) = list_data.current;

	/* all done! */
}

/********
****** Public XmHTML Functions
********/

/* all url types we know about, 17 in total */
static String anchor_tokens[] = {"about", "exec", "file", "ftp", "gopher",
	"help", "http", "https" ,"info", "mailto", "man", "news", "pipe", "telnet",
	"wais", "xexec", "zzz"};

/*****
* Name: 		XmHTMLGetURLType
* Return Type: 	URLType
* Description: 	tries to figure out what type of url the given href is
* In:
*	href:		url specification
* Returns:
*	type of url when we know it, ANCHOR_UNKNOWN otherwise.
*****/
URLType
XmHTMLGetURLType(String href)
{
	char *chPtr;

	if(href == NULL || *href == '\0')
		return(ANCHOR_UNKNOWN);

	_XmHTMLDebug(2, ("format.c: XmHTMLGetURLType; checking url type of %s\n",
		href));

	/* first pick up any leading url spec */
	if((chPtr = strstr(href, ":")) != NULL && (chPtr - href) < 7)
	{
		char token[7];
		URLType ret_val;

		strncpy(token, href, chPtr - href);
		token[chPtr - href] = '\0';	/* NULL terminate */

		ret_val = (URLType)stringToToken(token, anchor_tokens,
					(int)ANCHOR_UNKNOWN);

#ifdef DEBUG
		if(ret_val == ANCHOR_UNKNOWN)
			_XmHTMLDebug(2, ("format.c: XmHTMLGetURLType; unknown type %s\n",
				token));
#endif
		return(ret_val);
	}
	return(href[0] == '#' ? ANCHOR_JUMP : ANCHOR_FILE_LOCAL);
}
