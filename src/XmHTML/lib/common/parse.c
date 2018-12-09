#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* parse.c : XmHTML HTML parser
*
* This file Version	$Revision$
*
* Creation date:		Wed Nov 13 00:33:27 GMT+0100 1996
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
* Revision 1.2  2012/03/01 17:56:31  ziad
* Cput
*
* Revision 1.1  2011/06/30 16:10:38  rwcox
* Cadd
*
* Revision 1.20  1998/04/27 07:02:33  newt
* Fixed an annoying buffer overrun in _ParserStoreElement
*
* Revision 1.19  1998/04/04 06:28:23  newt
* XmHTML Beta 1.1.3
*
* Revision 1.18  1997/10/23 00:25:08  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.17  1997/08/31 17:37:26  newt
* removed HT_TEXTFLOW
*
* Revision 1.16  1997/08/30 01:22:19  newt
* _XmHTMLWarning proto changes.
* Fixed parser to remove out of order style and script elements.
* Fixed quote detection, unbalanced quotes inside tags are now properly
* recognized.
* Fixed the main parser so SetValues can be called from within the
* XmNdocumentCallback.
* Removed all progressive parsing stuff, it was unused and didn't work
* properly either.
*
* Revision 1.15  1997/08/01 13:07:07  newt
* Reduced data storage. Minor bugfixes in HTML rules. Added state stack
* backtracking and updated comments (again...).
*
* Revision 1.14  1997/05/28 01:53:43  newt
* Bugfixes in comment parsing. Modified the parser to properly deal with the
* contents of the <SCRIPT> and <STYLE> head attributes.
*
* Revision 1.13  1997/04/29 14:30:48  newt
* Removed ParserCallback stuff.
*
* Revision 1.12  1997/04/03 05:40:54  newt
* #ifdef PEDANTIC/#endif changes
*
* Revision 1.11  1997/03/28 07:23:01  newt
* More changes in document verification/repair.
* Implemented parserCallback stuff.
* Frameset support added.
* XmNmimeType changes: split _XmHTMLparseHTML into parseHTML, parsePLAIN
* and parseIMAGE.
*
* Revision 1.10  1997/03/20 08:13:16  newt
* Major changes: almost a full rewrite and integrated document verification
* and repair.
*
* Revision 1.9  1997/03/11 19:58:06  newt
* added a third argument to _XmHTMLTagGetNumber: default return value.
* Added _XmHTMLGetImageAlignment
*
* Revision 1.8  1997/03/04 18:49:04  newt
* ?
*
* Revision 1.7  1997/03/04 01:01:53  newt
* CheckTermination: changed <p> handling
*
* Revision 1.6  1997/03/02 23:22:48  newt
* Sneaky bugfix in expandEscapes (Dick Porter, dick@cymru.net); Sanity check
* in storeTextElement changed to check if len <= 0 instead of <= 1
*
* Revision 1.5  1997/02/11 02:10:13  newt
* Added support for SGML shorttags. Re-ordered all switch statements
*
* Revision 1.4  1997/02/04 02:53:18  newt
* state checking now checks for overlapping and missing closing elements.
* Added the basefont element.
*
* Revision 1.3  1997/01/09 06:55:50  newt
* expanded copyright marker
*
* Revision 1.2  1997/01/09 06:47:00  newt
* a few bugfixes in XmHTMLTagCheck and XmHTMLTagGetValue
*
* Revision 1.1  1996/12/19 02:17:13  newt
* Initial Revision
*
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*****
* When MINIPARSE is defined during compilation, a standalone HTML parser
* is generated (see tools/miniparse.c for an example implementation).
* The interface for this parser is defined in the include file miniparse.h.
* Since I'm using this to test & debug new features in the HTML parser,
* DEBUG is automatically defined in miniparse.h, *unless* NDEBUG was
* defined.
*****/
#ifdef MINIPARSE
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#include "miniparse.h"
#define XmHTML_STRINGDEFINES
#include "HTMLWarnings.h"
#undef XmHTML_STRINGDEFINES
#include "icons.h"				/* W3C Working Draft WD-wwwicn-960729 */
#else
#include "toolkit.h"
#include XmHTMLPrivateHeader
#endif

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*****
* HTML Element names.
* This list is alphabetically sorted to speed up the searching process.
* DO NOT MODIFY
*****/
static String __XmHTML_TOKENS_HTML_32__[] =
{"!doctype", "a", "address", "applet", "area", "b", "base", "basefont", "big",
"blockquote", "body", "br", "caption", "center", "cite", "code", "dd", "dfn",
"dir", "div", "dl", "dt", "em", "font", "form", "frame", "frameset", "h1",
"h2", "h3", "h4", "h5", "h6", "head", "hr", "html", "i", "img", "input",
"isindex", "kbd", "li", "link", "map", "menu", "meta", "noframes", "ol",
"option", "p", "page", "param", "pre", "samp", "script", "select", "small",
"strike", "strong", "style", "sub", "sup", "tab", "table", "td", "textarea",
"th", "title", "tr", "tt", "u", "ul", "var", "plain text"};

/*****
* Basic set of HTML 3.2 tokens we recognize. User-specific tokens are
* appended to another array
*****/
String *html_tokens = (String*)__XmHTML_TOKENS_HTML_32__;
static const int last_std_token = HT_ZTEXT;
static int no_html_tokens = HT_ZTEXT;

/*****
* User defined objects
*****/
XmHTMLParserTag **non_std_token = NULL;
int no_non_std_tokens = 0;

#define IsUserTag(id) ((id) > last_std_token && (id) < no_html_tokens)

#ifdef MINIPARSE
Boolean parser_strict_checking = True;
Boolean parser_debug = False;
Boolean parser_verification_timings = False;
Boolean parser_warnings = True;
int parser_errors = 0;
int parsed_object_count = 0;
int parsed_text_object_count = 0;
ParserDocumentCallback parser_document_callback = NULL;
ParserAutoCorrectCallback parser_autocorrect_callback = NULL;
#endif

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/

/* store a real element. Skips verification */
#ifndef MINIPARSE
static String storeElementUnconditional(Parser *parser, char *start, char *end);
#endif

/* parserCallback driver */
static void parserWarning(Parser *parser, htmlEnum id, htmlEnum current,
	parserError error);

/*****
* Only call the warner when we may issue warnings and this isn't a
* notification message (cheaper to do this here than have a useless function
* call).
*****/
#define parserCallback(PARSER,ID,CURR,ERR) do {\
	if(PARSER->warn != XmHTML_NONE && ERR != HTML_NOTIFY) \
		parserWarning(PARSER, ID, CURR, ERR); \
}while(0)

/* main HTML parser driver */
static XmHTMLObject *parserDriver(XmHTMLWidget html, XmHTMLObject *old_list,
	String input);

/* HTML parser */
static void parseHTML(Parser *parser);

#ifndef MINIPARSE

/* Fast HTML parser for HTML3.2 conformant documents */
static void parsePerfectHTML(Parser *parser);

/* plain text parser */
static void parsePLAIN(Parser *parser);

/* image parser */
static void parseIMAGE(Parser *parser);

#else

#ifdef DEBUG
#define _XmHTMLDebug(LEVEL,MSG) {\
	if(parser_debug) { printf MSG ; fflush(stdout); } \
}
#else
#define _XmHTMLDebug(LEVEL,MSG) /* */
#endif	/* DEBUG */
#endif	/* MINIPARSE */

#if defined(DEBUG) && !defined(MINIPARSE)
#include <errno.h>	/* for perror */
static void writeParsedOutputToFile(XmHTMLObject *objects, String prefix);
#endif

/*** Private Variable Declarations ***/
static Boolean bad_html, html32;

#ifdef DEBUG
static int num_lookups, num_searches, p_inserted, p_removed;
#endif

#ifdef MINIPARSE
static Boolean tag_is_wrong_but_allowed;
static int id_depth;
#endif

/*****
* Name: 		_ParserTokenToId
* Return Type: 	int
* Description: 	converts the html token passed to an internal id.
* In:
*	parser:		current parser context
*	token:		token for which to fetch an internal id.
*	warn:		if true, spits out a warning for unknown tokens.
* Returns:
*	The internal id upon success, -1 upon failure
*
* Note: this routine uses a binary search into an array of all possible
*	HTML 3.2 tokens. It is very important that _BOTH_ the array
*	html_tokens _AND_ the enumeration htmlEnum are *NEVER* changed.
*	Both arrays are alphabetically sorted. Modifying any of these two
*	arrays will	have VERY SERIOUS CONSEQUENCES, te return value of this
*	function matches a corresponding htmlEnum value.
*	As the table currently contains about 70 elements, a match will always
*	be found in at most 7 iterations (2^7 = 128)
*****/
htmlEnum
_ParserTokenToId(Parser *parser, String token, Boolean warn)
{
	register int mid, lo = 0, hi = HT_ZTEXT-1;
	int cmp;

#ifdef DEBUG
	num_lookups++;
#endif

	while(lo <= hi)
	{
#ifdef DEBUG
		num_searches++;
#endif
		mid = (lo + hi)/2;
		if((cmp = strcmp(token, html_tokens[mid])) == 0)
			return(mid);

		else
			if(cmp < 0)				/* in lower end of array */
				hi = mid - 1;
			else					/* in higher end of array */
				lo = mid + 1;
	}

	/*****
	* Not found. Check if it's a user defined token.
	*****/
	for(mid = last_std_token + 1; mid < no_html_tokens; mid++)
	{
#ifdef DEBUG
		num_searches++;
#endif
		if(!strcmp(token, html_tokens[mid]))
			return(mid);
	}

	/*
	* Not found, invalid token passed
	* We don't want always have warnings. When XmNhandleShortTags is set to
	* True, this routine is used to check whether we a / is right behind a
	* token or not.
	*/
	if(warn)
		parserCallback(parser, HT_ZTEXT, HT_ZTEXT, HTML_UNKNOWN_ELEMENT);
	return(-1);
}

/*****
* Name: 		_ParserTokenToIcon
* Return Type: 	char
* Description: 	converts the HTML & icon escapes sequences to the icon
*				images.
* In:
*	**icon:		icon sequence to convert. This argument is updated upon
*				return.
*	*index:		index of icon in array of icon entities.
* Returns:
*	On success, an index in the array of icon entities representing.
*	-1 on failure.
*****/
static int
_ParserTokenToIcon(char **icon)
{
	register int mid, lo = 0, hi = NUM_ESCAPE_ICONS - 1;
	int cmp, skip = 1;

	/* search the list of defined icon entities */
	while(lo <= hi)
	{
		mid = (lo + hi)/2;
		if((cmp = strncmp(*icon+1, _XmHTMLIconEntities[mid].escape,
			_XmHTMLIconEntities[mid].len - skip)) == 0)
		{
			/* skip escape sequence and return index in icon array */
			*icon += _XmHTMLIconEntities[mid].len;
			return(mid);
		}
		else
			if(cmp < 0)				/* in lower end of array */
				hi = mid - 1;
			else					/* in higher end of array */
				lo = mid + 1;
	}
	/* failed to match */
	return(-1);
}

/*****
* Name: 		_ParserPushState
* Return Type: 	void
* Description: 	pushes the given id on the state stack
* In:
*	parser:		current parser context
*	id:			element id to push
* Returns:
*	nothing.
*****/
void
_ParserPushState(Parser *parser, htmlEnum id)
{
	stateStack *tmp;

	tmp = (stateStack*)malloc(sizeof(stateStack));
	tmp->id = id;
	tmp->next = parser->state_stack;
	parser->state_stack = tmp;

#ifdef MINIPARSE
	id_depth++;
	{
		int i;
		_XmHTMLDebug(4, ("%i: ", id_depth));
		if(id_depth < 10)
			for(i = 0; i < id_depth; i++)
			{
				_XmHTMLDebug(4, ("\t"));
			}
		else
		{
			_XmHTMLDebug(4, ("\t\t\t\t\t...\t\t\t\t"));
		}
		_XmHTMLDebug(4, ("parse.c: pushed %s (line %i)\n", html_tokens[id],
			parser->num_lines));
	}
#endif
}

/*****
* Name: 		_ParserPopState
* Return Type: 	htmlEnum
* Description: 	pops an element of the state stack
* In:
*	parser:		current parser context
* Returns:
*	id of element popped.
*****/
htmlEnum
_ParserPopState(Parser *parser)
{
	htmlEnum id;
	stateStack *tmp;

	if(parser->state_stack->next != NULL)
	{
		tmp = parser->state_stack;
		parser->state_stack = parser->state_stack->next;
		id = tmp->id;
		free((char*)tmp);
	}
	else
		id = parser->state_stack->id;

#ifdef MINIPARSE
	id_depth--;
	{
		int i;
		_XmHTMLDebug(4, ("%i: ", id_depth+1));
		if(id_depth < 9)
			for(i = 0; i < id_depth+1; i++)
			{
				_XmHTMLDebug(4, ("\t"));
			}
		else
		{
			_XmHTMLDebug(4, ("\t\t\t\t\t...\t\t\t\t"));
		}
		_XmHTMLDebug(4, ("parse.c: popped %s (line %i)\n", html_tokens[id],
			parser->num_lines));
	}
#endif

	return(id);
}

/*****
* Name: 		_ParserClearStack
* Return Type: 	void
* Description: 	clears and resets the state stack of a parser
* In:
*	parser:		current parser context
* Returns:
*	nothing
*****/
void
_ParserClearStack(Parser *parser)
{
	stateStack *tmp = parser->state_stack;

	while(tmp->next != NULL)
		(void)_ParserPopState(parser);

	/* initialize the stateStack */
	parser->state_stack->id = HT_DOCTYPE;
	parser->state_stack->next = NULL;
	parser->state_stack = &parser->state_base;
}

/*****
* Name: 		_ParserOnStack
* Return Type: 	Boolean
* Description: 	checks whether the given id is somewhere on the current
*				state stack.
* In:
*	parser:		current parser context
*	id:			element id to check.
* Returns:
*	True when present, False if not.
*****/
Boolean
_ParserOnStack(Parser *parser, htmlEnum id)
{
	stateStack *tmp = parser->state_stack;

	while(tmp->next != NULL && tmp->id != id)
		tmp = tmp->next;
	return(tmp->id == id);
}

/**********
***** Element Rules
**********/

/*****
* Name: 		_ParserIsElementTerminated
* Return Type: 	Boolean
* Description: 	checks if the given element has a terminating counterpart
* In:
*	id:			element to check
* Returns:
*	True when the given element is terminated, false if not.
*****/
#define _ParserIsElementTerminated(id) \
	(!(id == HT_BR || id == HT_IMG || id == HT_HR || id == HT_INPUT || \
		id == HT_INPUT || id == HT_AREA || id == HT_META || \
		id == HT_DOCTYPE || id == HT_LINK || id == HT_FRAME || \
		id == HT_BASE || id == HT_BASEFONT || id == HT_ISINDEX || \
		id == HT_ZTEXT || id == HT_TAB) || \
		(IsUserTag(id) && non_std_token[id]->terminated))

/*****
* Name: 		_ParserIsBodyElement
* Return Type: 	Boolean
* Description: 	checks whether the given id is allowed to appear inside the
*				<BODY> tag.
* In:
*	id:			id to check.
* Returns:
*	True when allowed, False if not.
*****/
#define _ParserIsBodyElement(id) \
	(!(id == HT_DOCTYPE || id == HT_BASE || id == HT_HTML || id == HT_HEAD || \
		id == HT_LINK || id == HT_META || id == HT_STYLE || id == HT_TITLE || \
		id == HT_FRAMESET || id == HT_FRAME || id == HT_SCRIPT || \
		id == HT_ZTEXT))

/*****
* Name:			_ParserCheckElementOccurance
* Return Type:	Boolean
* Description:	checks whether the appearence of the current token is
*				allowed in the current parser state.
* In:
*	parser:		current parser context
*	current:	HTML token to check;
*	state:		parser state;
* Returns:
*	When current is not allowed, the id of the element that should be
*	preceeding this one. If no suitable preceeding element can be deduced,
*	it returns -1. When the element is allowed, HT_ZTEXT is returned.
*****/
int
_ParserCheckElementOccurance(Parser *parser, htmlEnum current,
	htmlEnum state)
{
	stateStack *curr;

	switch(current)
	{
		case HT_DOCTYPE:
			return((int)HT_ZTEXT); /* always allowed */

		case HT_HTML:
			if(state == HT_DOCTYPE)
				return((int)HT_ZTEXT);
			return(-1);

		case HT_BODY:
			if(state == HT_HTML || state == HT_FRAMESET)
				return((int)HT_ZTEXT);
			else
			{
				/* try and guess an appropriate return value */
				if(state == HT_HEAD)
					return((int)HT_HEAD);
				else
					return((int)HT_HTML);
			}
			return(-1);	/* not reached */

		case HT_HEAD:
			if(state == HT_HEAD)	/* head may not be nested */
				return(-1);

			/* fall through */

		case HT_FRAMESET:
			/* frames may be nested */
			if(state == HT_HTML || state == HT_FRAMESET)
				return((int)HT_ZTEXT);
			else
				return((int)HT_HTML); /* obvious */
			break;

		case HT_NOFRAMES:
			if(state == HT_HTML)
				return((int)HT_ZTEXT);
			else
				return((int)HT_HTML); /* obvious */
			break;

		case HT_FRAME:
			if(state == HT_FRAMESET)
				return((int)HT_ZTEXT);
			else
				return((int)HT_FRAMESET); /* obvious */
			break;

		case HT_SCRIPT:
		case HT_STYLE:
			if(state == HT_HEAD)
				return(HT_ZTEXT);
			if(!parser->strict_checking)
			{
				parserCallback(parser, current, state, HTML_VIOLATION);
			return(-1);
				return(HT_ZTEXT);
			}
			return(_ParserOnStack(parser, HT_HEAD) ? -1 : (int)HT_HEAD);
			break;

		case HT_BASE:
		case HT_ISINDEX:
		case HT_META:
		case HT_LINK:
		case HT_TITLE:
			if(state == HT_HEAD)
				return((int)HT_ZTEXT); /* only allowed in the <HEAD> section */
			else
				return(_ParserOnStack(parser, HT_HEAD) ? -1 : (int)HT_HEAD);
			break;

		case HT_IMG:
			if(state == HT_PRE)
			{
				/* strictly speaking, images are not allowed inside <pre> */
				if(!parser->strict_checking)
					parserCallback(parser, current, state, HTML_VIOLATION);
				else
					return(-1);
			}
			if(IS_CONTAINER(state) || IS_MARKUP(state) || IS_MISC(state))
				return((int)HT_ZTEXT);
			else
				return(-1); /* too bad, obliterate it */

		case HT_A:
			if(state == HT_A)
				return(-1); /* no nested anchors */
			/* fall thru, all these elements may occur in the given context */
		case HT_APPLET:
		case HT_B:
		case HT_BASEFONT:
		case HT_BIG:
		case HT_BR:
		case HT_CITE:
		case HT_CODE:
		case HT_DFN:
		case HT_EM:
		case HT_FONT:
		case HT_I:
		case HT_INPUT:
		case HT_KBD:
		case HT_MAP:
		case HT_SMALL:
		case HT_SAMP:
		case HT_SELECT:
		case HT_STRIKE:
		case HT_STRONG:
		case HT_SUB:
		case HT_SUP:
		case HT_TAB:
		case HT_TEXTAREA:
		case HT_TT:
		case HT_U:
		case HT_VAR:
			if(IS_CONTAINER(state) || IS_MARKUP(state) || IS_MISC(state))
				return((int)HT_ZTEXT);
			else
				return(-1); /* too bad, obliterate it */

		case HT_ZTEXT:
				return(HT_ZTEXT);  /* always allowed */

		case HT_AREA:		/* only allowed when inside a <MAP> */
			if(state == HT_MAP)
				return((int)HT_ZTEXT);
			else
				return((int)HT_MAP); /* obvious */
			break;

		case HT_P:
			if(state == HT_ADDRESS || IS_CONTAINER(state))
				return((int)HT_ZTEXT);
			/* guess a proper return value */
			switch(state)
			{
				case HT_OL:
				case HT_UL:
				case HT_DIR:
				case HT_MENU:
					return((int)HT_LI);
				case HT_TABLE:
					return((int)HT_TD);
				case HT_DL:
					return((int)HT_DD);
				default:
					/*****
					* strictly speaking, <p> should not be happening, but
					* as this is one of the most abused elements, allow
					* for it if we haven't been told to be strict.
					*****/
#if 0
					if(!parser->strict_checking)
#endif
					{
						parserCallback(parser, current, state, HTML_VIOLATION);
						return((int)HT_ZTEXT);
					}
					return(-1); /* too bad, obliterate it */
			}
			return(-1);	/* not reached */

		case HT_FORM:
			if(state == HT_FORM)
				return(-1); /* no nested forms */
			if(!parser->strict_checking)
			{
				/* allow in any state if not being strict */
				parserCallback(parser, current, state, HTML_VIOLATION);
				return(HT_ZTEXT);
			}
			/* fall thru otherwise */
		case HT_ADDRESS:
		case HT_BLOCKQUOTE:
		case HT_CENTER:
		case HT_DIV:
		case HT_H1:
		case HT_H2:
		case HT_H3:
		case HT_H4:
		case HT_H5:
		case HT_H6:
		case HT_HR:
		case HT_TABLE:
		case HT_DIR:
		case HT_MENU:
		case HT_DL:
		case HT_PRE:
		case HT_OL:
		case HT_UL:
		case HT_PAGE:		/* XmHTML Page Extension */
			if(IS_CONTAINER(state))
				return((int)HT_ZTEXT);
			/* correct for most common errors */
			switch(state)
			{
				case HT_OL:
				case HT_UL:
				case HT_DIR:
				case HT_MENU:
					return((int)HT_LI);
				case HT_TABLE:
					return((int)HT_TD);
				case HT_DL:
					return((int)HT_DD);
				default:
					/*
					* Almost everyone ignores the fact that horizontal
					* rules may *only* occur in container elements and
					* nowhere else. We can safely loosen this when we are
					* told not to be strict as it is a single element.
					*
					* bad hack: only check when we are being used
					* by XmHTML's internal parser
					*/
					if(current == HT_HR && parser->automatic &&
						!parser->strict_checking)
					{
						parserCallback(parser, current, state, HTML_VIOLATION);
						return(HT_ZTEXT);
					}
					return(-1); /* too bad, obliterate it */
			}
			return(-1);	/* not reached */

		case HT_LI:
			if(state == HT_UL || state == HT_OL || state == HT_DIR ||
				state == HT_MENU)
				return((int)HT_ZTEXT);
			/*
			* Guess a return value: walk the current parser state and
			* see if a list is already present. If it's not, return HT_UL,
			* else return -1.
			*/
			for(curr = parser->state_stack; curr->next != NULL;
				curr = curr->next)
			{
				if(curr->id == HT_UL || curr->id == HT_OL ||
					curr->id == HT_DIR || curr->id == HT_MENU)
					return(-1);
			}
			return((int)HT_UL); /* start a new list */

		case HT_DT:
		case HT_DD:
			if(state == HT_DL)
				return((int)HT_ZTEXT);
			return(_ParserOnStack(parser, HT_DL) ? -1 : (int)HT_DL);

		case HT_OPTION:		/* Only inside the SELECT element */
			if(state == HT_SELECT)
				return((int)HT_ZTEXT);
			else
				return((int)HT_SELECT); /* obvious */
			break;

		case HT_CAPTION: /* Only allowed in TABLE */
		case HT_TR:
			if(state == HT_TABLE)
				return((int)HT_ZTEXT);
			/* no smart guessing here, it completely fucks things up */
			return(-1);

		case HT_TD:
		case HT_TH:
			/* Only allowed when in a table row */
			if(state == HT_TR)
				return((int)HT_ZTEXT);
			/* nested cells are not allowed, so insert another row */
			if(state == current || state == HT_TABLE)
				return(HT_TR);
			/* final check: insert a row when one is not present on the stack */
			return(_ParserOnStack(parser, HT_TR) ? -1 : (int)HT_TR);

		case HT_PARAM: /* Only allowed in applets */
			if(state == HT_APPLET)
				return((int)HT_ZTEXT);
			else
				return((int)HT_APPLET); /* obvious */
			break;
		/* no default so w'll get a warning when we miss anything */
	}

	/****
	* Check if this is a user-defined object. They can not be checked
	* and we always allow them.
	*****/
	if(IsUserTag(current))
		return((int)HT_ZTEXT);

	return(-1);
}

/*****
* Name:			_ParserCheckElementContent
* Return Type:	Boolean
* Description:	checks whether the appearence of the current token is valid in
*				the current state.
* In:
*	parser:		current parser context
*	current:	token to check
*	state:		current state of the parser
* Returns:
*	True if the current token is in a valid state. False otherwise.
*****/
Boolean
_ParserCheckElementContent(Parser *parser, htmlEnum current, htmlEnum state)
{
	/* plain text and user tags are always allowed */
	if(current == HT_ZTEXT || IsUserTag(current))
		return(True);

	switch(state)
	{
		case HT_DOCTYPE:
			return(True);

		case HT_HTML:
			if(current == HT_HTML || current == HT_BODY ||
				current == HT_HEAD || current == HT_FRAMESET)
				return(True);
			break;

		case HT_FRAMESET:
			if(current == HT_FRAME || current == HT_FRAMESET)
				return(True);
			break;

		case HT_HEAD:
			if(current == HT_TITLE || current == HT_ISINDEX ||
				current == HT_BASE || current == HT_SCRIPT ||
				current == HT_STYLE || current == HT_META ||
				current == HT_LINK)
				return(True);
			break;

		case HT_NOFRAMES:
		case HT_BODY:
			if(current == HT_A || current == HT_ADDRESS ||
				current == HT_APPLET || current == HT_B ||
				current == HT_BIG || current == HT_BLOCKQUOTE ||
				current == HT_BR || current == HT_CENTER ||
				current == HT_CITE || current == HT_CODE ||
				current == HT_DFN || current == HT_DIR ||
				current == HT_DIV || current == HT_DL ||
				current == HT_EM || current == HT_FONT ||
				current == HT_FORM || current == HT_NOFRAMES ||
				current == HT_H1 || current == HT_H2 ||
				current == HT_H3 || current == HT_H4 ||
				current == HT_H5 || current == HT_H6 ||
				current == HT_HR || current == HT_I ||
				current == HT_IMG || current == HT_INPUT ||
				current == HT_KBD || current == HT_MAP ||
				current == HT_MENU || current == HT_OL ||
				current == HT_P || current == HT_PRE ||
				current == HT_SAMP || current == HT_SELECT ||
				current == HT_SMALL || current == HT_STRIKE ||
				current == HT_STRONG || current == HT_SUB ||
				current == HT_SUP || current == HT_TABLE ||
				current == HT_TEXTAREA || current == HT_TT ||
				current == HT_U || current == HT_UL ||
				current == HT_VAR || current == HT_ZTEXT)
				return(True);
			if(!parser->strict_checking)
			{
#ifdef MINIPARSE
				tag_is_wrong_but_allowed = True;
#endif
				/* but always issue a warning */
				parserCallback(parser, current, state, HTML_VIOLATION);
				return(True);
			}
			break;

		case HT_B:
		case HT_CITE:
		case HT_DFN:
		case HT_EM:
		case HT_FONT:
		case HT_I:
		case HT_KBD:
		case HT_STRONG:
		case HT_TT:
		case HT_VAR:
			if(current == HT_A || current == HT_APPLET ||
				current == HT_B || current == HT_BIG ||
				current == HT_BR || current == HT_CITE ||
				current == HT_CODE || current == HT_DFN ||
				current == HT_EM || current == HT_FONT ||
				current == HT_I || current == HT_IMG ||
				current == HT_INPUT || current == HT_KBD ||
				current == HT_MAP || current == HT_NOFRAMES ||
				current == HT_SAMP || current == HT_SCRIPT ||
				current == HT_SELECT || current == HT_SMALL ||
				current == HT_STRIKE || current == HT_STRONG ||
				current == HT_SUB || current == HT_SUP ||
				current == HT_TEXTAREA || current == HT_TT ||
				current == HT_U || current == HT_VAR ||
				current == HT_ZTEXT)
				return(True);

			/* allow most container elements as well if we can relax */
			if(!parser->strict_checking &&
				(current == HT_TABLE || current == HT_TR || current == HT_TH ||
					current == HT_TD || current == HT_OL || current == HT_UL ||
					current == HT_DL || current == HT_P || current == HT_DIV ||
					current == HT_BLOCKQUOTE || current == HT_CENTER ||
					current == HT_FORM || current == HT_CAPTION ||
					current == HT_H1 || current == HT_H2 || current == HT_H3 ||
					current == HT_H4 || current == HT_H5 || current == HT_H6))
			{
#ifdef MINIPARSE
				tag_is_wrong_but_allowed = True;
#endif
				/* but always issue a warning */
				parserCallback(parser, current, state, HTML_VIOLATION);
				return(True);
			}
			break;

		case HT_ADDRESS:
			if(current == HT_P)
				return(True);
			/* fall thru, these elements are also allowed */
		case HT_A:
		case HT_BIG:
		case HT_CAPTION:
		case HT_CODE:
		case HT_P:
		case HT_SAMP:
		case HT_SMALL:
		case HT_STRIKE:
		case HT_SUB:
		case HT_SUP:
		case HT_TAB:
		case HT_U:
			if(current == HT_A || current == HT_APPLET ||
				current == HT_B || current == HT_BIG ||
				current == HT_BR || current == HT_CITE ||
				current == HT_CODE || current == HT_DFN ||
				current == HT_EM || current == HT_FONT ||
				current == HT_I || current == HT_IMG ||
				current == HT_INPUT || current == HT_KBD ||
				current == HT_MAP || current == HT_NOFRAMES ||
				current == HT_SAMP || current == HT_SCRIPT ||
				current == HT_SELECT || current == HT_SMALL ||
				current == HT_STRIKE || current == HT_STRONG ||
				current == HT_SUB || current == HT_SUP ||
				current == HT_TEXTAREA || current == HT_TT ||
				current == HT_U || current == HT_VAR ||
				current == HT_ZTEXT)
				return(True);
			break;

		case HT_DT:
			if(current == HT_A || current == HT_APPLET ||
				current == HT_B || current == HT_BIG ||
				current == HT_BR || current == HT_CITE ||
				current == HT_CODE || current == HT_DFN ||
				current == HT_EM || current == HT_FONT ||
				current == HT_I || current == HT_IMG ||
				current == HT_INPUT || current == HT_KBD ||
				current == HT_MAP || current == HT_NOFRAMES ||
				current == HT_SAMP || current == HT_SCRIPT ||
				current == HT_SELECT || current == HT_SMALL ||
				current == HT_STRIKE || current == HT_STRONG ||
				current == HT_SUB || current == HT_SUP ||
				current == HT_TEXTAREA || current == HT_TT ||
				current == HT_U || current == HT_VAR ||
				current == HT_ZTEXT)
				return(True);
			/*****
			* Allow for nesting of <dl> inside <dt> if we are not being
			* strict.
			*****/
			else if(current == HT_DL && !parser->strict_checking)
			{
#ifdef MINIPARSE
				tag_is_wrong_but_allowed = True;
#endif
				/* but always issue a warning */
				parserCallback(parser, current, state, HTML_VIOLATION);
				return(True);
			}
			break;

		case HT_H1:
		case HT_H2:
		case HT_H3:
		case HT_H4:
		case HT_H5:
		case HT_H6:
			if(current == HT_A || current == HT_APPLET ||
				current == HT_B || current == HT_BIG ||
				current == HT_BR || current == HT_CITE ||
				current == HT_CODE || current == HT_DFN ||
				current == HT_EM || current == HT_FONT ||
				current == HT_I || current == HT_IMG ||
				current == HT_INPUT || current == HT_KBD ||
				current == HT_MAP || current == HT_NOFRAMES ||
				current == HT_SAMP || current == HT_SCRIPT ||
				current == HT_SELECT || current == HT_SMALL ||
				current == HT_STRIKE || current == HT_STRONG ||
				current == HT_SUB || current == HT_SUP ||
				current == HT_TEXTAREA || current == HT_TT ||
				current == HT_U || current == HT_VAR ||
				current == HT_ZTEXT)
				return(True);

			/* allow these as well if we can relax */
			if(!parser->strict_checking &&
				(current == HT_P || current == HT_DIV))
			{
#ifdef MINIPARSE
				tag_is_wrong_but_allowed = True;
#endif
				/* but always issue a warning */
				parserCallback(parser, current, state, HTML_VIOLATION);
				return(True);
			}
			break;

		case HT_APPLET:
			if(current == HT_A || current == HT_APPLET ||
				current == HT_B || current == HT_BIG ||
				current == HT_BR || current == HT_CITE ||
				current == HT_CODE || current == HT_DFN ||
				current == HT_EM || current == HT_FONT ||
				current == HT_I || current == HT_IMG ||
				current == HT_INPUT || current == HT_KBD ||
				current == HT_MAP || current == HT_NOFRAMES ||
				current == HT_PARAM || current == HT_SAMP ||
				current == HT_SCRIPT || current == HT_SELECT ||
				current == HT_SMALL || current == HT_STRIKE ||
				current == HT_STRONG || current == HT_SUB ||
				current == HT_SUP || current == HT_TEXTAREA ||
				current == HT_TT || current == HT_U ||
				current == HT_VAR || current == HT_ZTEXT)
				return(True);
			break;

		case HT_MAP:
			if(current == HT_AREA)
				return(True);
			break;

		case HT_AREA:		/* only allowed when inside a <MAP> */
			if(state == HT_MAP)
				return(True);
			break;

		/* unterminated tags that may not contain anything */
		case HT_BASE:
		case HT_BR:
		case HT_HR:
		case HT_IMG:
		case HT_INPUT:
		case HT_ISINDEX:
		case HT_LINK:
		case HT_META:
		case HT_PARAM:
			return(True);

		case HT_SCRIPT:
		case HT_STYLE:
			if(current == HT_ZTEXT)
				return(True);
			if(!parser->strict_checking)
			{
#ifdef MINIPARSE
				tag_is_wrong_but_allowed = True;
#endif
				/* allow but issue a warning */
				parserCallback(parser, current, state, HTML_VIOLATION);
				return(True);
			}
			break;

		case HT_OPTION:
		case HT_TEXTAREA:
		case HT_TITLE:
			if(current == HT_ZTEXT)
				return(True);
			break;

		case HT_FRAME:
			if(current == HT_FRAMESET)
				return(True);
			break;
		case HT_SELECT:
			if(current == HT_OPTION)
				return(True);
			break;

		case HT_TABLE:
			if(current == HT_CAPTION ||
				current == HT_TR)
				return(True);
			/* allow these as well if we can relax */
			if(!parser->strict_checking && (IS_CONTAINER(current)))
			{
#ifdef MINIPARSE
				tag_is_wrong_but_allowed = True;
#endif
				/* but always issue a warning */
				parserCallback(parser, current, state, HTML_VIOLATION);
				return(True);
			}
			break;

		case HT_TR:
			if(current == HT_TH ||
				current == HT_TD)
				return(True);
			break;

		case HT_DIR:
		case HT_MENU:
		case HT_OL:
		case HT_UL:
			if(current == HT_LI)
				return(True);
			break;

		case HT_DL:
			if(current == HT_DT ||
				current == HT_DD)
				return(True);
			break;

		case HT_FORM:
			/* nested forms are not allowed */
			if(current == HT_FORM)
				return(False);
			/* allow these as well if we can relax */
			else if(!parser->strict_checking &&
				((IS_CONTAINER(current)) || (OPTIONAL_CLOSURE(current))))
			{
#ifdef MINIPARSE
				tag_is_wrong_but_allowed = True;
#endif
				/* but always issue a warning */
				parserCallback(parser, current, state, HTML_VIOLATION);
				return(True);
			}
			/* fall thru */
		case HT_BLOCKQUOTE:
		case HT_CENTER:
		case HT_DIV:
		case HT_TD:
		case HT_TH:
			if(current == HT_H1 || current == HT_H2 ||
				current == HT_H3 || current == HT_H4 ||
				current == HT_H5 || current == HT_H6 ||
				current == HT_ADDRESS)
				return(True);
			/* fall thru */
		case HT_LI:
		case HT_DD:
			if(current == HT_A || current == HT_APPLET ||
				current == HT_B || current == HT_BIG ||
				current == HT_BLOCKQUOTE || current == HT_BR ||
				current == HT_CENTER || current == HT_CITE ||
				current == HT_CODE || current == HT_DFN ||
				current == HT_DIR || current == HT_DIV ||
				current == HT_DL || current == HT_EM ||
				current == HT_FONT || current == HT_FORM ||
				current == HT_HR || current == HT_I ||
				current == HT_IMG || current == HT_INPUT ||
				current == HT_KBD || current == HT_MAP ||
				current == HT_MENU || current == HT_NOFRAMES ||
				current == HT_OL || current == HT_P ||
				current == HT_PRE || current == HT_SAMP ||
				current == HT_SCRIPT || current == HT_SELECT ||
				current == HT_SMALL || current == HT_STRIKE ||
				current == HT_STRONG || current == HT_SUB ||
				current == HT_SUP || current == HT_TABLE ||
				current == HT_TEXTAREA || current == HT_TT ||
				current == HT_U || current == HT_UL ||
				current == HT_VAR || current == HT_ZTEXT)
				return(True);
			break;

		case HT_PRE:
			if(current == HT_A || current == HT_APPLET ||
				current == HT_B || current == HT_BR ||
				current == HT_CITE || current == HT_CODE ||
				current == HT_DFN || current == HT_EM ||
				current == HT_I || current == HT_INPUT ||
				current == HT_KBD || current == HT_MAP ||
				current == HT_NOFRAMES || current == HT_SAMP ||
				current == HT_SCRIPT || current == HT_SELECT ||
				current == HT_STRIKE || current == HT_STRONG ||
				current == HT_TEXTAREA || current == HT_TT ||
				current == HT_U || current == HT_VAR ||
				current == HT_FONT || current == HT_ZTEXT)
				return(True);
			break;

		case HT_ZTEXT:
			return(True); /* always allowed */

		/* elements of which we don't know any state information */
		case HT_BASEFONT:
			return(True);

		case HT_PAGE:
			return(True);	/* is ignored for now */

		/* no default so we'll get a warning when we miss anything */
	}
	/*****
	* There are a number of semi-container elements that often contain
	* the <P>/<DIV> element. As this isn't a really dangerous element to be
	* floating around randomly, allow it if we haven't been told to
	* be strict.
	* We can't do this for the elements that have an optional terminator
	* as this would mess up the entire parser algorithm.
	*****/
#if 1
	if(!parser->strict_checking && (current == HT_P || current == HT_DIV))
	{
		/* h1 through h6 is handled above */
		if(state == HT_UL || state == HT_OL || state == HT_DL ||
			state == HT_TABLE || state == HT_CAPTION)
		{
#ifdef MINIPARSE
			tag_is_wrong_but_allowed = True;
#endif
			/* but always issue a warning */
			parserCallback(parser, current, state, HTML_VIOLATION);
			return(True);
		}
	}
#endif
#if 0
	if(!parser->strict_checking)
	{
#ifdef MINIPARSE
		tag_is_wrong_but_allowed = True;
#endif
		/* but always issue a warning */
		if(!(OPTIONAL_CLOSURE(current)))
			parserCallback(parser, current, state, HTML_VIOLATION);
		return(True);
	}
#endif
	return(False);
}

/*****
* Name: 		_ParserNewObject
* Return Type: 	XmHTMLObject
* Description: 	allocates a new XmHTMLObject structure
* In:
*	parser:		current parser context
*	id:			id for this element
*	element:	char description for this element
*	attributes:	attributes for this element
*	is_end:		bool indicating whether this element is a closing one
*	terminated:	True when this is element has a terminating counterpart
* Returns:
*	a newly allocated XmHTMLObject. Exits the program if the allocation fails.
*****/
XmHTMLObject*
_ParserNewObject(Parser *parser, htmlEnum id, char *element, char *attributes,
	Boolean is_end, Boolean terminated)
{
	static XmHTMLObject *entry = NULL;

	entry = (XmHTMLObject*)malloc(sizeof(XmHTMLObject));

	entry->id         = id;
	entry->element    = element;
	entry->attributes = attributes;
	entry->is_end     = is_end;
	entry->terminated = terminated;
	entry->line = parser->num_lines;
	entry->next = (XmHTMLObject*)NULL;
	entry->prev = (XmHTMLObject*)NULL;

#ifdef MINIPARSE
	entry->ignore      = False;
	entry->auto_insert = False;
	entry->violation   = tag_is_wrong_but_allowed;
	tag_is_wrong_but_allowed = False;
#endif

	return(entry);
}

/*****
* Name: 		_ParserInsertElement
* Return Type: 	void
* Description: 	creates and inserts a new element in the parser tree
* In:
*	parser:		current parser context
*	element:	element name
*	new_id:		id of element to insert
*	is_end:		False when the new element should open, True when it should
*				close.
* Returns:
*	nothing.
*****/
void
_ParserInsertElement(Parser *parser, String element, htmlEnum new_id,
	Boolean is_end)
{
	XmHTMLObject *extra;
	String tmp;

	/* need to do this, _XmHTMLFreeObjects will free this */
	tmp = strdup(element);

	/* allocate a element */
	extra = _ParserNewObject(parser, new_id, tmp, NULL, is_end, True);

#ifdef MINIPARSE
	extra->auto_insert = True;
#endif

	/* insert this element in the list */
	parser->num_elements++;
	extra->prev = parser->current;
	parser->current->next = extra;
	parser->current = extra;

	_XmHTMLDebug(4, ("parse.c: _ParserInsertElement, added a missing %s %s at "
		"line %i.\n", is_end ? "closing" : "opening", element,
		parser->num_lines));

#ifdef DEBUG
	p_inserted++;
#endif
}

/*****
* Name: 		_ParserTerminateElement
* Return Type: 	Boolean
* Description: 	backtracks in the list of elements to terminate the given
*				element. Used for terminating an unbalanced element.
* In:
*	parser:		current parser context
*	element:	element name
*	current:	current element;
*	expect:		expected element;
* Returns:
*	True when current is allowed, False if not.
*****/
Boolean
_ParserTerminateElement(Parser *parser, String element, htmlEnum current,
	htmlEnum expect)
{
	stateStack *state = parser->state_stack;

	/*
	* If current element is the next one on the stack, insert the expected
	* element, restore the stack and allow the current element.
	*/
	if(state->next != NULL && state->next->id == current)
	{
		String tmp;
		XmHTMLObject *extra;

		/* need to do this, _XmHTMLFreeObjects will free this */
		tmp = strdup(element);

		/* insert expected element */
		extra = _ParserNewObject(parser, expect, tmp, NULL, True, True);

#ifdef MINIPARSE
		extra->auto_insert = True;
#endif

		parser->num_elements++;
		extra->prev = parser->current;
		parser->current->next = extra;
		parser->current = extra;

		/* pop expected element from the stack */
		(void)_ParserPopState(parser);

		_XmHTMLDebug(4, ("parse.c: _ParserTerminateElement, terminated "
			"element %s at line %i.\n", element, parser->num_lines));

#ifdef DEBUG
		p_inserted++;
#endif

		/* current allowed now */
		return(True);
	}
	/* needs a backtrack, postpone until it really becomes invalid */
	return(False);
}

/*****
* Name:			_ParserCreate
* Return Type: 	Parser*
* Description: 	creates a new Parser Context
* In:
*	w:			widget owning this parser
* Returns:
*	A newly created parser
*****/
Parser*
_ParserCreate(Widget w)
{
	Parser *parser = (Parser*)calloc(1, sizeof(Parser));

	/* initialize the stateStack */
	parser->state_stack = &parser->state_base;
	parser->state_stack->id = HT_DOCTYPE;
	parser->state_stack->next = NULL;

	/*
	* Initialize list data. More efficient than every conditional test
	* when an element is to be stored in the list.
	*/
	parser->head = _ParserNewObject(parser, HT_ZTEXT, NULL, NULL, False, False);
	parser->current = parser->head;
	parser->num_elements = 1;

	/* text editors start line numbers at 1*/
	parser->num_lines = 1;

	parser->widget = w;

	/* should we issue warning messages about bad HTML documents? */
#ifndef MINIPARSE
	if(XmIsHTML(w))
	{
		parser->warn = ((XmHTMLWidget)w)->html.bad_html_warnings;
		parser->do_icons = ((XmHTMLWidget)w)->html.icon_entities_enabled;
	}
#else
	if(parser_warnings)
		parser->warn = XmHTML_ALL;
	else
		parser->warn = XmHTML_NONE;
	parser->do_icons = True;
#endif

	return(parser);
}

/*****
* Name:			_ParserDelete
* Return Type: 	void
* Description: 	deletes a Parser context
* In:
*	parser:		parser to be deleted
* Returns:
*	nothing
*****/
void
_ParserDelete(Parser *parser)
{
	/* delete source */
	if(parser->source)
		free(parser->source);

	/* delete list head */
	if(parser->head)
		free(parser->head);

	/* destroy list of objects if present */
	if(parser->current)
		_XmHTMLFreeObjects(parser->current);

	/* and free the allocated parser */
	free(parser);
}

/*****
* Name: 		_ParserInsertTextElement
* Return Type: 	void
* Description: 	allocates and stores a plain text element
* In:
*	parser:		current parser context
*	start:		plain text starting point
*	end:		plain text ending point
* Returns:
*	nothing
*****/
void
_ParserInsertTextElement(Parser *parser, char *start, char *end)
{
	static XmHTMLObject *element = NULL;
	static char *content = NULL;
	/* length of this block */
	int len = end - start;

	/* sanity */
	if(*start == '\0' || len <= 0)
		return;

	content = my_strndup(start, len);

	/*****
	* expansion of character escape sequences is done in format.c,
	* routine CopyText. The reason for this is that we must be able
	* to construct a valid HTML text for a number of the XmHTMLText
	* routines.
	*****/

	element = _ParserNewObject(parser, HT_ZTEXT, content, NULL, False, False);

	/* store this element in the list */
	parser->num_text++;
	element->prev = parser->current;
	parser->current->next = element;
	parser->current = element;
}

/*****
* Name: 		_ParserStoreTextElement
* Return Type: 	void
* Description: 	allocates and stores a plain text element
* In:
*	parser:		current parser context
*	start:		plain text starting point
*	end:		plain text ending point
* Returns:
*	nothing
*****/
void
_ParserStoreTextElement(Parser *parser, char *start, char *end)
{
	/* sanity */
	if(*start == '\0' || (end - start)<= 0)
		return;

	if(parser->do_icons)
	{
		int idx;
		XmHTMLObject *element;
		char *chPtr = start;
		char *startPtr, *endPtr;

		/* scan the entire text in search of escape codes (yuck) */
		startPtr = start;
		endPtr = end;

		while(chPtr && chPtr != end)
		{
			switch(*chPtr)
			{
				/* a possible icon entity */
				case '&':

					/* store current end */
					endPtr = chPtr;

					/* check if it's an icon escape */
					if(isalpha(*(chPtr+1)) &&
						(idx = _ParserTokenToIcon(&chPtr)) != -1)
					{
						/* it is, insert current text */
						_ParserInsertTextElement(parser, startPtr, endPtr);

						/* create a new object */
						element = _ParserNewObject(parser, HT_IMG,
							strdup(html_tokens[HT_IMG]), NULL, False, False);

#ifdef MINIPARSE
						/* auto inserted element */
						element->auto_insert = True;
						element->attributes  = NULL;
						element->attributes = malloc(256);
						sprintf(element->attributes, "src=\"%s\" "
							"icon_index=%i",
							_XmHTMLIconEntities[idx].escape, idx);
#else
						element->attributes =
							_XmHTMLImageGetIconAttribs(parser->widget, idx);
#endif

						/* store the new element in the list */
						parser->num_elements++;
						element->prev = parser->current;
						parser->current->next = element;
						parser->current = element;

						/* set new start & end */
						startPtr = chPtr+1;
						endPtr   = end;
						break;
					}
					else /* skip it, it might be a character escape */
					{
						endPtr = end;
						chPtr++;
					}
					break;
				default:
					chPtr++;
			}
		}
		/* flush remaining text */
		_ParserInsertTextElement(parser, startPtr, endPtr);
	}
	else
		_ParserInsertTextElement(parser, start, end);
}

/*****
* Name: 		_ParserStoreTextElementRtoL
* Return Type: 	void
* Description: 	allocates and stores a plain text element. Inverts
*				contents of this text element as well.
* In:
*	parser:		current parser context
*	start:		plain text starting point
*	end:		plain text ending point
* Returns:
*	nothing
*****/
void
_ParserStoreTextElementRtoL(Parser *parser, char *start, char *end)
{
	static XmHTMLObject *element = NULL;
	static char *content = NULL;
	register char *inPtr, *outPtr;

	/* length of this block */
	int len = end - start;

	/* sanity */
	if(*start == '\0' || len <= 0)
		return;

	content = (char*)malloc(len+1);	/* +1 for terminating \0 */

	/* copy text, reversing contents as we do */
	inPtr = start;
	outPtr = &content[len-1];
	while(1)
	{
		switch(*inPtr)
		{
			case '&':
				/* we don't touch escape sequences */
				{
					register char *ptr;

					/* set start position */
					ptr = inPtr;

					/* get end */
					while(ptr < end && *ptr != ';')
						ptr++;

					/* might not be a valid escape sequence */
					if(ptr == end)
						break;

					/* insertion position */
					outPtr -= (ptr - inPtr);

					/* copy literally */
					memcpy(outPtr, inPtr, (ptr+1) - inPtr);

					/* new start position */
					inPtr = ptr;
				}
				break;
			/*****
			* All bi-directional characters need to be reversed if we want
			* them to keep their intended behaviour.
			*****/
			case '`':
				*outPtr = '\'';
				break;
			case '\'':
				*outPtr = '`';
				break;
			case '<':
				*outPtr = '>';
				break;
			case '>':
				*outPtr = '<';
				break;
			case '\\':
				*outPtr = '/';
				break;
			case '/':
				*outPtr = '\\';
				break;
			case '(':
				*outPtr = ')';
				break;
			case ')':
				*outPtr = '(';
				break;
			case '[':
				*outPtr = ']';
				break;
			case ']':
				*outPtr = '[';
				break;
			case '{':
				*outPtr = '}';
				break;
			case '}':
				*outPtr = '{';
				break;
			default:
				*outPtr = *inPtr;
				break;
		}
		inPtr++;
		outPtr--;
		if(inPtr == end)
			break;
	}
	content[len] = '\0';	/* NULL terminate */

	/*
	* expansion of character escape sequences is done in format.c,
	* routine CopyText. The reason for this is that we must be able
	* to construct a valid HTML text for a number of the XmHTMLText
	* routines.
	*/

	element = _ParserNewObject(parser, HT_ZTEXT, content, NULL, False, False);

	/* store this element in the list */
	parser->num_text++;
	element->prev = parser->current;
	parser->current->next = element;
	parser->current = element;
}

/*****
* Name: 		_ParserCopyElement
* Return Type: 	void
* Description: 	copies and inserts the given object
* In:
*	parser:		current parser context
*	src:		object to copy
*	is_end:		terminator state
* Returns:
*	nothing
*****/
void
_ParserCopyElement(Parser *parser, XmHTMLObject *src, Boolean is_end)
{
	static XmHTMLObject *copy;
	int len;

	/* sanity */
	if(src == NULL)
		return;

	copy = (XmHTMLObject*)malloc(sizeof(XmHTMLObject));

	copy->id   = src->id;
	copy->is_end = is_end;
	copy->terminated = src->terminated;
	copy->line = parser->num_lines;
	copy->next = (XmHTMLObject*)NULL;
	copy->attributes = NULL;

#ifdef MINIPARSE
	copy->ignore      = src->ignore;
	copy->auto_insert = src->auto_insert;
#endif

	/* allocate element data */
	len = strlen(src->element)+(src->attributes ? strlen(src->attributes) : 1);
	copy->element = (char*)malloc((len+2)*sizeof(char));

	/* copy element data */
	len = strlen(src->element);
	strcpy(copy->element, src->element);
	copy->element[len] = '\0';

	/* copy possible attributes */
	if(src->attributes)
	{
		strcpy(&copy->element[len+1], src->attributes);
		copy->attributes = &copy->element[len+1];
	}

	parser->num_elements++;
	/* attach prev and next ptrs to the appropriate places */
	copy->prev = parser->current;
	parser->current->next = copy;
	parser->current = copy;
}

/*****
* Name: 		parserWarning
* Return Type: 	int
* Description: 	gives out warning messages depending on the type of error.
* In:
*	parser:		current parser context
*	id:			offending id
*	current:	current parser state
*	error:		type of error
* Returns:
*	nothing.
*****/
static void
parserWarning(Parser *parser, htmlEnum id, htmlEnum current, parserError error)
{
	static char msg[256];

	/* update error count before doing anything else */
	if(error != HTML_UNKNOWN_ELEMENT)
#ifdef MINIPARSE
		parser_errors++;
#else
		parser->err_count++;
#endif

	/*
	* make appropriate error message, set bad_html flag and update error
	* count when error indicates a markup error or HTML violation.
	*/
	switch(error)
	{
		case HTML_UNKNOWN_ELEMENT:
			{
				int len;
				if(!(parser->warn & XmHTML_UNKNOWN_ELEMENT))
					return;
				msg[0] = '\0';		/* nullify */
				sprintf(msg, "%s <", XMHTML_MSG_122);
				len = parser->cend - parser->cstart;
				if(len > 127)
					len = 127;
				strncat(msg, &parser->source[parser->cstart], len);
				strcat(msg, ">.");
			}
			break;
		case HTML_OPEN_ELEMENT:
			parser->html32 = False;
			if(!(parser->warn & XmHTML_OPEN_ELEMENT))
				return;
			sprintf(msg, XMHTML_MSG_123, html_tokens[id], html_tokens[current]);
			break;
		case HTML_BAD:
			parser->html32 = False;
			if(!(parser->warn & XmHTML_BAD))
				return;
			sprintf(msg, XMHTML_MSG_124, html_tokens[id]);
			break;
		case HTML_OPEN_BLOCK:
			parser->html32 = False;
			if(!(parser->warn & XmHTML_OPEN_BLOCK))
				return;
			sprintf(msg, XMHTML_MSG_125, html_tokens[id], html_tokens[current]);
			break;
		case HTML_CLOSE_BLOCK:
			parser->html32 = False;
			if(!(parser->warn & XmHTML_CLOSE_BLOCK))
				return;
			sprintf(msg, XMHTML_MSG_126, html_tokens[id]);
			break;
		case HTML_NESTED:
			parser->html32 = False;
			if(!(parser->warn & XmHTML_NESTED))
				return;
			sprintf(msg, XMHTML_MSG_127, html_tokens[id]);
			break;
		case HTML_VIOLATION:
			parser->html32 = False;
			if(!(parser->warn & XmHTML_VIOLATION))
				return;
			sprintf(msg, XMHTML_MSG_128, html_tokens[id], html_tokens[current]);
			break;
		case HTML_INTERNAL:
			sprintf(msg, XMHTML_MSG_129,"");
			break;
		case HTML_NOTIFY:	/* not reached */
			return;

		/* no default */
	}
	strcat(msg, "\n    ");
	strcat(msg, XMHTML_MSG_130);

	_XmHTMLWarning(__WFUNC__(parser->widget, "_ParserVerify"),
		msg, parser->num_lines);
}

/*****
* Name: 		_ParserVerify
* Return Type: 	int
* Description: 	element verifier, the real funny part.
* In:
*	parser:		current parser context
*	id:			element to verify
*	is_end:		element terminating state
* Returns:
*	-1 when element should be removed, 0 when the element is not terminated
*	and 1 when it is.
*	This routine tries to do a huge amount of damage control by a number
*	of checks (and is a real mess).
* Note:
*	This routine is becoming increasingly complex, especially with possible
*	iteration over all current parser states to find a proper insertion point
*	when the new element is out of place, the checks on contents of the current
*	element and appearance of the new element and the difference between
*	opening and closing elements.
*	This routine is far too complex to explain, I can hardly even grasp it
*	myself. If you really want to know what is happening here, read thru it
*	and keep in mind that _ParserCheckElementOccurance and
*	_ParserCheckElementContent behave *very* differently from each other.
*****/
int
_ParserVerify(Parser *parser, htmlEnum id, Boolean is_end)
{
	/* current parser state */
	htmlEnum curr = parser->state_stack->id;
	int iter = 0, new_id;

	/* ending elements are automatically terminated */
	if(is_end || _ParserIsElementTerminated(id))
	{
		if(!is_end)
		{
			/*
			* First check: if the new element matches the current state,
			* we first need to terminate the previous element (remember the
			* new element is a starting one). Don't do this for nested
			* elements since that has the potential of seriously messing
			* things up.
			*/
			if(id == curr && !(NESTED_ELEMENT(id)))
			{
				/* invalid nesting if this is not an optional closure */
				if(!(OPTIONAL_CLOSURE(curr)))
				{
					parserCallback(parser, id, curr, HTML_NESTED);
					return(-1);
				}

				/* default is to terminate current state */
				_ParserInsertElement(parser, html_tokens[curr], curr, True);
				/* new element matches current, so it stays on the stack */
				return(1);
			}
			/*
			* Second check: see if the new element is allowed to occur
			* inside the current element.
			*/
			new_id = _ParserCheckElementOccurance(parser, id, curr);
			if(new_id != HT_ZTEXT && new_id != -1)
			{
				parserCallback(parser, id, curr, HTML_VIOLATION);
				_ParserInsertElement(parser, html_tokens[new_id],
					(htmlEnum)new_id, new_id == curr);
				/*
				* If the new element terminates it's opening counterpart,
				* pop it from the stack. Otherwise it adds a new parser state.
				*/
				if(new_id == curr)
					(void)_ParserPopState(parser);
				else
					_ParserPushState(parser, (htmlEnum)new_id);
				/* new element is now allowed, push it */
				_ParserPushState(parser, id);
				return(1);
			}
			/*
			* not allowed, see if the content matches. If not, terminate
			* the previous element and proceed as if nothing ever happened.
			*/
recheck:
			/* damage control */
			if(iter > 4 || (parser->state_stack->next == NULL && iter))
			{
				/* stack restoration */
				if(parser->state_stack->id == HT_DOCTYPE)
					_ParserPushState(parser, HT_HTML);
				if(parser->state_stack->id == HT_HTML)
					_ParserPushState(parser, HT_BODY);

				/* HTML_BAD, default is to remove it */
				parserCallback(parser, id, curr, HTML_BAD);
				return(-1);
			}
			iter++;
			/*
			* Third check: see if the new element is allowed as content
			* of the current element. This check will iterate until it
			* finds a matching parser state or until the parser runs out
			* of states.
			*/
			if(!_ParserCheckElementContent(parser, id, curr))
			{
				/*
				* HTML_OPEN_BLOCK, default is to insert current
				* spit out a warning if it's really missing
				*/
				if(!(OPTIONAL_CLOSURE(curr)))
					parserCallback(parser, id, curr, HTML_OPEN_BLOCK);

				/* terminate current element before adding the new one*/
				if(id == curr ||
					(curr != HT_DOCTYPE && curr != HT_HTML &&
					 curr != HT_BODY))
					_ParserInsertElement(parser, html_tokens[curr], curr, True);
				(void)_ParserPopState(parser);
				curr = parser->state_stack->id;
				goto recheck;
			}
			else if(!is_end && !(_ParserCheckElementContent(parser, id, curr)))
			{
				parserCallback(parser, id, curr, HTML_VIOLATION);
				return(-1);
			}
			/* element allowed, push it */
			_ParserPushState(parser, id);
			return(1);
		}
		else
		{
			/* First check: see if this element has a terminating counterpart */
			if(!_ParserIsElementTerminated(id))
			{
				/*
				* We do not known terminated elements that can't be
				* terminated (*very* stupid HTML).
				*/
				parserWarning(parser, curr, curr, HTML_UNKNOWN_ELEMENT);
				return(-1);	/* obliterate it */
			}
			/*
			* Second check: see if the counterpart of this terminating element
			* is on the stack. If it isn't, we probably terminated it
			* ourselves to keep the document properly balanced and we don't
			* want to insert this terminator as it probably will change the
			* document substantially (a perfect example is
			* <p><form></p>..</form>, which without this check would be changed
			* to <p></p><form></form><p></p>... instead of
			* <p></p><form>...</form>.
			*/
			if(!_ParserOnStack(parser, id))
			{
				parserWarning(parser, id, curr, HTML_CLOSE_BLOCK);
				return(-1);
			}

			/* element ends, check balance. */
reterminate:
			/* damage control */
			if(iter > 4 || (parser->state_stack->next == NULL && iter))
			{
				/* stack restoration */
				if(parser->state_stack->id == HT_DOCTYPE)
					_ParserPushState(parser, HT_HTML);
				if(parser->state_stack->id == HT_HTML)
					_ParserPushState(parser, HT_BODY);

				/* HTML_BAD, default is to remove it */
				parserCallback(parser, id, curr, HTML_BAD);
				return(-1);
			}
			iter++;
			if(id != curr)
			{
				/*
				* This check and the next are real ugly: this one checks
				* whether the current parser state is still valid if the
				* new terminator is inserted, while the next one checks whether
				* the new terminator may appear in the current parser state.
				* This is becoming increasingly complex :-(
				*/
				if((_ParserCheckElementOccurance(parser, id, curr))
					!= HT_ZTEXT)
				{
					/* remove if it's not an optional closing element */
					if(!(OPTIONAL_CLOSURE(curr)))
					{
						/*
						* if id is present on stack we have an unbalanced
						* terminator
						*/
						if(_ParserOnStack(parser, id))
							goto unbalanced;

						parserCallback(parser, id, curr, HTML_CLOSE_BLOCK);
						/* HTML_CLOSE_BLOCK, default is to remove it */
						return(-1);
					}
					/* terminate current before adding the new one */
#if 0
					/* seems to be a bit useless... */
					if(id == curr ||)
#endif
					if(
						(curr != HT_DOCTYPE && curr != HT_HTML &&
						 curr != HT_BODY))
						_ParserInsertElement(parser, html_tokens[curr], curr,
						True);
					curr = _ParserPopState(parser);
					if(curr != id)
					{
						curr = parser->state_stack->id;
						goto reterminate;
					}
				}
				else if((new_id = _ParserCheckElementOccurance(parser,
							curr, id)) != -1)
				{
					/*****
					* Check whether the proposed element is still valid
					* when inserted in the current element (might seem silly,
					* but it's a valid check for nested constructs).
					*****/
					if(new_id == HT_ZTEXT ||
						(_ParserCheckElementOccurance(parser, new_id, id)) ==
							HT_ZTEXT)
						new_id = curr;

					/* remove if it's not an optional closing element */
					if(!(OPTIONAL_CLOSURE(curr)))
					{
						/*
						* if id is present on stack we have an unbalanced
						* terminator
						*/
						if(_ParserOnStack(parser, id))
							goto unbalanced;

						/* HTML_CLOSE_BLOCK, default is to remove it */
						parserCallback(parser, id, curr, HTML_CLOSE_BLOCK);
						return(-1);
					}
					/* terminate current before adding the new one */
					if(id == curr ||
						(curr != HT_DOCTYPE && curr != HT_HTML &&
						 curr != HT_BODY))
						_ParserInsertElement(parser, html_tokens[new_id],
							new_id, True);
					curr = _ParserPopState(parser);
					if(curr != id)
					{
						curr = parser->state_stack->id;
						goto reterminate;
					}
				}
				else
				{
unbalanced:
					/* switch if it's not an optional closing element */
					if(!(OPTIONAL_CLOSURE(curr)))
					{
						/*****
						* This is something like:
						* <a href=..>...<b>...</a>...</b>
						* current = HT_B;
						* id = HT_A;
						* expected = HT_B;
						* repair action: insert the expected element if it's
						* the next one on the stack, else forget it, it will
						* be inserted automatically when the stack reaches
						* the required depth.
						*****/
						parserCallback(parser, id, curr, HTML_OPEN_ELEMENT);
						if(!_ParserTerminateElement(parser,
							html_tokens[curr], id, curr))
							return(-1);
					}
					/*
					* Current state is an optional closure and the new
					* element causes it to be inserted. Make it so and
					* redo the entire process. This will emit all optional
					* closures that prevent the new element from becoming
					* legal and will balance the stack.
					* Sigh. The horror of HTML...
					*/
					/* fix 08/04/97-01, kdh */
					else
					{
						if(id == curr ||
							(curr != HT_DOCTYPE && curr != HT_HTML &&
							 curr != HT_BODY))
							_ParserInsertElement(parser, html_tokens[curr],
								curr, True);
						curr = _ParserPopState(parser);
						if(curr != id)
						{
							curr = parser->state_stack->id;
							goto reterminate;
						}
					}
				}
			}
			/* resync */
			if(id == parser->state_stack->id)
				(void)_ParserPopState(parser);
		}
		return(1);	/* element allowed */
	}
	else
	{
		/* see if the new element is allowed as content of current element. */
		if((new_id = _ParserCheckElementOccurance(parser, id, curr))
			!= HT_ZTEXT)
		{
			/* maybe terminate the current parser state? */
			if(new_id == -1)
				new_id = curr;

			parserCallback(parser, id, curr, HTML_VIOLATION);

			/* HTML_VIOLATION, default is to insert since new_id is valid */
			_ParserInsertElement(parser, html_tokens[new_id], (htmlEnum)new_id,
				new_id == curr);
			if(new_id == curr)
				(void)_ParserPopState(parser);
			else
				_ParserPushState(parser, (htmlEnum)new_id);
		}
		return(0);
	}
	return(0);	/* not reached */
}

/*****
* Name: 		_ParserVerifyVerification
* Return Type: 	XmHTMLObject*
* Description: 	checks whether the document verification/repair routines did
*				a proper job.
* In:
*	objects:	tree of parsed objects
* Returns:
*	NULL when all parser states are balanced, offending object otherwise.
*****/
XmHTMLObject *
_ParserVerifyVerification(XmHTMLObject *objects)
{
	XmHTMLObject *tmp = objects;
	Parser parser;
	htmlEnum curr;

#ifdef MINIPARSE
	struct timeval ts, te;
	int secs, usecs;
	if(parser_verification_timings)
	{
		fprintf(stderr, "Verifying parser output: parser ");
		gettimeofday(&ts, NULL);
	}

	id_depth = 0;
#endif

	/* walk to the first terminated item in the list */
	while(tmp != NULL && !tmp->terminated)
		tmp = tmp->next;

	memset(&parser, 0, sizeof(Parser));

	/* reset state stack */
	parser.state_stack = &parser.state_base;
	parser.state_stack->id = curr = tmp->id;
	parser.state_stack->next = NULL;

	tmp = tmp->next;

	for(; tmp != NULL; tmp = tmp->next)
	{
		if(tmp->terminated)
		{
			if(tmp->is_end)
			{
				if(curr != tmp->id)
					break;
				curr = _ParserPopState(&parser);
			}
			else
			{
				_ParserPushState(&parser, curr);
				curr = tmp->id;
			}
		}
	}
	/* clear the stack */
	while(parser.state_stack->next != NULL)
		curr = _ParserPopState(&parser);

#ifdef MINIPARSE
	fprintf(stderr, "%s\n", tmp == NULL ? "did OK" : "Failed");
	if(tmp)
	{
		_XmHTMLWarning(__WFUNC__(NULL, "_ParserVerifyVerification"),
			XMHTML_MSG_91, tmp->is_end ? "closing" : "opening",
			html_tokens[tmp->id], tmp->line);
	}
	if(parser_verification_timings)
	{
		gettimeofday(&te, NULL);
		secs = (int)(te.tv_sec - ts.tv_sec);
		usecs = (int)(te.tv_usec - ts.tv_usec);
		if(usecs < 0) usecs *= -1;
		fprintf(stderr, "verifyVerification done in %i.%i seconds\n",
			secs, usecs);
	}
#endif

	return(tmp);
}

/*****
* Name: 		_ParserStoreElement
* Return Type: 	String
* Description: 	allocates and stores a HTML element
* In:
*	parser:		current parser context
*	start:		element starting point
*	end:		element ending point
* Returns:
*	Updated char position if we had to skip <SCRIPT> or <STYLE> data.
*****/
String
_ParserStoreElement(Parser *parser, char *start, char *end)
{
	register char *chPtr, *elePtr;
	char *startPtr, *endPtr;
	Boolean is_end = False;
	static XmHTMLObject *element;
	static char *content;
	htmlEnum id;
	int terminated;
#ifdef MINIPARSE
	Boolean ignore = False;
#endif

	if(end == NULL || *end == '\0')
		return(end);

	/* absolute ending position for this element */
	parser->cend = parser->cstart + (end - start);

	/*****
	* If this is true, we have an empty tag or an empty closing tag.
	* action to take depends on what type of empty tag it is.
	*****/
	if(start[0] == '>' || (start[0] == '/' && start[1] == '>'))
	{
		/*****
		* if start[0] == '>', we have an empty tag, otherwise we have an empty
		* closing tag. In the first case, we walk backwards until we reach the
		* very first tag.
		* An empty tag simply means: copy the previous tag, nomatter what
		* content it may have. In the second case, we need to pick up the
		* last recorded opening tag and copy it.
		*****/
		if(start[0] == '>')
		{
			/*****
			* Walk backwards until we reach the first non-text element.
			* Elements with an optional terminator which are not terminated
			* are updated as well.
			*****/
			for(element = parser->current ; element != NULL;
				element = element->prev)
			{
				if(OPTIONAL_CLOSURE(element->id) && !element->is_end &&
					element->id == parser->state_stack->id)
				{
					_ParserInsertElement(parser, element->element,
						element->id, True);
					(void)_ParserPopState(parser);
					break;
				}
				else if(element->id != HT_ZTEXT)
					break;
			}
			_XmHTMLDebug(4, ("parse.c: _ParserStoreElement, empty tag on "
				"line %i, inserting %s\n", parser->num_lines,
				element->element));
			_ParserCopyElement(parser, element, False);
			if(element->terminated)
				_ParserPushState(parser, element->id);
		}
		else
		{
			for(element = parser->current; element != NULL;
				element = element->prev)
			{
				if(element->terminated)
				{
					if(OPTIONAL_CLOSURE(element->id))
					{
						/* add possible terminators for these elements */
						if(!element->is_end &&
							element->id == parser->state_stack->id)
						{
							_ParserInsertElement(parser, element->element,
								element->id, True);
							(void)_ParserPopState(parser);
						}
					}
					else
						break;
				}
			}

			_XmHTMLDebug(4, ("parse.c: _ParserStoreElement, empty closing tag "
				"on line %i, inserting %s\n", parser->num_lines,
				element->element));
			_ParserCopyElement(parser, element, True);
			(void)_ParserPopState(parser);
		}
		return(end);
	}

	startPtr = start;
	/* Check if we have any unclosed tags */
	if((endPtr = strstr(start, "<")) == NULL)
		endPtr = end;
	/* check if we stay within bounds */
	else if(endPtr - end > 0)
		endPtr = end;

	while(1)
	{
		is_end = False;

		/*****
		* First skip past spaces and a possible opening /. The endPtr test
		* is mandatory or else we would walk the entire text over and over
		* again every time this routine is called.
		*****/
		for(elePtr = startPtr; *elePtr != '\0' && elePtr != endPtr; elePtr++)
		{
			if(*elePtr == '/')
			{
				is_end = True;
				elePtr++;
				break;
			}
			if(!(isspace(*elePtr)))
				break;
		}
		/* usefull sanity */
		if(endPtr - elePtr < 1)
			break;

		/* allocate and copy element */
		content = my_strndup(elePtr, endPtr - elePtr);

		chPtr = elePtr = content;

		/*****
		* Move past the text to get any element attributes. The ! will let
		* us pick up the !DOCTYPE definition.
		* Don't put the chPtr++ inside the tolower, chances are that it will
		* be evaluated multiple times if tolower is a macro.
		* From: Danny Backx <u27113@kb.be>
		*****/
		if(*chPtr == '!')
			chPtr++;
		while(*chPtr && !(isspace(*chPtr)))		/* fix 01/17/97-01; kdh */
		{
			*chPtr = _FastLower(*chPtr);
			chPtr++;
		}

		/*****
		* attributes are only allowed in opening elements
		* This is a neat hack: to reduce allocations, we do *not* copy the
		* element name into it's own buffer. Instead we use the one allocated
		* above, and place a \0 in the space right after the HTML element.
		* If this element has attributes, we set the attribute pointer (=chPtr)
		* to point right after this \0.
		* This also has the advantage that no reallocation or string copying
		* is required.
		* Freeing the memory allocated can be done in one call on the element
		* field of the XmHTMLObject struct.
		*****/
		if(!is_end)
		{
			if(*chPtr && *(chPtr+1))
			{
				register String ptr;

				content[chPtr-elePtr] = '\0';
				chPtr = content + strlen(content) + 1;

				/*****
				* speedup later on: check if we have any non-whitespace
				* chars. If we have, we have an attribute. Else we don't
				* in which case we set chPtr to NULL. Prevents unnecessary
				* calls to any of the TagGet functions in the formatter.
				*****/
				ptr = chPtr;
				while('\0' != *ptr && isspace(*ptr))
					ptr++;
				if('\0' == *ptr)
					chPtr = NULL;
				else
				{
					/*****
					* Make attributes lowercase so we can do caseless
					* string compare when retrieving attribute values
					* later on.
					*****/
					ptr = chPtr;
					while('\0' != *ptr)
					{
						/*****
						* Attributes which have a value always contain
						* an equal sign. So the original delimiter is
						* the equal sign. When we encounter one, we stop
						* lowering chars and move to either the next
						* word -or- the next quote.
						*****/
						if('=' == *ptr)
						{
							/* skip whitespace until first non-whitespace */
							ptr++;
							while('\0' != *ptr && isspace(*ptr))
								ptr++;
							/*
							* if next char is a quote, move to end of
							* quote, else move to end of word.
							*/
							if('"' == *ptr)
							{
								ptr++;
								while('\0' != *ptr && '"' != *ptr)
									ptr++;
							}
							else
								while('\0' != *ptr && !isspace(*ptr))
									ptr++;

						}
						else
						{
							*ptr = _FastLower(*ptr);
							ptr++;
						}
					}
				}
			}
			else	/* no trailing attributes for this element */
				if(*chPtr)		/* fix 01/17/97-01; kdh */
					content[chPtr-elePtr] = '\0';
				else
					chPtr = NULL;
		}
		else	/* closing element, can't have any attributes */
		{
			/*****
			* only need element *name*, so be sure to null out anything
			* that comes right after it (such as spaces and stuff).
			* fix 02/12/98-01, kdh.
			*****/
			if(*chPtr)
				content[chPtr-elePtr] = '\0';
			chPtr = NULL;
		}

		/* Ignore elements we do not know */
		if((id = _ParserTokenToId(parser, elePtr, parser->warn)) != -1)
		{
			/*****
			* Check if this element belongs to body. This test is as best
			* as it can get (we do not scan raw text for non-whitespace chars)
			* but will omit any text appearing *before* the <body> tag is
			* inserted.
			*****/
			if(!parser->have_body)
			{
				if(id == HT_BODY)
					parser->have_body = True;
				else if(id == HT_PAGE)
					parser->have_page = True;
				else if(_ParserIsBodyElement(id))
				{
					_ParserInsertElement(parser, "body", HT_BODY, False);
					_ParserPushState(parser, HT_BODY);
					parser->have_body = True;
				}
			}

			/* Go and verify presence of the new element. */
			terminated = _ParserVerify(parser, id, is_end);

			if(terminated == -1)
			{
				parser->html32 = False;	/* not HTML32 conforming */
#ifdef DEBUG
				p_removed++;
#endif
#ifdef MINIPARSE
				ignore = True;
#else
				free(content);
				/* remove contents of badly placed SCRIPT & STYLE elements */
				if((id == HT_SCRIPT || id == HT_STYLE) && !is_end)
					goto removeData;
				return(end);
#endif
			}
#ifdef MINIPARSE
			else
				ignore = False;
#endif

			/* insert the current element */
			element = _ParserNewObject(parser, id, elePtr, chPtr, is_end,
				(Boolean)terminated);
			/* attach prev and next ptrs to the appropriate places */
			parser->num_elements++;
			element->prev = parser->current;
			parser->current->next = element;
			parser->current = element;

#ifdef MINIPARSE
			element->ignore = ignore;
#endif

			/*****
			* The SCRIPT & STYLE elements are a real pain in the ass to deal
			* with properly: they are text with whatever in it,
			* and it's terminated by a closing element. It would be
			* *very* easy if the tags content are enclosed within a
			* comment, but since this is *NOT* required by the HTML 3.2 spec,
			* we need to check it in here...
			*****/
#ifndef MINIPARSE
removeData:
#endif
			if((id == HT_SCRIPT || id == HT_STYLE) && !is_end)
			{
				int done = 0;
				char *start = end;
				register int text_len = 0;

				/* move past closing > */
				start++;
				while(*end != '\0' && done == 0)
				{
					switch(*end)
					{
						case '<':
							/* catch comments */
							if(*(end+1) == '/')
							{
								if(!(strncasecmp(end+1, "/script", 7)))
									done = 1;
								else if(!(strncasecmp(end+1, "/style", 6)))
									done = 2;
							}
							break;
						case '\n':
							parser->num_lines++;
							/* fall through */
						default:
							text_len++;
							break;
					}
					if(*end == '\0')
						break;
					end++;
				}
				if(done)
				{
					/*****
					* Only store contents if this tag was in place. This
					* check is required as this piece of code is also used
					* to remove the tags content when the tag is out of it's
					* proper place (not inside the <head> section).
					* We must always do this when we are compiled as standalone
					* parser.
					* Parser will advance by one byte upon return, so we
					* need to decrement by two bytes
					*****/
					/* store script contents */
					end--;
					_ParserStoreTextElement(parser, start, end-1);
					end--;
				}
				else
					/* restore original end position */
					end = start-1;

#ifdef DEBUG
				/* closing tag is also removed */
				if(terminated == -1)
					p_removed++;
#endif
				/* no check for unclosed tags here, just return */
				return(end);
			}
		}
		else
		{
			/* ignore */
			free(content);	/* fix 01/28/97-01, kdh */
		}

		/* check if we have any unclosed tags remaining. */
		if(endPtr - end < 0)
		{
			endPtr++;
			startPtr = endPtr;
			/* Check if we have any unclosed tags */
			if((endPtr = strstr(startPtr, "<")) == NULL)
				endPtr = end;

			/* check if we stay within bounds */
			else if(endPtr - end > 0)
					endPtr = end;
		}
		else
			break;
	}
	return(end);
}

#ifndef MINIPARSE
/*****
* Name: 		storeElementUnconditional
* Return Type: 	String
* Description: 	allocates and stores a HTML element *without* verifying it.
* In:
*	parser:		current parser context
*	start:		element starting point
*	end:		element ending point
* Returns:
*	Updated char position if we had to skip <SCRIPT> or <STYLE> data.
*****/
static String
storeElementUnconditional(Parser *parser, char *start, char *end)
{
	register char *chPtr, *elePtr;
	char *startPtr, *endPtr;
	Boolean is_end = False;
	static XmHTMLObject *element;
	static char *content;
	htmlEnum id;
	Boolean terminated;

	if(end == NULL || *end == '\0')
		return(end);

	/* absolute ending position for this element */
	parser->cend = parser->cstart + (end - start);

	/* no null end tags here */

	startPtr = start;
	/* Check if we have any unclosed tags */
	if((endPtr = strstr(start, "<")) == NULL)
		endPtr = end;
	/* check if we stay within bounds */
	else if(endPtr - end > 0)
		endPtr = end;

	is_end = False;

	/*****
	* First skip past spaces and a possible opening /. The endPtr test
	* is mandatory or else we would walk the entire text over and over
	* again every time this routine is called.
	*****/
	for(elePtr = startPtr; *elePtr != '\0' && elePtr != endPtr; elePtr++)
	{
		if(*elePtr == '/')
		{
			is_end = True;
			elePtr++;
			break;
		}
		if(!(isspace(*elePtr)))
			break;
	}
	/* usefull sanity */
	if(endPtr - elePtr < 1)
		return(end);

	/* allocate and copy element */
	content = my_strndup(elePtr, endPtr - elePtr);

	chPtr = elePtr = content;

	/* Move past the text to get any element attributes. */
	if(*chPtr == '!')
		chPtr++;
	while(*chPtr && !(isspace(*chPtr)))
	{
		*chPtr = _FastLower(*chPtr);
		chPtr++;
	}

	/* attributes are only allowed in opening elements */
	if(!is_end)
	{
		if(*chPtr && *(chPtr+1))
		{
			register String ptr;
			content[chPtr-elePtr] = '\0';
			ptr = chPtr = content + strlen(content) + 1;
			/* check for attributes */
			while(*ptr != '\0' && isspace(*ptr))
				ptr++;
			if(*ptr == '\0')
				chPtr = NULL;
			else	/* make attributes lowercase */
				my_locase(chPtr);
		}
		else	/* no trailing attributes for this element */
			if(*chPtr)
				content[chPtr-elePtr] = '\0';
			else
				chPtr = NULL;
	}
	else	/* closing element, can't have any attributes */
		chPtr = NULL;

	/* Ignore elements we do not know */
	if((id = _ParserTokenToId(parser, elePtr, parser->warn)) != -1)
	{
		/* see if this element has a closing counterpart */
		terminated = _ParserIsElementTerminated(id);

		/* insert the current element */
		element = _ParserNewObject(parser, id, elePtr, chPtr, is_end,
					terminated);

		/* attach prev and next ptrs to the appropriate places */
		parser->num_elements++;
		element->prev = parser->current;
		parser->current->next = element;
		parser->current = element;

		/* get contents of the SCRIPT & STYLE elements */
		if((id == HT_SCRIPT || id == HT_STYLE) && !is_end)
		{
			int done = 0;
			char *start = end;
			register int text_len = 0;

			/* move past closing > */
			start++;
			while(*end != '\0' && done == 0)
			{
				switch(*end)
				{
					case '<':
						/* catch comments */
						if(*(end+1) == '/')
						{
							if(!(strncasecmp(end+1, "/script", 7)))
								done = 1;
							else if(!(strncasecmp(end+1, "/style", 6)))
								done = 2;
						}
						break;
					case '\n':
						parser->num_lines++;
						/* fall through */
					default:
						text_len++;
						break;
				}
				if(*end == '\0')
					break;
				end++;
			}
			if(done)
			{
				/* store script contents */
				end--;
				_ParserStoreTextElement(parser, start, end-1);
				end--;
			}
			else
				/* restore original end position */
				end = start-1;

			/* no check for unclosed tags here, just return */
			return(end);
		}
	}
	else /* ignore */
		free(content);

	return(end);
}
#endif /* MINIPARSE */

/*****
* Name:			ParserWriteOutputToFile/writeParsedOutputToFile
* Return Type: 	void
* Description: 	writes a parser tree to file
* In:
*	objects:	parser tree to be outputted;
*	prefix:		file prefix
*	notext:		if True, don't write plain text (ParserWriteOutput only)
* Returns:
*	nothing
*****/
#if defined(MINIPARSE) || defined(DEBUG)
#ifdef MINIPARSE
void
ParserWriteOutputToFile(XmHTMLObject *objects, String prefix, Boolean notext)
#else
static void
writeParsedOutputToFile(XmHTMLObject *objects, String prefix)
#endif
{
	XmHTMLObject *tmp;
	char name[1024];
	FILE *file;
	int i, tablevel = 0;
	static int count = 0;

	/*
	* No buffer overrun check here. If this sigsegv's, its your own fault.
	* Don't use names longer than 1024 bytes then.
	*/
	sprintf(name, "%s.%i", prefix, count);
	count++;

	if((file = fopen(name, "w")) == NULL)
	{
		perror(name);
		return;
	}

	for(tmp = objects; tmp != NULL; tmp = tmp->next)
	{
		if(tmp->id != HT_ZTEXT)
		{
			fprintf(file, "%i:", tmp->line);
			if(tmp->is_end)
			{
#ifdef MINIPARSE
				if(!tmp->ignore)
#endif
					tablevel--;
				if(tablevel < 0) tablevel++;
				for(i = 0; i != tablevel; i++)
					fputs("\t", file);
				fprintf(file, "</%s", html_tokens[tmp->id]);
			}
			else
			{
				for(i = 0; i != tablevel; i++)
					fputs("\t", file);
				fprintf(file, "<%s", html_tokens[tmp->id]);
#ifdef MINIPARSE
				if(tmp->terminated && !tmp->ignore)
#else
				if(tmp->terminated)
#endif
					tablevel++;
			}

			if(tmp->attributes)
				fprintf(file, " %s", tmp->attributes);

			fputs(">", file);

#ifdef MINIPARSE
			if(tmp->auto_insert == 1)
				fputs(" [AUTO-INSERTED]", file);
			else if(tmp->auto_insert == 2)
				fputs(" [CHECK-AUTO-INSERTED]", file);
			if(tmp->ignore)
				fputs(" [IGNORED]", file);
			if(tmp->violation)
				fputs(" [VIOLATION]", file);
#endif

			fputs("\n", file);
		}
		else
#ifdef MINIPARSE
			if(!notext)
#endif
			fprintf(file, "%i: %s\n", tmp->line, tmp->element);
	}
	fputs("\n", file);
	fclose(file);
}

#endif	/* MINIPARSE || DEBUG */

/*****
* Name:			ParserWriteHTMLOutputToFile
* Return Type: 	void
* Description: 	creates a HTML file from a parser tree
* In:
*	objects:	parser tree to be outputted;
*	prefix:		file prefix
*	notext:		if True, don't write plain text (ParserWriteOutput only)
* Returns:
*	nothing
*****/
#ifdef MINIPARSE
void
ParserWriteHTMLOutputToFile(XmHTMLObject *objects, String prefix,
	Boolean notext)
{
	XmHTMLObject *tmp;
	char name[1024];
	FILE *file;
	static int count;

	/*
	* No buffer overrun check here. If this sigsegv's, its your own fault.
	* Don't use names longer than 1024 bytes then.
	*/
	sprintf(name, "%s.%i.html", prefix, count);
	count++;

	if((file = fopen(name, "w")) == NULL)
	{
		perror(name);
		return;
	}

	for(tmp = objects; tmp != NULL; tmp = tmp->next)
	{
		if(tmp->id != HT_ZTEXT)
		{
			if(!tmp->ignore)
			{
				if(tmp->is_end)
					fprintf(file, "</%s", html_tokens[tmp->id]);
				else
					fprintf(file, "<%s", html_tokens[tmp->id]);

				if(tmp->attributes)
					fprintf(file, " %s", tmp->attributes);
				fputs(">", file);
				/* add a newline to auto inserted elements */
				if(tmp->auto_insert == 1 || notext)
					fputs("\n", file);
			}
		}
		else if(!notext)
			fprintf(file, "%s", tmp->element);
	}
	fputs("\n", file);
	fflush(file);
	fclose(file);
}
#endif

/*****
* Name: 		_ParserCutComment
* Return Type: 	String
* Description: 	removes HTML comments from the input stream
* In:
*	parser:		current parser context
*	start:		comment opening position;
* Returns:
*	comment ending position
* Note:
*	HTML comments are one of the most difficult things to deal with due to
*	the unlucky definition: a comment starts with a <!, followed by zero or
*	more comments, followed by >. A comment starts and ends with "--", and does
*	not contain any occurance of "--". This effectively means that dashes
*	*must* occur in a multiple of four. And this is were the problems lies:
*	_many_ people don't realize this and thus open their comments with <!-- and
*	end it with --> and put everything they like in it, including any number
*	of dashes and --> sequences. To deal with all of this as much as we can,
*	we scan the text until we reach a --> sequence with a balanced number of
*	dashes. If we run into a --> and we don't have a balanced number of dashes,
*	we look ahead in the buffer. The *original* comment is then terminated (by
*	rewinding to the original comment ending) if we encounter an element
*	opening (can be anything *except* <-) or if we run out of characters. If
*	we encounter another --> sequence, the comment ends here.
*	This is a severe performance penalty since a considerable number of
*	characters *can* be scanned in order to find an element start or the next
*	comment ending. Wouldn't life be *much* easier if we lived in a perfect
*	world!
*****/
String
_ParserCutComment(Parser *parser, String start)
{
	int dashes = 0, nchars = 0, start_line = parser->num_lines, nlines;
	Boolean end_comment = False, start_dashes = False;
	String chPtr = start;

	/* move past opening exclamation character */
	chPtr++;
	nlines = 0;
	while(!end_comment && *chPtr != '\0')
	{
		switch(*chPtr)
		{
			case '\n':
				nlines+=1;
				nchars++;
				break;	/* fix 01/14/97-01; kdh */
			case '-':
				/* comment dashes occur twice in succession */
				/* fix 01/14/97-02; kdh */
				/* fix 04/30/97-1; sl */
				if(*(chPtr+1) == '-' && !start_dashes)
				{
					chPtr++;
					nchars++;
					dashes++;
					start_dashes = True;
				}
				if(*(chPtr+1) == '-' || *(chPtr-1) == '-')
					dashes++;
				break;
			case '>':
				/*
				* Problem: > in a comment is a valid character, so the comment
				* should only be terminated when we have a multiple of four
				* dashes. If we haven't, we need to look ahead.
				*/
				if(*(chPtr-1) == '-')
				{
					if(!(dashes % 4))
						end_comment = True;
					else
					{
						char *sub = chPtr;
						Boolean end_sub = False;
						int sub_lines = nlines, sub_nchars = nchars;
						/*
						* Scan ahead until we run into another --> sequence or
						* element opening. If we don't, rewind and terminate the
						* comment.
						*/
						do
						{
							chPtr++;
							switch(*chPtr)
							{
								case '\n':
									nlines += 1;
									nchars++;
									break;	/* fix 01/14/97-01; kdh */
								case '-':
									if(*(chPtr+1) == '-' || *(chPtr-1) == '-')
										dashes++;
									break;
								case '<':
									/* comment ended at original position */
									if(*(chPtr+1) != '-')
									{
										chPtr = sub;
										end_sub = True;
									}
									break;
								case '>':
									/* comment ends here */
									if((strncmp(chPtr-2, "--", 2) == 0) &&
										start_dashes)
									{
										end_sub = True;
										end_comment = True;
										break;
									}
									/* another nested > */
									break;
								case '\0':
									/* comment ended at original position */
									chPtr = sub;
									end_sub = True;
									break;
							}
						}
						while(*chPtr != '\0' && !end_sub);
						if(chPtr == sub)
						{
							/* comment was ended at original position, rewind */
							end_comment = True;
							nlines = sub_lines;
							nchars = sub_nchars;
						}
					}
				}
				else /* special case: the empty comment */
					if(*(chPtr-1) == '!' && !(dashes % 4))
						end_comment = True;
				break;
		}
		chPtr++;
		nchars++;
	}

	_XmHTMLDebug(4, ("parse.c: _ParserCutComment, removed comment spanning "
		"%i chars between line %i and %i\n", nchars, start_line,
		start_line + nlines));

	parser->num_lines += nlines;

	/* spit out a warning if the dash count is no multiple of four */
	if(dashes %4 && parser->warn)
		_XmHTMLWarning(__WFUNC__(parser->widget, "parseHTML"),
			XMHTML_MSG_92, start_line, dashes);
	return(chPtr);
}

/*****
* A very usefull macro for handling shorttags.
* SGML shorttag handling. We use a buffer of a fixed length for storing an
* encountered token. We can do this safely ;-) since SGML shorttag's may
* *never* contain any attributes whatsoever. The fixed buffer does include
* some room for leading whitespace though.
* Note: all multi-line comments in this macro is placed as multiple
* single-line comments. Some cpp's choke on it otherwise..
*****/
#define NULL_END_TAG { \
	/* longest token is 10 chars (blockquote), include whitespace room */ \
	char token[16], *ptr; \
	htmlEnum id; \
	/* check if text between opening < and this first / is a valid html tag.*/ \
	/* Opening NULL-end tags must always be attached to the tag itself.*/ \
	if(chPtr - start > 15 || isspace(*(chPtr-1))) \
		break; \
	/* copy text */ \
	strncpy(token, start, chPtr - start); \
	token[chPtr - start] = '\0'; \
	ptr = token; \
	_XmHTMLDebug(4, ("parse.c: possible null-end token in: %s\n", token)); \
	/* cut leading spaces */ \
	while(*ptr && (isspace(*ptr))) \
	{ if(*ptr == '\n') parser->num_lines++; ptr++; } \
	/* make lowercase */ \
	my_locase(ptr); \
	_XmHTMLDebug(4, ("parse.c: checking null-end token %s\n", token)); \
	/* no warning message when _ParserTokenToId fails */ \
	if((id = _ParserTokenToId(parser, token, False)) != -1) \
	{ \
		/* store this element */ \
		(void)_ParserStoreElement(parser, start, chPtr); \
		_XmHTMLDebug(4,("parse.c: stored valid null-end token %s\n",token)); \
		/* move past the / */ \
		chPtr++; \
		text_start = chPtr; \
		text_len = 0; \
		/* walk up to the next / which terminates this block */ \
		for(; *chPtr != '\0' && *chPtr != '/'; chPtr++, cnt++, text_len++) \
			if(*chPtr == '\n') parser->num_lines++; \
		/* store the text */ \
		if(text_len && text_start != NULL) \
			parser->store_text(parser, text_start, chPtr); \
		text_start = chPtr + 1; /* starts after this slash */ \
		text_len = 0; \
		/* store the end element. Use the empty closing element notation so */ \
		/* storeElement knows what to do. Reset element ptrs after that. */ \
		(void)_ParserStoreElement(parser, "/>", chPtr); \
		start = NULL;		/* entry has been terminated */ \
		done = True; \
	} \
	else { _XmHTMLDebug(4, ("parse.c: %s: not a token.\n", token)); } \
}

/*****
* Name: 		parseHTML
* Return Type: 	void
* Description: 	html parser; creates a doubly linked list of all elements
*				(both HTML and plain text).
* In:
*	parser:		current parser context
* Returns:
*	nothing.
*****/
static void
parseHTML(Parser *parser)
{
	register char *chPtr;
	char *start, *text_start;
	int text_len = 0, cnt = 0;
	Dimension line_len;
	Boolean done;
	int on_stack = 0;
#ifndef MINIPARSE
	XmHTMLWidget html = (XmHTMLWidget)parser->widget;
#endif

	/* we assume all documents are valid ;-) */
	parser->bad_html = False;
	/* and that every document is HTML 3.2 conforming */
	parser->html32 = True;

	/* are we instructed to being strict on HTML 3.2 conformance ? */
#ifndef MINIPARSE
	parser->strict_checking = html->html.strict_checking;
	if(HTML_ATTR(string_direction) == XmSTRING_DIRECTION_R_TO_L)
		parser->store_text = _ParserStoreTextElementRtoL;
	else
		parser->store_text = _ParserStoreTextElement;
#else
	parser->strict_checking = parser_strict_checking;
	parser->store_text = _ParserStoreTextElement;
#endif

	/* start scanning */
	start = text_start = NULL;
	chPtr = parser->source;
	text_len = 0;
	parser->num_lines = 1;	/* every editor starts its linecount at 1 */
	parser->cstart    = 0;
	parser->cend      = 0;
	parser->line_len  = 0;
	line_len          = 0;

	while(*chPtr)
	{
		switch(*chPtr)
		{
			case '<':		/* start of element */
				/* See if we have any text pending */
				if(text_len && text_start != NULL)
				{
					parser->store_text(parser, text_start, chPtr);
					text_start = NULL;
					text_len = 0;
				}
				/* move past element starter */
				start = chPtr+1; /* element text starts here */
				done = False;
				/* absolute starting position for this element */
				parser->cstart = start - parser->source;
				/*
				* scan until the end of this tag. Comments are removed
				* properly. The behavior of XmHTML is undefined when a
				* comment is placed inside a tag.
				*/
				while(*chPtr != '\0' && !done)
				{
					chPtr++;
					switch(*chPtr)
					{
						case '<':
							/*
							* Aaie, another tag!
							* Check if it's a comment. If it is we
							* fall through, else we insert it.
							*/
							if(*(chPtr+1) != '!')
							{
								char *elePtr = start;
								/* was this a closing tag? */
								if(*elePtr == '/')
									elePtr++;
								/* first skip leading whitespace */
								while(*elePtr != '\0' && isspace(*elePtr))
									elePtr++;
								/*
								* now walk to the first non-alpha numeric
								* char
								*/
								while(*elePtr != '\0' && isalnum(*elePtr))
									elePtr++;

								if(parser->warn)
								{
									int len = elePtr - start;

									/* no overruns */
									char *msg = my_strndup(start,
										(len < 128 ? len : 128));

									_XmHTMLWarning(__WFUNC__(parser->widget,
										"parseHTML"), XMHTML_MSG_136,
										msg, parser->num_lines);
									free(msg);
								}
								/* store it */
								chPtr = _ParserStoreElement(parser, start,
											elePtr);
								done = True;
								break;
							}
							else
								chPtr++;
							/* fall through */
						case '!':
							/* either a comment or the !doctype */
							if((*(chPtr+1) == '>' || *(chPtr+1) == '-'))
							{
								chPtr = _ParserCutComment(parser, chPtr);
								/* back up one element */
								chPtr--;
								start = chPtr;
								done = True;
							}
							break;
							/*
							* those goddamn quotes. They should be balanced,
							* so we look ahead and see if we can find a
							* closing > after the closing quote. Anything
							* can appear within these quotes so we break
							* out of it once we find either < or /> inside
							* the quote or > after the closing quote.
							*/
						case '\"':
						{
							/* first look ahead for a corresponding " */
							char *tmpE, *tmpS = chPtr;

							chPtr++;	/* move past it */
							for(; *chPtr && *chPtr != '\"' && *chPtr != '>';
								parser->num_lines += *chPtr++ == '\n' ? 1 : 0);

							/* phieeuw, we found one */
							if(!*chPtr || *chPtr == '\"')
								break;

							/*
							* Fuck me, it's unbalanced, check if we
							* run into one before we see a <.
							* save position first.
							*/
							tmpE = chPtr;
							for(; *chPtr && *chPtr != '\"' && *chPtr != '<';
								parser->num_lines += *chPtr++ == '\n' ? 1 : 0);

							/* phieeuw, we found one */
							if(!*chPtr || *chPtr == '\"')
								break;

							/*
							* If we get here it means the element
							* didn't have a closing quote. Spit out
							* a warning and restore saved position.
							*/
							if(parser->warn)
							{
								int len = chPtr - tmpS;

								/* no overruns */
								char *msg = my_strndup(tmpS,
									(len < 128 ? len : 128));

								_XmHTMLWarning(__WFUNC__(parser->widget,
									"parseHTML"), XMHTML_MSG_93,
									msg, parser->num_lines);
								free(msg);
							}
							chPtr = tmpE;
							/* fall thru */
						}
						case '>':
							/* go and store the element */
							chPtr = _ParserStoreElement(parser, start, chPtr);
							done = True;
							break;
						case '/':
							/*
							* only handle shorttags when requested.
							* We have a short tag if this / is preceeded by
							* a valid character.
							*/
							if(isalnum(*(chPtr-1)))
							{
								NULL_END_TAG;
							}
							break;
						case '\n':
							parser->num_lines++;
							break;
						default:
							break;
					}
				}
				if(done)
					text_start = chPtr+1; /* plain text starts here */
				text_len = 0;
				start = NULL;
				break;
			case '\n':
				parser->num_lines++;
				if(cnt > line_len)
					line_len = cnt;
				cnt = -1;	/* fall through */
			default:
				cnt++;
				text_len++;
				break;
		}
		/* Need this test, we can run out of characters at *any* time. */
		if(*chPtr == '\0')
			break;
		chPtr++;
	}

	/* see if everything is balanced */
	if(parser->state_stack->next != NULL)
	{
		htmlEnum state;

		/* this is a bad html document */
		parser->bad_html = True;
		/* and thus not HTML 3.2 conforming */
		parser->html32 = False;

		/*
		* in very bad HTML documents, text might appear after the last
		* closing tag. For completeness, we need to flush that text also.
		* Please note that this can only happen when the stack is
		* unbalanced, and that's the reason it's in here and not outside
		* this stack test.
		*/
		if(text_len && text_start != NULL)
		{
			parser->store_text(parser, text_start, chPtr);
		}
		/* bad hack to make sure everything gets appended at the end */
		parser->cstart = strlen(parser->source);
		parser->cend   = parser->cstart + 1;
		/* make all elements balanced */
		while(parser->state_stack->next != NULL)
		{
			on_stack++;
			state = _ParserPopState(parser);
			_ParserInsertElement(parser, html_tokens[state], state, True);
		}
	}

#ifdef MINIPARSE
	if(parser_autocorrect_callback)
#ifdef DEBUG
		parser_autocorrect_callback(on_stack, p_inserted, p_removed);
#else
		parser_autocorrect_callback(on_stack, -1, -1);
#endif
#endif

	/*
	* allow lines to have 80 chars at maximum. It's only used when
	* the XmNresizeWidth resource is true.
	*/
	parser->line_len = (line_len > 80 ? 80 : line_len);

	_XmHTMLDebug(4, ("parse.c: parseHTML, allocated %i HTML elements "
		"and %i text elements (%i total).\n", parser->num_elements,
		parser->num_text, parser->num_elements + parser->num_text));

	_XmHTMLDebug(4, ("parse.c: parseHTML, removed %i unbalanced elements\n",
		p_removed));
	_XmHTMLDebug(4, ("parse.c: parseHTML, inserted %i missing elements\n",
		p_inserted));

	_XmHTMLDebug(4, ("_ParserTokenToId statistics\nno of lookups: %i\n"
		"Average search actions: %f\n", num_lookups,
		(float)num_searches/(float)num_lookups));

#ifdef DEBUG
#ifndef MINIPARSE
	if(html->html.debug_prefix)
		writeParsedOutputToFile(parser->head->next, html->html.debug_prefix);
#endif
#endif
}

#ifndef MINIPARSE

#define CUT_COMMENT { \
	int dashes = 0; \
	Boolean end_comment = False; \
	chPtr++; \
	while(!end_comment && *chPtr != '\0') \
	{ \
		switch(*chPtr) \
		{ \
			case '\n': \
				parser->num_lines++; \
				break; \
			case '-': \
				/* comment dashes occur twice in succession */ \
				if(*(chPtr+1) == '-') \
				{ \
					chPtr++; \
					dashes+=2; \
				} \
				break; \
			case '>': \
				if(*(chPtr-1) == '-' && !(dashes % 4)) \
					end_comment = True; \
				break; \
		} \
		chPtr++; \
	} \
}

/*****
* Name: 		parsePerfectHTML
* Return Type: 	void
* Description: 	html parser; creates a doubly linked list of all elements
*				(both HTML and plain text).
* In:
*	parser:		current parser context
* Returns:
*	nothing.
* Note:
*	This parser assumes the text it is parsing is ABSOLUTELY PERFECT HTML3.2.
*	No parserstack is used or created. It's only called when the mime type of
*	a document is text/html-perfect. No HTML shorttags & comments must be
*	correct. Elements of which the terminator is optional *MUST* have a
*	terminator present or bad things will happen.
*****/
static void
parsePerfectHTML(Parser *parser)
{
	register char *chPtr;
	char *start, *text_start;
	int text_len = 0, cnt = 0;
	Dimension line_len = 0;
	Boolean done;
	XmHTMLWidget html = (XmHTMLWidget)parser->widget;

	/* we assume all documents are valid ;-) */
	parser->bad_html = False;
	/* and that every document is HTML 3.2 conforming */
	parser->html32 = True;

	parser->strict_checking = True;
	if(HTML_ATTR(string_direction) == XmSTRING_DIRECTION_R_TO_L)
		parser->store_text = _ParserStoreTextElementRtoL;
	else
		parser->store_text = _ParserStoreTextElement;

	_XmHTMLDebug(4, ("parse.c: parsePerfectHTML, start\n"));

	/* start scanning */
	start = text_start = NULL;
	chPtr = parser->source;
	text_len = 0;
	parser->num_lines = 1;	/* every editor starts its linecount at 1 */
	parser->cstart    = 0;
	parser->cend      = 0;
	parser->line_len  = 0;
	line_len          = 0;

	while(*chPtr)
	{
		switch(*chPtr)
		{
			case '<':		/* start of element */
				/* See if we have any text pending */
				if(text_len && text_start != NULL)
				{
					parser->store_text(parser, text_start, chPtr);
					text_start = NULL;
					text_len = 0;
				}
				/* move past element starter */
				start = chPtr+1; /* element text starts here */
				done = False;
				/* absolute starting position for this element */
				parser->cstart = start - parser->source;

				/* scan until end of this tag */
				while(*chPtr != '\0' && !done)
				{
					chPtr++;
					switch(*chPtr)
					{
						case '!':
							/* either a comment or the !doctype */
							if((*(chPtr+1) == '>' || *(chPtr+1) == '-'))
							{
								CUT_COMMENT;
								/* back up one element */
								chPtr--;
								start = chPtr;
								done = True;
							}
							break;
						case '>':
							/* go and store the element */
							chPtr = storeElementUnconditional(parser, start,
										chPtr);
							done = True;
							break;
						case '\n':
							parser->num_lines++;
							break;
						default:
							break;
					}
				}
				if(done)
					text_start = chPtr+1; /* plain text starts here */
				text_len = 0;
				start = NULL;
				break;
			case '\n':
				parser->num_lines++;
				if(cnt > line_len)
					line_len = cnt;
				cnt = -1;	/* fall through */
			default:
				cnt++;
				text_len++;
				break;
		}
		/* Need this test, we can run out of characters at *any* time. */
		if(*chPtr == '\0')
			break;
		chPtr++;
	}
	/*
	* allow lines to have 80 chars at maximum. It's only used when
	* the XmNresizeWidth resource is true.
	*/
	parser->line_len = (line_len > 80 ? 80 : line_len);

	_XmHTMLDebug(4, ("parse.c: parsePerfectHTML, allocated %i HTML elements "
		"and %i text elements (%i total).\n", parser->num_elements,
		parser->num_text, parser->num_elements + parser->num_text));

	_XmHTMLDebug(4, ("_ParserTokenToId statistics\nno of lookups: %i\n"
		"Average search actions: %f\n", num_lookups,
		(float)num_searches/(float)num_lookups));
}

/*****
* Name: 		parsePLAIN
* Return Type: 	void
* Description: 	creates a parser tree for plain text.
* In:
*	parser:		current parser context
* Returns:
*	nothing
* Note:
* 	This routine adds html and body tags and the full text in a <pre></pre>.
*	We don't parse a single thing since it can screw up the autocorrection
*	routines when this raw text contains html commands.
*****/
static void
parsePLAIN(Parser *parser)
{
	register char *chPtr;
	int i, line_len;
	Boolean do_icons = parser->do_icons;

	line_len = i = 0;
	parser->num_lines = 1;	/* every editor starts its linecount at 1 */
	parser->cstart    = 0;
	parser->cend      = 0;

	_ParserInsertElement(parser, html_tokens[HT_HTML], HT_HTML, False);
	_ParserInsertElement(parser, html_tokens[HT_BODY], HT_BODY, False);
	_ParserInsertElement(parser, html_tokens[HT_PRE], HT_PRE, False);

	/* We don't expand icon entities appearing in plain text */
	parser->do_icons = False;

	/* store the raw text */
	chPtr = parser->source + parser->len;

	if(((XmHTMLWidget)parser->widget)->html.string_direction ==
		 XmSTRING_DIRECTION_R_TO_L)
		_ParserStoreTextElementRtoL(parser, parser->source, chPtr);
	else
		_ParserStoreTextElement(parser, parser->source, chPtr);

	/* count how many lines we have and get the longest line as well */
	for(chPtr = parser->source; *chPtr != '\0'; chPtr++)
	{
		switch(*chPtr)
		{
			case '\n':
				parser->num_lines++;
				if(i > line_len)
					line_len = i;
				i = 0;
				break;
			default:
				i++;
		}
	}

	/* add closing elements */
	_ParserInsertElement(parser, html_tokens[HT_PRE], HT_PRE, True);
	_ParserInsertElement(parser, html_tokens[HT_BODY], HT_BODY, True);
	_ParserInsertElement(parser, html_tokens[HT_HTML], HT_HTML, True);

	/* reset do_icons */
	parser->do_icons = do_icons;

	/* maximum line length */
	parser->line_len = (line_len > 80 ? 80 : line_len);
}

static const char *content_image = "<html><body><img src=\"%s\"></body></html>";

/*****
* Name: 		parseImage
* Return Type: 	void
* Description: 	creates a parser tree for the given image so XmHTML can
*				display it.
* In:
*	parser:		current parser context
* Returns:
*	nothing.
*****/
static void
parseIMAGE(Parser *parser)
{
	String input, tmpPtr;

	/* save original input */
	input = parser->source;

	/* create a temporary HTML source text for this image */
	tmpPtr = (char*)malloc((strlen(content_image) + parser->len + 1)*
		sizeof(char));
	sprintf(tmpPtr, content_image, parser->source);

	/* set it */
	parser->source = tmpPtr;
	parser->len    = strlen(tmpPtr);

	/* parse it */
	parseHTML(parser);

	/* free temporary source */
	free(tmpPtr);

	/* restore original source */
	parser->source = input;
	parser->len    = strlen(input);
}
#endif /* MINIPARSE */

/*****
* Name: 		parserDriver
* Return Type: 	XmHTMLObject*
* Description: 	main HTML parser driver.
* In:
*	html:		XmHTMLWidget id;
*	old_list:	objects to be freed;
*	input:		text to parse.
* Returns:
*	a newly generated parser tree.
*****/
static XmHTMLObject *
parserDriver(XmHTMLWidget html, XmHTMLObject *old_list, String input)
{
	Parser *parser;
	XmHTMLObject *list_return;

	/* free any previous list */
	if(old_list != NULL)
	{
		_XmHTMLFreeObjects(old_list);
		old_list = NULL;
	}

	/* create a parser */
	parser = _ParserCreate((Widget)html);

	/* always in automatic mode */
	parser->automatic = True;

	/* reset debug counters */
#ifdef DEBUG
	num_lookups = num_searches = p_inserted = p_removed = 0;
#endif

#ifdef MINIPARSE
	id_depth = 0;
	parser_errors = 0;
#endif

	/* First copy the input text to a private buffer, it will get modified. */
	parser->len    = strlen(input);
	parser->source = my_strndup(input, parser->len);

#ifndef MINIPARSE
	/* parse text */
	if(!(strcasecmp(html->html.mime_type, "text/html")))
	{
		html->html.mime_id = XmPLC_DOCUMENT;
		parseHTML(parser);
	}
	else if(!(strcasecmp(html->html.mime_type, "text/html-perfect")))
	{
		html->html.mime_id = XmPLC_DOCUMENT;
		parsePerfectHTML(parser);
	}
	else if(!(strcasecmp(html->html.mime_type, "text/plain")))
	{
		/* plain text parser should never honor icon substitution */
		parser->do_icons = False;
		html->html.mime_id = XmHTML_NONE;
		parsePLAIN(parser);
	}
	else if(!(strncasecmp(html->html.mime_type, "image/", 6)))
	{
		html->html.mime_id = XmPLC_IMAGE;
		parseIMAGE(parser);
	}
#else
	/* only HTML for standalone parser */
	parseHTML(parser);

	parsed_object_count = parser->num_elements;
	parsed_text_object_count = parser->num_text;
#endif

	/* Set return list. The first element in the list is a dummy one */
	list_return = parser->head->next;
	/* sanity */
	if(list_return)
		list_return->prev = NULL;

	parser->current = (XmHTMLObject*)NULL;

	/* these are the only globals we use in the entire parser module */
	bad_html = parser->bad_html;
	html32   = parser->html32;

	/* delete this parser */
	_ParserDelete(parser);

	/* all done! */
	return(list_return);
}

/********
****** Public Functions
********/

/*****
* Name: 		_XmHTMLparseHTML
* Return Type: 	XmHTMLObject*
* Description: 	html parser driver
* In:
*	html:		XmHTMLWidget id
*	old_list:	previous list to be freed.
*	input:		HTML text to parse
*	dest:		destination widget id, if any
* Returns:
*	nothing when standalone, parsed list of objects otherwise.
*****/
XmHTMLObject*
_XmHTMLparseHTML(XmHTMLWidget html, XmHTMLObject *old_list, char *input,
	XmHTMLWidget dest)
{
	static XmHTMLObject *output;
	XmHTMLObject *prev_output, *checked, *prev_list;
	String text;
	Boolean redo = False;
	int loop_count = 0;
#ifdef MINIPARSE
	int input_len = 0;
#endif

	/* free any previous list */
	if(old_list != NULL)
	{
		_XmHTMLFreeObjects(old_list);
		old_list = NULL;
	}

	/* sanity check */
	if(input == NULL || *input == '\0')
		return(NULL);

	prev_output = old_list;
	text = input;

	/*
	* block setValues while we are parsing. This is required as a large
	* number of resources can initialize a recomputation of everything.
	* This *can* be rather funest since we are currently parsing the text
	* that is to be formatted!
	* (the least effects would be a double call to the formatter and
	*  a visible flickering of the screen).
	*/
#ifndef MINIPARSE
	if(dest)
		ATTR_HTML(dest,in_layout) = True;

	/* save ptr to current parser tree */
	prev_list = ATTR_HTML(html, elements);
#else
	input_len = strlen(text);
#endif

	do
	{
		_XmHTMLDebug(4, ("parse.c, _XmHTMLparseHTML, doing pass %i\n",
			loop_count));

		checked = NULL;
		redo = False;

		output = parserDriver(html, prev_output, text);

		/* sanity */
		if(output == NULL)
		{
			/* need to free parser output also */
			if(loop_count)
				free(text);
			/* release SetValues lock */
#ifndef MINIPARSE
			if(dest)
				dest->html.in_layout = True;
#endif
			return(NULL);
		}

		/*
		* If the state stack was unbalanced, check if the verification/repair
		* routines produced a balanced parser tree.
		*/
		if(bad_html)
			checked = _ParserVerifyVerification(output);

		/*
		* If we have a document callback, call it now. If we don't have one
		* and verifyVerification failed, we iterate again on the
		* current document.
		* The verify stuff mimics the default DocumentCallback behaviour,
		* that is, advise another pass if the parser tree is unbalanced.
		* The reason for this is that, although a document may have yielded
		* an unbalanced state stack, the parser tree *is* properly balanced
		* and hence suitable for usage.
		*
		* Additional note, 08/15/97: we only have to call an installed
		* documentCallback if dest is non-NULL, in which case we have
		* been called to parse new text. If dest *is* NULL we have been
		* called to parse something else (for now only externally installed
		* imageMaps can cause this).
		*/
#ifndef MINIPARSE
		if(html->html.document_callback && dest)
		{
			if(loop_count)
				free(text);
			text = NULL;

			ATTR_HTML(dest,elements) = output;

			/*****
			* Required to allow access to the current widget without
			* causing a crash
			*****/
			ATTR_HTML(html,elements) = output;

			redo = _XmHTMLDocumentCallback(html, html32,
				!bad_html, checked == NULL, False, loop_count);

			/*
			* We have been requested to do another pass on the current
			* document.
			*/
			if(redo)
			{
				/* save ptr to current parser output */
				prev_output = output;

				/* pull new source out of the parser */
				text = _XmHTMLTextGetString(output);
			}
		}
#else
		if(parser_document_callback)
		{
			if(loop_count)
				free(text);
			text = NULL;

			redo = parser_document_callback(output, html32, !bad_html,
					checked == NULL, loop_count, input_len);

			/*
			* We have been requested to do another pass on the current
			* document.
			*/
			if(redo)
			{
				/* save ptr to current parser output */
				prev_output = output;

				/* pull new source out of the parser */
				text = _XmHTMLTextGetString(output);

				/* get new text length */
				input_len = strlen(text);
			}
		}
#endif
		/* no document callback, do one iteration using parser tree output */
		else
		{
			/* no document callback, so we can never have a new source */
			if(loop_count)
				free(text);
			text = NULL;
			redo = False;

			/* only one additional loop if parser tree wasn't balanced */
			if((loop_count < 2 && checked != NULL))
			{
				/* save ptr to current parser output */
				prev_output = output;
				redo = True;
				text = _XmHTMLTextGetString(output);
#ifdef MINIPARSE
				/* get new text length */
				input_len = strlen(text);
#endif
			}
		}
		loop_count++;
	}
	while(redo);

	/*
	* Free parser output if loop_count is larger than one and we did not
	* receive a new source. text check required so we don't crash if no new
	* source has been provided.
	*/
	if(loop_count > 1 && text != NULL)	/* fix 06/18/97-01, dp */
		free(text);

#ifndef MINIPARSE
	/****
	* Reset parser tree, we might have put something different inside it
	* if we had a document callback.
	*****/
	ATTR_HTML(html, elements) = prev_list;
	/* release SetValues lock & transfer mimetype */
	if(dest)
	{
		ATTR_HTML(dest,in_layout) = True;
		ATTR_HTML(dest,mime_id) = ATTR_HTML(html,mime_id);
	}
#else
	if(loop_count == 5 && (bad_html || checked != NULL))
	{
		_XmHTMLWarning(__WFUNC__(NULL, "_XmHTMLparseHTML"), XMHTML_MSG_94);
	}
#endif /* MINIPARSE */

	return(output);
}

/*****
* Name: 		_XmHTMLTextGetString
* Return Type: 	String
* Description: 	creates a HTML source document from the given parser tree.
* In:
*	objects:	parser tree.
* Returns:
*	created document in a buffer. This buffer must be freed by the caller.
*****/
String
_XmHTMLTextGetString(XmHTMLObject *objects)
{
	XmHTMLObject *tmp;
	static String buffer;
	String chPtr;
	int i, size = 0;
	int *sizes;	/* for storing element lengths */

	if(objects == NULL)
		return(NULL);

	sizes = (int*)malloc((int)HT_ZTEXT*sizeof(int));

	for(i = 0; i < (int)HT_ZTEXT; i++)
		sizes[i] = strlen(html_tokens[(htmlEnum)i]);

	/* first pass, compute length of buffer to allocate */
	for(tmp = objects; tmp != NULL; tmp = tmp->next)
	{
		if(tmp->id != HT_ZTEXT)
		{
			if(tmp->is_end)
				size += 1;	/* a / */

			/* a pair of <> + element length */
			size += 2 + sizes[(int)tmp->id];

			/* a space and the attributes */
			if(tmp->attributes)
				size += 1 + strlen(tmp->attributes);
		}
		else
			size += strlen(tmp->element);
	}
	size +=1;	/* terminating character */

	buffer = (String)malloc(size * sizeof(char));
	chPtr = buffer;

	/* second pass, compose the text */
	for(tmp = objects; tmp != NULL; tmp = tmp->next)
	{
#ifdef MINIPARSE
		if(tmp->ignore)
			continue;
#endif
		if(tmp->id != HT_ZTEXT)
		{
			*chPtr++ = '<';
			if(tmp->is_end)
				*chPtr++ = '/';
			strcpy(chPtr, html_tokens[tmp->id]);
			chPtr += sizes[(int)tmp->id];

			/* a space and the attributes */
			if(tmp->attributes)
			{
				*chPtr++ = ' ';
				strcpy(chPtr, tmp->attributes);
				chPtr += strlen(tmp->attributes);
			}
			*chPtr++ = '>';
		}
		else
		{
			strcpy(chPtr, tmp->element);
			chPtr += strlen(tmp->element);
		}
	}
	*chPtr = '\0';	/* NULL terminate */
	free(sizes);	/* no longer needed */
	return(buffer);
}

/*****
* Name: 		_XmHTMLFreeObjects
* Return Type: 	void
* Description: 	releases all memory occupied by the parsed list of objects.
* In:
*	objects:	object list to be destroyed
* Returns:
*	nothing.
*****/
void
_XmHTMLFreeObjects(XmHTMLObject *objects)
{
	XmHTMLObject *temp;
#ifdef DEBUG
	int i = 0;
#endif

	/* free all parsed objects */
	while(objects)
	{
		temp = objects->next;
		/* sanity check. Should not be needed anyway. */
		if(objects->element)
			free(objects->element);
		free(objects);
		objects = temp;
#ifdef DEBUG
		i++;
#endif
	}
	objects = NULL;

	_XmHTMLDebug(4, ("parse.c: _XmHTMLFreeObjects, freed %i elements\n", i));
}

#ifdef MINIPARSE

/*****************************************************************************
 *                                                                           *
 *             Helper functions for the Standalone parser                    *
 *                                                                           *
 *****************************************************************************/

/***
* Character translation table for converting from upper to lower case
* Since this is a table lookup, it might perform better than the libc
* tolower routine on a number of systems.
***/
const Byte __my_translation_table[256]={
	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,
	24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,
	45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,97,98,
	99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,
	116,117,118,119,120,121,122,91,92,93,94,95,96,97,98,99,100,101,102,
	103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,
	120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,
	137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,
	154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,
	171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,
	188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,
	205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,
	222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,
	239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255};

void
my_locase(char *string)
{
	register char *outPtr = string;
	for(outPtr = string; *outPtr != '\0';
		*(outPtr++) = _FastLower(*(string++)));
}

char*
my_strcasestr(const char *s1, const char *s2)
{
	register int i;
	register const char *p1, *p2, *s = s1;

	for (p2 = s2, i = 0; *s; p2 = s2, i++, s++)
	{
		for (p1 = s; *p1 && *p2 && _FastLower(*p1) == _FastLower(*p2);
				p1++, p2++)
			;
		if (!*p2)
			break;
	}
	if (!*p2)
		return((char*)s1 + i);
	return(NULL);
}

char*
my_strndup(const char *s1, size_t len)
{
	register int i;
	register char *p2;
	register const char *p1 = s1;
	static char *s2;

	if(len == 0 || s1 == NULL || *s1 == '\0')
		return(NULL);

	s2 = (char*)malloc(len+1);

	for(p2 = s2, i = 0; *p1 && i < len; *(p2++) = *(p1++), i++);

	/* NULL padding */
	while(i < len-1)
	{
		*(p2++) = '\0';
		i++;
	}

	*p2 = '\0';	/* NULL terminate */

	return(s2);
}

#include <assert.h>
/*****
* Name: 		my_strdup
* Return Type: 	char*
* Description: 	debugging version of strdup
* In:
*	s1:			string to be duplicated
* Returns:
*	duplicated string.
*****/
char*
my_strdup(const char *s1)
{
	static char *ret_val;

	assert(s1 != NULL);

	ret_val = malloc(strlen(s1)+1);
	strcpy(ret_val, s1);
	return(ret_val);
}

#ifdef NEED_STRCASECMP
int
my_strcasecmp (const char *s1, const char *s2)
{
	register int c1, c2;

	while (*s1 && *s2)
	{
		c1 = _FastLower(*s1);
		c2 = _FastLower(*s2);
		if (c1 != c2)
			return(c1 - c2);
		s1++;
		s2++;
	}
	return((int)(*s1 - *s2));
}
#define strcasecmp(S1,S2) my_strcasecmp(S1,S2);

int
my_strncasecmp (const char *s1, const char *s2, size_t n)
{
	register int c1, c2, l=0;

	while (*s1 && *s2 && l < n)
	{
		c1 = tolower(*s1);
		c2 = tolower(*s2);
		if (c1 != c2)
			return(c1 - c2);
		s1++;
		s2++;
		l++;
	}
	return((int)(0));
}

#define strcasecmp(S1,S2) my_strcasecmp(S1,S2)
#define strncasecmp(S1,S2,N) my_strncasecmp(S1,S2,N)

#endif

void
#ifdef __STDC__
__XmHTMLWarning(Widget w, String module, int line, String routine,
	String fmt,...)
{
	va_list arg_list;
	char buf[1024];

	if(!parser_warnings)
		return;

	va_start(arg_list, fmt);

#else /* ! __STDC__ */
__XmHTMLWarning(w, module, line, routine, fmt, va_alist)
	Widget w;
	String module;
	String routine;
	String fmt;
	va_dcl
{
	va_list arg_list;
	char buf[1024];

	if(!parser_warnings)
		return;

	va_start(arg_list);

#endif /* __STDC__ */

	vsprintf(buf, fmt, arg_list);
	va_end(arg_list);
	if(w != NULL)
		fprintf(stderr, "Warning:\n    %s\n", buf);
	else
		fprintf(stderr, "Warning:    %s\n", buf);
	fprintf(stderr, "    (%s, %s, line %i)\n", module, routine, line);
}

#endif
