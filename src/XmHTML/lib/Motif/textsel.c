#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* textsel.c : XmHTML Widget selection & find routines
*
* This file Version	$Revision$
*
* Creation date:		Fri Feb 13 14:01:33 GMT+0100 1998
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
*
* Copyright (C) 1994-1998 by Ripley Software Development 
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
* Revision 1.1  2011/06/30 16:10:30  rwcox
* Cadd
*
* Revision 1.2  1998/04/27 07:03:48  newt
* tka stuff
*
* Revision 1.1  1998/04/04 06:27:28  newt
* Initial Revision
*
*****/ 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "toolkit.h"
#include XmHTMLPrivateHeader

#ifdef HAVE_REGEX_H
#include <regex.h>
/*****
* Some POSIX systems define REG_NOERROR (GNU libc) and some don't (IRIX 6.3,
* HPUX 10.20a) so we check if it's defined or not.
* Solaris has REG_OK instead (Frank Linneweber <Frank.Linneweber@kryptokom.de>)
*****/
#ifndef REG_NOERROR
# ifdef REG_OK
#  define REG_NOERROR	REG_OK
# else
#  define REG_NOERROR	0
# endif	/* REG_OK */
#endif	/* !REG_NOERROR */
#else
#include "../compat/GNUregex.h"
#endif

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/
/*****
* A stripped down array of all words that are searchable. Contains enough
* information to search for text and provide information about the
* selection that should be made to display the text found. 
*****/
typedef struct _WordTab{
#ifdef DEBUG
	String 				word;		/* word to display						*/
#endif
	int 				len;		/* string length of word				*/
	int					pos;		/* absolute character position			*/
	int					word_idx;	/* index of word in owner word array	*/
	XmHTMLObjectTable	*owner;		/* owner of this word					*/
}WordTab;

/*****
* A Finder Context. Used for searching through the contents of the currently
* displayed document.
*****/
struct _XmHTMLTextFinder{
	/* pattern information */
	regex_t					pattern;	/* compiled search pattern		*/
	int						last_err;	/* last known error				*/
	int						flags;		/* regcomp flags				*/
	String					to_find;	/* string to be found			*/

	/* finder status */
	Boolean					active;		/* True when finder is used		*/
	Boolean					have_pat;	/* True when pattern is present	*/
	Boolean					ic;			/* ignore case					*/
	XmHTMLDirection			direction;	/* search direction				*/
	int						cur_pos;	/* current position				*/

	/* search table & text */
	WordTab					*fast_tab;	/* array of searchable words	*/
	String					text;		/* full clean text buffer		*/
	int						nwords;		/* no of searchable words		*/
	int						nchars;		/* total no of chars			*/

	/* match data */
	int						first;		/* idx of first matching word	*/
	int						first_char;	/* absolute pos of first char	*/
	int						last;		/* idx of last matching word	*/
	int						last_char;	/* absolute pos of last char	*/
	int						nmatch;		/* no of matching chars			*/
};

/*** Private Function Prototype Declarations ****/

/*** Private Variable Declarations ***/

/*****
* Name:			CreateWordList
* Return Type:	Boolean
* Description:	converts all text to an array of WordTab and creates a
*				string buffer containing all (raw) text suitable for
*				regex searching. Word positions in the raw text correspond
*				with positions in the WordTab array.
* In:
*	html:		XmHTMLWidget id;
*	finder:		finder for which to create a WordTab array and text buffer.
* Returns:
*	True on success and False on failure.
*****/
static Boolean
CreateWordList(XmHTMLWidget html, XmHTMLTextFinder finder)
{
	XmHTMLObjectTable *elePtr;
	XmHTMLWord *words;
	String text = NULL, chPtr;
	static WordTab *fast_tab = NULL;
	int nwords = 0, nchars = 0, n_words, maxchars;
	int i, j, k;

	/*****
	* Count how many words we have and compose a text buffer containing
	* all text. We ignore words of type IMG and FORM.
	*****/
	for(elePtr = html->html.formatted; elePtr != NULL; elePtr = elePtr->next)
	{
		if(elePtr->object_type == OBJ_TEXT)
		{
			int word_len = 0;

			n_words = elePtr->n_words;
			words = elePtr->words;
			for(i = 0; i < n_words; i++)
			{
				if(words[i].type == OBJ_TEXT ||
					words[i].type == OBJ_BLOCK)
				{
					if(words[i].type == OBJ_BLOCK)
					{
						nwords++;
						word_len = 1;
					}
					else
					{
						if(!(words[i].spacing & TEXT_SPACE_TRAIL) && 
							i+1 < n_words &&
							!(words[i+1].spacing & TEXT_SPACE_LEAD))
						{
							int k = i+1;
							word_len = words[i].len;
							while(k < n_words)
							{
								if(!(words[k].spacing & TEXT_SPACE_LEAD))
									word_len += words[k].len;
								/*****
								* see if this word has a trailing space and
								* the next a leading
								*****/
								if(!(words[k].spacing & TEXT_SPACE_TRAIL)
									 && k+1 < n_words &&
									!(words[k+1].spacing & TEXT_SPACE_LEAD))
									k++;
								else
									break;
							}
						}
						else
							word_len = words[i].len;
					}
					nwords++;
					nchars += word_len + 1;	/* and a space */
				}
				else
				{
					/* images & form elements are considered a space */
					nwords++;
					nchars += 1;
				}
			}
		}
		else
		{
			/* non-text objects are converted to newlines */
			nwords++;
			nchars += 1;
		}
	}

	/* no searchable text found */
	if(nwords == 0)
	{
		finder->last_err = RE_EWORDS;
		return(False);
	}

#ifdef DEBUG
	_XmHTMLDebug(18, ("WordTab & text buffer creation, WordTab array will "
		"contain %i words\nand text buffer will use %ibytes\n.", nwords,
		nchars));
#endif

	/* sanity */
	if((fast_tab = (WordTab*)calloc(nwords, sizeof(WordTab))) == NULL)
	{
		finder->last_err = RE_ENOMEM;
		return(False);
	}

	nchars++;	/* for terminator */

	if((text = (String)malloc(nchars * sizeof(char))) == NULL)
	{
		free(fast_tab);
		finder->last_err = RE_ENOMEM;
		return(False);
	}

	/* okay, got it. Now fill the searchable table */
	j = 0;
	chPtr = text;
	maxchars = nchars;
	nchars = 0;

	for(elePtr = html->html.formatted; elePtr != NULL; elePtr = elePtr->next)
	{
		if(elePtr->object_type == OBJ_TEXT)
		{
			n_words = elePtr->n_words;
			words = elePtr->words;
			for(i = 0; i < n_words; i++)
			{
				if(words[i].type == OBJ_TEXT ||
					words[i].type == OBJ_BLOCK)
				{
					if(words[i].type == OBJ_BLOCK)
					{
						*chPtr++ = '\n';
#ifdef DEBUG
						fast_tab[i].word = "\n";
#endif
						fast_tab[j].len      = 1;
						fast_tab[j].owner    = words[i].owner;
						fast_tab[j].word_idx = i;
						fast_tab[j].pos      = nchars;
						nchars++;

						my_assert(nchars < maxchars);

						/* move to next slot */
						j++;
					}
					else
					{
						if(!(words[i].spacing & TEXT_SPACE_TRAIL) && 
							i+1 < n_words &&
							!(words[i+1].spacing & TEXT_SPACE_LEAD))
						{
							/* store first word */
#ifdef DEBUG
							fast_tab[j].word = words[i].word;
#endif
							fast_tab[j].len      = words[i].len;
							fast_tab[j].owner    = words[i].owner;
							fast_tab[j].word_idx = i;
							fast_tab[j].pos      = nchars;
							nchars += fast_tab[j].len;

							my_assert(nchars < maxchars);

							/* move to next slot */
							j++;

							k = i+1;
							while(k < n_words)
							{
								if(!(words[k].spacing & TEXT_SPACE_LEAD))
								{
#ifdef DEBUG
									fast_tab[j].word = words[k].word;
#endif
									fast_tab[j].len      = words[k].len;
									fast_tab[j].owner    = words[k].owner;
									fast_tab[j].word_idx = k;
									fast_tab[j].pos      = nchars;
									nchars += fast_tab[j].len;

									my_assert(nchars < maxchars);

									/* copy word */
									memcpy(chPtr, words[k].word,
										words[k].len);
									chPtr += words[k].len;
									
									/* move to next slot */
									j++;
								}
								if(!(words[k].spacing & TEXT_SPACE_TRAIL)
									 && k+1 < n_words &&
									!(words[k+1].spacing & TEXT_SPACE_LEAD))
									k++;
								else
									break;
							}
						}
						else
						{
#ifdef DEBUG
							fast_tab[j].word = words[i].word;
#endif
							fast_tab[j].len      = words[i].len;
							fast_tab[j].owner    = words[i].owner;
							fast_tab[j].word_idx = i;
							fast_tab[j].pos      = nchars;
							nchars += fast_tab[j].len;
							my_assert(nchars < maxchars);

							/* copy word */
							memcpy(chPtr, words[i].word, words[i].len);
							chPtr += words[i].len;

							/* move to next slot */
							j++;
						}
						/* add a space */
						*chPtr++ = ' ';
						nchars++;
						my_assert(nchars < maxchars);
					}
				}
				else
				{
					/* images & form elements are considered as a space */
#ifdef DEBUG
					fast_tab[j].word = " ";
#endif
					fast_tab[j].len      = 1;
					fast_tab[j].owner    = words[i].owner;
					fast_tab[j].word_idx = i;
					fast_tab[j].pos      = nchars;

					*chPtr++ = ' ';
					nchars++;

					my_assert(nchars < maxchars);

					/* move to next slot */
					j++;
				}
			}
		}
		else
		{
			/* non-text objects are converted to newlines */
#ifdef DEBUG
			fast_tab[j].word = "\n";
#endif
			fast_tab[j].len      = 1;
			fast_tab[j].owner    = elePtr;
			fast_tab[j].word_idx = -1;
			fast_tab[j].pos      = nchars;

			*chPtr++ = '\n';
			nchars++;

			my_assert(nchars < maxchars);

			/* move to next slot */
			j++;
		}
	}
	/* terminate text */
	*chPtr++ = '\0';

	_XmHTMLDebug(18, ("WordTab & text buffer creation, %i words and %i "
		"bytes used." "\n", nwords, nchars));
	_XmHTMLDebug(18, ("Text buffer:\n-----\n%s\n-----\n", text));

	finder->fast_tab = fast_tab;
	finder->nwords   = nwords;
	finder->nchars   = nchars;
	finder->text     = text;
	finder->cur_pos  = 0;
	return(True);
}

String
XmHTMLTextGetSelection(Widget w)
{
	return(NULL);
}

void
XmHTMLTextClearSelection(Widget w, Time time)
{
	return;
}

Boolean
XmHTMLTextSetSelection(Widget w, XmHTMLTextPosition first,
	XmHTMLTextPosition last, Time time)
{
	return(False);
}

void
XmHTMLTextSetHighlight(Widget w, XmHTMLTextPosition first,
	XmHTMLTextPosition last, XmHighlightMode mode)
{
	if(first.start == NULL)
		return;

	switch(mode)
	{
		case XmHIGHLIGHT_NORMAL:
			_XmHTMLPaint((XmHTMLWidget)w, first.start, last.start);
			break;
		case XmHIGHLIGHT_SELECTED:
		case XmHIGHLIGHT_SECONDARY_SELECTED:
			{
				XRectangle rect;
				GC gc;
				XGCValues gcv;
				XmHTMLWidget html = (XmHTMLWidget)w;
				XmHTMLObjectTable *elePtr, *end = NULL;
				XmHTMLWord *word;
				int i,j = first.idx, nwords = first.nwords;

				gcv.foreground = BlackPixelOfScreen(XtScreen(w));
				gc = XtGetGC(w, GCForeground, &gcv);

				if(first.start == last.start)
				{
					for(i = first.idx; i < last.idx + 1; i++)
					{
						word = &(first.start->words[i]);

						rect.x = word->x;
						rect.y = word->y -
							word->font->height + word->font->descent;
						rect.width  = word->width;
						rect.height = word->height;

						XDrawRectangle(XtDisplay(w),
							XtWindow(html->html.work_area), gc,
							rect.x - html->html.scroll_x,
							rect.y - html->html.scroll_y,
							rect.width, rect.height);
					}
				}
				else
				{
					XmHTMLObjectTable *end;
				 
					end = (last.start ? last.start->next : NULL);

					for(elePtr = first.start; elePtr != end;
						elePtr = elePtr->next)
					{
						if(elePtr->object_type == OBJ_TEXT)
						{
							for(i = j; i < nwords; i++)
							{
								word = &(elePtr->words[i]);

								rect.x = word->x;
								rect.y = word->y -
										word->font->height +
										word->font->descent;
								rect.width  = word->width;
								rect.height = word->height;

								XDrawRectangle(XtDisplay(w),
									XtWindow(html->html.work_area), gc,
									rect.x - html->html.scroll_x,
									rect.y - html->html.scroll_y,
									rect.width, rect.height);
							}
						}
						j = 0;
						nwords = (elePtr == last.start ?
									last.idx + 1 : elePtr->n_words);
					}
				}
				XtReleaseGC(w, gc);
			}
			break;
		default:
			break;
	}
}

Boolean
XmHTMLTextCopy(Widget w, Time time)
{
	return(False);
}

Boolean
XmHTMLTextShowPosition(Widget w, XmHTMLTextPosition position)
{
	return(False);
}

/*****
* Name: 		XmHTMLTextFinderCreate
* Return Type: 	XmHTMLTextFinder
* Description:	Create a new Finder Context suitable for searching the
*				contents of a HTML Widget;
* In: 
*	w:			XmHTMLWidget id;
* Returns:
*	a new Finder context.
*****/
XmHTMLTextFinder
XmHTMLTextFinderCreate(Widget w)
{
	static XmHTMLTextFinder finder = NULL;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "TextFinderCreate");
		return(NULL);
	}

	if((finder = (XmHTMLTextFinder)calloc(1,
		sizeof(struct _XmHTMLTextFinder))) != NULL)
	{
		finder->direction = XmHTML_FORWARD;
		finder->flags     = REG_EXTENDED;
		if(!(CreateWordList((XmHTMLWidget)w, finder)))
		{
			free(finder);
			finder = NULL;
		}
	}
	return(finder);
}

/*****
* Name:			XmHTMLTextFinderDestroy
* Return Type: 	void
* Description: 	destroys a finder context and frees any resources allocated
*				by it.
* In: 
*	finder:		finder to be destroyed.
* Returns:
*	nothing.
*****/
void
XmHTMLTextFinderDestroy(XmHTMLTextFinder finder)
{
	/* free pattern (if any) */
	if(finder->have_pat)
	{
		regfree(&(finder->pattern));
		free(finder->to_find);
	}

	/* free wordlist */
	if(finder->nwords)
	{
		free(finder->fast_tab);
		free(finder->text);
	}

	free(finder);
}

/*****
* Name: 		XmHTMLTextFinderSetPattern
* Return Type: 	Boolean
* Description:	sets the search pattern in a finder.
* In: 
*	finder:		finder to be updated;
*	to_find:	text to be searched;
* Returns:
*	True when pattern was successfully set, False if not.
*	Error information is stored internally.
*****/
Boolean
XmHTMLTextFinderSetPattern(XmHTMLTextFinder finder, String to_find)
{
	int flags = finder->flags;

	/* See if we have a string to be found */
	if(to_find == NULL)
	{
		finder->last_err = RE_EEMPTY;
		return(False);
	}

	/* delete any existing pattern */
	if(finder->have_pat)
	{
		regfree(&(finder->pattern));
		free(finder->to_find);
		finder->have_pat = False;
		finder->to_find  = NULL;
	}

	/* set case-insensitive flag if we must ignore case while searching */
	if(finder->ic)
		flags |= REG_ICASE;

	/* compile search text into a pattern */
	if((finder->last_err = regcomp(&(finder->pattern), to_find, flags)) != 0)
		return(False);

	/* reset all */
	finder->to_find    = strdup(to_find);
	finder->have_pat   = True;
	finder->active     = False;
	finder->cur_pos    = 0;
	finder->first      = -1;
	finder->first_char = -1;
	finder->last       = -1;
	finder->last_char  = -1;
	finder->nmatch     = -1;

	return(True);
}

/*****
* Name:			XmHTMLTextFinderSetPatternFlags
* Return Type: 	Boolean
* Description: 	allows users to fine-tune the pattern-compiler by providing
*				their own regcomp flags.
* In: 
*	finder:		current Finder Context;
*	flags:		regcomp flags to be used.
*	direction:	search direction.
*	ignore_...:	if True, case is ignored when searching
* Returns:
*	True when flags have been succesfully changed, False if not.
* Note:
*	validity checking of the provided flags is rather impossible, so the
*	caller better be sure that the flags are correct or regcomp will fail
*	miserably.
*****/
Boolean
XmHTMLTextFinderSetPatternFlags(XmHTMLTextFinder finder, int flags,
	Boolean ignore_case, XmHTMLDirection direction)
{
	Boolean ret_val = True;

	/*****
	* If we already had a compiled pattern, free and compile again using
	* the new flags. This actually allows the caller to change the
	* search style at *any* time.
	*****/
	if(finder->have_pat && finder->flags != flags && flags != -1)
	{
		struct _XmHTMLTextFinder dummy;
		String to_find = strdup(finder->to_find);

		/* remember current state */
		memset(&dummy, 0, sizeof(struct _XmHTMLTextFinder));
		dummy.active     = finder->active;
		dummy.cur_pos    = finder->cur_pos;
		dummy.first      = finder->first;
		dummy.first_char = finder->first_char;
		dummy.last       = finder->last;
		dummy.last_char  = finder->last_char;
		dummy.nmatch     = finder->nmatch;
		dummy.flags      = finder->flags;
		dummy.ic         = finder->ic;
		dummy.direction  = finder->direction;

		/* free old pattern */
		regfree(&(finder->pattern));
		finder->have_pat = False;
		free(finder->to_find);
		finder->to_find = NULL;

		/* set new flags */
		finder->flags     = flags;
		finder->ic        = ignore_case;
		finder->direction = direction;

		/* try re-compiling the pattern with the new flags */
		ret_val = XmHTMLTextFinderSetPattern(finder, to_find);

		/* if failed, reset old flags & re-compile the pattern */
		if(!ret_val)
		{
			/* save error code */
			int last_err = finder->last_err;

			/* restore flags */
			finder->flags     = dummy.flags;
			finder->ic        = dummy.ic;
			finder->direction = dummy.direction;

			/* recompile pattern */
			(void)XmHTMLTextFinderSetPattern(finder, to_find); 

			/* and restore error code */
			finder->last_err = last_err;
		}

		/* restore previous finder state */
		finder->active     = dummy.active;
		finder->cur_pos    = dummy.cur_pos;
		finder->first      = dummy.first;
		finder->first_char = dummy.first_char;
		finder->last       = dummy.last;
		finder->last_char  = dummy.last_char;
		finder->nmatch     = dummy.nmatch;

		/* no longer needed */
		free(to_find);
	}
	else
	{
		if(flags != -1)
			finder->flags = flags;
		finder->ic        = ignore_case;
		finder->direction = direction;
	}
	return(ret_val);
}

/*****
* Name:			XmHTMLTextFinderGetError
* Return Type: 	int
* Description: 	returns error code of the last known regex error.
* In: 
*	finder:		current Finder Context;
* Returns:
*	error code of last known regex error.
*****/
int
XmHTMLTextFinderGetError(XmHTMLTextFinder finder)
{
	return(finder->last_err);
}

/*****
* Name:			XmHTMLTextFinderGetErrorString
* Return Type: 	String
* Description: 	returns a description of the the last known regex error.
* In: 
*	finder:		current Finder Context;
* Returns:
*	A newly allocated string that must be freed by the caller
*****/
String
XmHTMLTextFinderGetErrorString(XmHTMLTextFinder finder)
{
	String err_buf = NULL;
	int nchars = 0;

	/* see how many bytes we have to allocate */
	switch(finder->last_err)
	{
		case RE_EEMPTY:
			nchars = strlen(XMHTML_MSG_131);
			break;
		case RE_ENOMEM:
			nchars = strlen(XMHTML_MSG_132);
			break;
		case RE_EBADPARENT:
			nchars = strlen(XMHTML_MSG_133);
			break;
		case RE_EWORDS:
			nchars = strlen(XMHTML_MSG_134);
			break;
		case RE_ERROR:
			nchars = strlen(XMHTML_MSG_135);
			break;
		default:
			nchars = regerror(finder->last_err, &(finder->pattern), NULL, 0);
	}

	if(nchars != 0)
	{
		err_buf = (String)calloc(nchars+1, sizeof(char));

		switch(finder->last_err)
		{
			case RE_EEMPTY:
				strcpy(err_buf, XMHTML_MSG_131);
				break;
			case RE_ENOMEM:	
				strcpy(err_buf, XMHTML_MSG_132);
				break;
			case RE_EBADPARENT:
				strcpy(err_buf, XMHTML_MSG_133);
				break;
			case RE_EWORDS:
				strcpy(err_buf, XMHTML_MSG_134);
				break;
			case RE_ERROR:
				strcpy(err_buf, XMHTML_MSG_135);
				break;
			default:
				regerror(finder->last_err, &(finder->pattern), err_buf,
				nchars);
		}
	}
	return(err_buf);
}

/*****
* Name:			XmHTMLTextFinderReset
* Return Type: 	void
* Description: 	rewinds the finder to it's starting position and clears
*				the pattern buffer.
* In: 
*	finder:		finder to be reset;
* Returns:
*	nothing.
*****/
void
XmHTMLTextFinderReset(XmHTMLTextFinder finder)
{
	/* rewind to start of buffer */
	finder->cur_pos = 0;

	if(finder->have_pat)
	{
		regfree(&(finder->pattern));
		free(finder->to_find);
		finder->to_find = NULL;
	}

	finder->have_pat   = False;
	finder->active     = False;
	finder->cur_pos    = 0;
	finder->first      = -1;
	finder->first_char = -1;
	finder->last       = -1;
	finder->last_char  = -1;
	finder->nmatch     = -1;
}

/*****
* Name:			XmHTMLTextFind
* Return Type: 	XmHTMLRegexStatus
* Description: 	find the first appearance of the compiled pattern in the
*				current text.
* In: 
*	w:			XmHTMLWidget id;
*	finder:		current finder id;
* Returns:
*	XmREG_MATCH if a match was found, XmREG_NOMATCH if no match was found
*	and XmREG_ERROR on error (no pattern supplied or empty text buffer).
*****/
XmHTMLRegexStatus
XmHTMLTextFindString(Widget w, XmHTMLTextFinder finder)
{
	regmatch_t pmatch[1];
#ifdef DEBUG
	String buf;
#endif

	/* sanity */
	if(!finder->have_pat || finder->nwords == 0)
	{
		finder->last_err = (finder->have_pat ? RE_EEMPTY : RE_EWORDS);
		return(XmREG_ERROR);
	}

	/* Important sanity, we don't want to cause any overruns */
	if(finder->cur_pos >= finder->nchars)
	{
		finder->last_err = REG_NOMATCH;
		return(XmREG_NOMATCH);
	}

	/* we are active */
	finder->active = True;
	finder->last_err = REG_NOERROR;

	/* try to find a match */
	if((regexec(&(finder->pattern), finder->text + finder->cur_pos, 1,
		pmatch, 0)) == REG_NOMATCH)
	{
		finder->last_err = REG_NOMATCH;
		return(XmREG_NOMATCH);
	}

	/*****
	* Found a match, remember the offsets (we index the buffer in the
	* regexec call so we must add them here if we want these positions to
	* match).
	*****/
	finder->first_char = finder->cur_pos + pmatch[0].rm_so;
	finder->last_char  = finder->cur_pos + pmatch[0].rm_eo;
	finder->nmatch     = pmatch[0].rm_eo - pmatch[0].rm_so;

#ifdef DEBUG
	buf = (String)malloc(finder->nmatch+1);
	strncpy(buf, finder->text + finder->first_char, finder->nmatch);
	buf[finder->nmatch] = '\0';
	_XmHTMLDebug(18, ("textsel.c: XmHTMLTextFind, found a match at character "
		"position %i, %i chars long.\n", finder->first_char, finder->nmatch));
	_XmHTMLDebug(18, ("textsel.c: XmHTMLTextFind, match is: %s\n", buf));
	free(buf);
#endif

	/* position text buffer right after the end of the match */
	finder->cur_pos = finder->last_char + 1;

	return(XmREG_MATCH);
}

Boolean
XmHTMLTextFindToPosition(Widget w, XmHTMLTextFinder finder,
	XmHTMLTextPosition *start, XmHTMLTextPosition *end)
{
	int i, lc, nmatch = 0;

	if(!finder->active || !finder->have_pat || finder->last_err == REG_NOMATCH)
	{
		finder->last_err = (finder->have_pat ? REG_NOMATCH : RE_EEMPTY);
		return(False);
	}

	/* find first word that matches */
	for(i = 0; i < finder->nwords; i++)
	{
		if(finder->fast_tab[i].pos >= finder->first_char)
		{
			if(finder->fast_tab[i].pos > finder->first_char)
				i--;
			break;
		}
	}

	if(i == finder->nwords)
	{
		finder->last_err = REG_NOMATCH;
		return(False);
	}

	/* store start of match */
	start->start  = finder->fast_tab[i].owner;
	start->idx    = finder->fast_tab[i].word_idx;
	start->fc     = finder->cur_pos - finder->nmatch - finder->fast_tab[i].pos;
	start->nwords = start->start->n_words - start->idx;

	/* chars provided by this word */
	nmatch = finder->fast_tab[i].len - start->fc;

	while(nmatch < finder->nmatch && i < finder->nwords)
	{
		i++;
		nmatch += finder->fast_tab[i].len;
	}
	if(nmatch > finder->nmatch)
		i--;

	/* ends at last word */
	if(i == finder->nwords)
	{
		end = NULL;
		return(True);
	}

	/* store end of match */
	end->start  = finder->fast_tab[i].owner;
	end->idx    = finder->fast_tab[i].word_idx;
	end->nwords = end->idx;

	/* index of last char */
	end->fc     = finder->last_char - finder->fast_tab[i].pos;

	return(True);
}

