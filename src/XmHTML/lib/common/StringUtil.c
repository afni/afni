#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* StringUtil.c:  XmHTML string routines, including tag analyzers.
*
* This file Version	$Revision$
*
* Creation date:		Wed May 29 22:35:32 GMT+0100 1996
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
* (C)Copyright 1995 Ripley Software Development
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
* Source History:
* ForUtil-0.52
* newt
*****/
/*****
* ChangeLog
* $Log$
* Revision 1.1  2011/06/30 16:10:37  rwcox
* Cadd
*
* Revision 1.10  1998/04/27 06:55:44  newt
* A few speedups in strcasestr
*
* Revision 1.9  1998/04/04 06:27:48  newt
* XmHTML Beta 1.1.3
*
* Revision 1.8  1997/10/23 00:24:40  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.7  1997/08/30 00:28:24  newt
* HashTable routines added. Changed my_ prefix to __rsd_.
*
* Revision 1.6  1997/08/01 12:53:38  newt
* Modified debugging memory allocation routines to show file and line info.
*
* Revision 1.5  1997/05/28 01:31:26  newt
* Added debug versions of all memory allocation routines using assertions.
*
* Revision 1.4  1997/04/29 14:20:15  newt
* Prefixed all functions with my_ to prevent name conflicts with libwww3.
* Added my_strndup
*
* Revision 1.3  1997/03/02 23:04:39  newt
* changed unsigned char to Byte
*
* Revision 1.2  1997/02/11 02:04:09  newt
* Added strcasecmp/strncasecmp
*
* Revision 1.1  1997/01/09 06:54:51  newt
* expanded copyright marker
*
* Revision 2.0  1996/09/19 02:45:27  newt
* Updated for source revision 2.0
*
* Revision 1.1  1996/06/27 03:53:51  newt
* Initial Revision.
* Originally comes from ForUtil-0.52, but has been adapted for Newt.
*
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>	/* toupper, tolower, isspace */
#include <sys/types.h>

#include "toolkit.h"
#include XmHTMLPrivateHeader
#include "escapes.h"

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/***
* Character translation table for converting from upper to lower case
* Since this is a table lookup, it might perform better than the libc
* tolower routine on a number of systems.
***/
#ifndef I18N
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

/*****
* No else clause, when I18N is defined, _FastLower is redefined to tolower
* include/common/XmHTMLI.h
******/
#endif /* I18N */

/*** Private Function Prototype Declarations ****/
static String to_ascii(int val);
static String to_roman(int val);

/* convert a character escape sequence */
static char tokenToEscape(char **escape, Boolean warn);

/*** Private Variable Declarations ***/
static char *Ones[] =
		{"i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix"};
static char *Tens[] =
		{"x", "xx", "xxx", "xl", "l", "lx", "lxx", "lxxx", "xc"};
static char *Hundreds[] =
		{"c", "cc", "ccc", "cd", "d", "dc", "dcc", "dccc", "cm"};

/*****
* Name: 		my_upcase
* Return Type: 	void
* Description: 	makes a string all uppercase
* In:
*	string:		string to translate to uppercase
* Returns:
*	nothing, string is changed upon return.
*****/
void
my_upcase(char *string)
{
	register char *outPtr = string;
	for(outPtr = string; *outPtr != '\0';
		*(outPtr++) = toupper(*(string++)));
}

/*****
* Name: 		my_locase
* Return Type: 	void
* Description: 	make a string all lower case
* In:
*	string:		string to translate to lowercase
* Returns:
*	nothing, string is changed upon return.
*****/
void
my_locase(char *string)
{
	register char *outPtr = string;
	for(outPtr = string; *outPtr != '\0';
		*(outPtr++) = _FastLower(*(string++)));
}

/*****
* Name: 		my_strcasestr
* Return Type: 	char *
* Description: 	returns the starting address of s2 in s1, ignoring case
* In:
*	s1:			string to examine
*	s2:			string to find
* Returns:
*	a ptr to the position in s1 where s2 is found, or NULL if s2 is not found.
*****/
char *
my_strcasestr(const char *s1, const char *s2)
{
	register int i;
	register const char *p1, *p2, *s;
	char first = _FastLower(*s2);

	/* find the location of the first matching char of s2 in s1 */
	for(s = s1, i = 0; *s && _FastLower(*s) != first; i++, s++);

	if(!*s)
		return(0);

	/* the first char matches, check if the remainder also matches */
	for(p2 = s2; *s; p2 = s2, i++, s++)
	{
		for (p1 = s; *p1 && *p2 && _FastLower(*p1) == _FastLower(*p2);
				p1++, p2++);
		if(!*p2)
			break;
	}
	if (!*p2)
		return((char*)s1 + i);
	return 0;
}

#ifdef DEBUG
/*****
* Name: 		__rsd_strdup
* Return Type: 	char*
* Description: 	debugging version of strdup
* In:
*	s1:			string to be duplicated
* Returns:
*	duplicated string.
*****/
char*
__rsd_strdup(const char *s1, char *file, int line)
{
	static char *ret_val;

	/* dump if failed */
	my_assert(s1 != NULL);

	ret_val = malloc(strlen(s1)+1);
	strcpy(ret_val, s1);
	return(ret_val);
}
#endif

/*****
* Name: 		my_strndup
* Return Type: 	char*
* Description: 	duplicates up to len chars of string s1 and NULL terminates
*				it.
* In:
*	s1:			source string;
*	len:		max no of chars to copy;
* Returns:
*	a ptr to the duplicated string, padded with NULL if len is larger then
*	s1. Return value is always NULL terminated.
*****/
char *
my_strndup(const char *s1, size_t len)
{
	static char *ret;

	/* no negative lengths */
	if(len < 0 || s1 == NULL || *s1 == '\0')
		return(NULL);

	/* size of text + a terminating \0 */
	ret = (char*)malloc(len+1);

	ret = (char*)memcpy(ret, s1, len);
	ret[len] = '\0';

	return(ret);
}

/*****
* UnixWare doesn't have these functions in its standard C library
* contributed by Thanh Ma (tma@encore.com), fix 02/03/97-03, tma
*****/

#ifdef NEED_STRCASECMP
/*****
* Name: 		strncasecmp
* Return Type: 	int
* Description: 	case insensitive string compare upto n characters of string
*				s1.
* In:
*	s1:			source string
*	s2:			string to compare with
*	n:			no of characters to compare.
* Returns:
*	0 when they match, character difference otherwise.
*****/
int
my_strncasecmp (const char *s1, const char *s2, size_t n)
{
	register int c1, c2, l=0;

	while (*s1 && *s2 && l < n)
	{
		c1 = _FastLower(*s1);
		c2 = _FastLower(*s2);
		if (c1 != c2)
			return(c1 - c2);
		s1++;
		s2++;
		l++;
	}
	return((int)(0));
}

/*****
* Name: 		strcasecmp
* Return Type: 	int
* Description: 	case insensitive string compare
* In:
*	s1:			source string
*	s2:			string to compare with
* Returns:
*	0 when they match, character difference otherwise.
*****/
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
#endif /* NEED_STRCASECMP */

/*****
* string to number routines
*****/

/*****
* Name: 		to_ascii
* Return Type: 	String
* Description: 	converts a numerical value to an abc representation.
* In:
*	val:		number to convert
* Returns:
*	converted number.
*****/
static String
to_ascii(int val)
{
	int remainder, i = 0, j = 0, value = val;
	char number[32];
	static char out[32];	/* return buffer */

	do
	{
		remainder = (value % 26);
		number[i++] = (remainder ? remainder + 96 : 'z');
	}
	while((value = (remainder ? (int)(value/26) : (int)((value-1)/26)))
		&& i < 32); /* no buffer overruns */

	for(j = 0; i > 0 && j < 32; i--, j++)
		out[j] = number[i-1];

	out[j] = '\0';	/* NULL terminate */

	return(out);
}

/*****
* Name: 		to_roman
* Return Type: 	String
* Description: 	converts the given number to a lowercase roman numeral.
* In:
*	val:		number to convert
* Returns:
*	converted number
* Note:
*	This routine is based on a similar one found in the Arena browser.
*****/
static String
to_roman(int val)
{
	int value, one, ten, hundred, thousand;
	static char buf[48], *p, *q;

	value = val;
	sprintf(buf, "%i", val);

	thousand = value/1000;
	value = value % 1000;
	hundred = value/100;
	value = value % 100;
	ten = value/10;
	one = value % 10;

	p = buf;
	while(thousand-- > 0)
		*p++ = 'm';

	if(hundred)
	{
		q = Hundreds[hundred-1];
		while ((*p++ = *q++));
		--p;
	}
	if(ten)
	{
		q = Tens[ten-1];
		while ((*p++ = *q++));
		--p;
	}
	if(one)
	{
		q = Ones[one-1];
		while ((*p++ = *q++));
		--p;
	}
	*p = '\0';

	return(buf);
}

/*****
* Name: 		ToAsciiLower
* Return Type: 	String
* Description: 	returns the abc representation of the given number
* In:
*	val:		number to convert
* Returns:
*	converted number
*****/
String
ToAsciiLower(int val)
{
	return((to_ascii(val)));
}

/*****
* Name: 		ToAsciiUpper
* Return Type: 	String
* Description: 	returns the ABC representation of the given number
* In:
*	val:		number to convert
* Returns:
*	converted number
*****/
String
ToAsciiUpper(int val)
{
	static String buf;
	buf = to_ascii(val);
	my_upcase(buf);
	return(buf);
}

/*****
* Name: 		ToRomanLower
* Return Type: 	String
* Description: 	converts numbers between 1-3999 to roman numerals, lowercase.
* In:
*	value:		value to convert
* Returns:
*	lowercase roman numeral
*****/
String
ToRomanLower(int val)
{
	return(to_roman(val));
}

/*****
* Name: 		ToRomanUpper
* Return Type: 	String
* Description: 	converts numbers between 1-3999 to roman numerals, uppercase.
* In:
*	value:		value to convert
* Returns:
*	uppercase roman numeral
*****/
String
ToRomanUpper(int val)
{
	static String buf;
	buf = to_roman(val);
	my_upcase(buf);
	return(buf);
}

/*****
* Name: 		stringToToken
* Return Type: 	Byte
* Description: 	converts a string to a numeric id.
* In:
*	token:		string to be converted;
*	tokens:		array of valid strings, alphabetically sorted;
*	max_val:	size of tokens array.
* Returns:
*	the id of the token if it appears in the tokens array, max_val otherwise.
*****/
Byte
stringToToken(String token, String *tokens, int max_val)
{
	register Byte mid, lo = 0, hi = (int)max_val - 1;
	int cmp;

	/* comparisons are done caseless */
	my_locase(token);

	/* binary search the array of tokens */
	while(lo <= hi)
	{
		mid = (lo + hi)/2;
		if((cmp = strcmp(token, tokens[mid])) == 0)
			return(mid);
		else
			if(cmp < 0)			/* in lower end of array */
				hi = mid - 1;
			else				/* in higher end of array */
				lo = mid + 1;
	}
	/* not found, return max_val */
	return((Byte)max_val);
}

/*****
* HTML Tag analyzers
*****/

/*****
* Name: 		tokenToEscape
* Return Type: 	char
* Description: 	converts the HTML & escapes sequences to the appropriate char.
* In:
*	**escape:	escape sequence to convert. This argument is updated upon
*				return.
*	warn:		warning issue flag;
* Returns:
*	the character representation of the given escape sequence
*
* Note: this routine uses a sorted table defined in the header file escapes.h
*	and uses a binary search to locate the appropriate character for the given
*	escape sequence.
*	This table contains the hashed escapes as well as the named escapes.
*	The number of elements is NUM_ESCAPES (currently 197), so a match is always
*	found in less than 8 iterations (2^8=256).
*	If an escape sequence is not matched and it is a hash escape, the value
*	is assumed to be below 160 and converted to a char using the ASCII
*	representation of the given number. For other, non-matched characters, 0
*	is returned and the return pointer is updated to point right after the
*	ampersand sign.
*****/
static char
tokenToEscape(char **escape, Boolean warn)
{
	register int mid, lo = 0, hi = NUM_ESCAPES -1;
	int cmp, skip = 1;
	char tmp[8];

	/*
	* first check if this is indeed an escape sequence.
	* It's much more cost-effective to do this test here instead of in
	* the calling routine.
	*/
	if(*(*escape+1) != '#' && !(isalpha(*(*escape+1))))
	{
		if(warn)
		{
			/* bad escape, spit out a warning and continue */
			strncpy(tmp, *escape, 7);
			tmp[7] = '\0';
			_XmHTMLWarning(__WFUNC__(NULL, "tokenToEscape"), XMHTML_MSG_3, tmp);
		}
		/* skip and return */
		*escape += 1;
		return('&');
	}
	/*
	* run this loop twice: one time with a ; assumed present and one
	* time without ; present.
	*/
	for(skip = 0; skip != 2; skip++)
	{
		lo = 0;
		hi = NUM_ESCAPES - 1;
		while(lo <= hi)
		{
			mid = (lo + hi)/2;
			if((cmp = strncmp(*escape+1, escapes[mid].escape,
				escapes[mid].len - skip)) == 0)
			{
				/* update ptr to point right after the escape sequence */
				*escape += escapes[mid].len + (1 - skip);
				return(escapes[mid].token);
			}
			else
				if(cmp < 0)				/* in lower end of array */
					hi = mid - 1;
				else					/* in higher end of array */
					lo = mid + 1;
		}
	}

	/*
	* If we get here, the escape sequence wasn't matched: big chance
	* it uses a &# escape below 160. To deal with this, we pick up the numeric
	* code and convert to a plain ASCII value which is returned to the
	* caller
	*/
	if( *(*escape+1) == '#')
	{
		char *chPtr, ret_char;
		int len = 0;

		*escape += 2;	/* skip past the &# sequence */
		chPtr = *escape;
		while(isdigit(*chPtr))
		{
			chPtr++;
			len++;
		}
		if(*chPtr == ';')
		{
			*chPtr = '\0';	/* null out the ; */
			len++;
		}
		ret_char = (char)atoi(*escape);	/* get corresponding char */
		/* move past the escape sequence */
		if(*(*escape + len) == ';')
			*escape += len + 1;
		else
			*escape += len;
		return(ret_char);
	}

	/* bad escape, spit out a warning and continue */
	if(warn)
	{
		strncpy(tmp, *escape, 7);
		tmp[7] = '\0';
		_XmHTMLWarning(__WFUNC__(NULL, "tokenToEscape"), XMHTML_MSG_3, tmp);
	}
	*escape += 1;
	return('&');
}

/*****
* Name: 		_XmHTMLExpandEscapes
* Return Type: 	void
* Description: 	replaces character escapes sequences with the appropriate char.
* In:
*	string:		text to scan for escape sequences
* Returns:
*	nothing
*****/
void
_XmHTMLExpandEscapes(char *string, Boolean warn)
{
	register char *chPtr = string;
	char escape;	/* value of escape character */

	/* scan the entire text in search of escape codes (yuck) */
	while(*string)	/* fix 02/26/97-02, dp */
	{
		switch(*string)
		{
			case '&':
				if((escape = tokenToEscape(&string, warn)) != 0)
					*chPtr++ = escape;
				break;
			default:
				*(chPtr++) = *(string++);
		}
		if(*string == 0)
		{
			*chPtr = '\0';	/* NULL terminate */
			return;
		}
	}
}

/*****
* Name: 		_XmHTMLTagCheck
* Return Type: 	Boolean
* Description: 	checks whether the given tag exists in the attributes of a
*				HTML element
* In:
*	attributes:	attributes from an HTML element
*	tag:		tag to look for.
* Returns:
*	True if tag is found, False otherwise.
*****/
Boolean
_XmHTMLTagCheck(char *attributes, char *tag)
{
	char *chPtr, *start;

	/* sanity check */
	if(attributes == NULL)
		return(False);

	if((chPtr = strstr(attributes, tag)) != NULL)
	{
		/* see if this is a valid tag: it must be preceeded with whitespace. */
		while(*(chPtr-1) && !isspace(*(chPtr-1)))
		{
			start = chPtr+strlen(tag); /* start right after this element */
			if((chPtr = strstr(start, tag)) == NULL)
				return(False);
		}
		if(chPtr)
			return(True);
		else
			return(False);
	}
	return(False);
}

/*****
* Name: 		_XmHTMLTagGetValue
* Return Type: 	char *
* Description: 	looks for the specified tag in the given list of attributes.
* In:
*	attributes:	attributes from an HTML element
*	tag:		tag to look for.
* Returns:
*	if tag exists, the value of this tag, NULL otherwise.
*	return value is always malloc'd; caller must free it.
*****/
String
_XmHTMLTagGetValue(char *attributes, char *tag)
{
	static char *buf;
	char *chPtr, *start, *end;

	if(attributes == NULL || tag == NULL)	/* sanity check */
		return(NULL);

	_XmHTMLDebug(4, ("StringUtil.c: _XmHTMLTagGetValue, attributes: %s, "
		"tag %s\n", attributes, tag));

	if((chPtr = strstr(attributes, tag)) != NULL)
	{
		/*****
		* check if the ptr obtained is correct, eg, no whitespace before it.
		* If this is not the case, get the next match.
		* Need to do this since a single strstr on, for example, align
		* will match both align _and_ valign.
		*
		* fix 02/06/98-02, eb
		* We need to verify that chPtr-1 is valid, if we have a match
		* at the start of the attributes ("foo=bar", "foo") for instance,
		* chPtr-1 is invalid.
		*****/
		while(chPtr > attributes && *(chPtr-1) && !isspace(*(chPtr-1)))
		{
			start = chPtr+strlen(tag); /* start right after this element */
			if((chPtr = strstr(start, tag)) == NULL)
				return(NULL);
		}
		if(chPtr == NULL)
			return(NULL);

		start = chPtr+strlen(tag); /* start right after this element */
		/* remove leading spaces */
		while(isspace(*start))
			start++;

		/* if no '=', return NULL */
		if(*start != '=')
		{
			_XmHTMLDebug(4, ("StringUtil.c: _XmHTMLTagGetValue, tag has no "
				"= sign.\n"));
			return(NULL);
		}

		start++;	/* move past the '=' char */

		/* remove more spaces */
		while(*start != '\0' && isspace(*start))
			start++;

		/* sanity check */
		if(*start == '\0')
		{
#ifdef PEDANTIC
			_XmHTMLWarning(__WFUNC__(NULL, "_XmHTMLTagGetValue"),
				XMHTML_MSG_4, tag);
#endif /* PEDANTIC */
			return(NULL);
		}

		/* unquoted tag values are treated differently */
		if(*start != '\"')
		{
			for(end = start; !(isspace(*end)) && *end != '\0' ; end++);
		}
		else
		{
			start++;
			for(end = start; *end != '\"' && *end != '\0' ; end++);
		}
		/* empty string */
		if(end == start)
			return(NULL);

		buf = my_strndup(start, end - start);

		_XmHTMLDebug(4, ("StringUtil.c: _XmHTMLTagGetValue, returning %s\n",
			buf));

		return(buf);
	}
	return(NULL);
}

/*****
* Name: 		_XmHTMLTagGetNumber
* Return Type: 	int
* Description: 	retrieves the numerical value of the given tag.
* In:
*	attributes:	attributes from an HTML element
*	tag:		tag to look for.
*	def:		default value if tag is not found.
* Returns:
*	if tag exists, the value of this tag, def otherwise
*****/
int
_XmHTMLTagGetNumber(char *attributes, char *tag, int def)
{
	char *chPtr;
	int ret_val = def;

	if((chPtr = _XmHTMLTagGetValue(attributes, tag)) != NULL)
	{
		ret_val = atoi(chPtr);
		_XmHTMLDebug(4, ("StringUtil.c: _XmHTMLTagGetNumber, value for tag %s "
			"is %i\n", tag, ret_val));
		free(chPtr);
	}
	return(ret_val);
}

/*****
* Name:			_XmHTMLTagCheckNumber
* Return Type: 	int
* Description: 	retrieves the numerical value of the given tag.
*				If the returned no is negative, the specified value was
*				a relative number. Otherwise it's an absolute number.
* In:
*	attributes:	attributes from an HTML element
*	tag:		tag to look for.
*	def:		default value if tag is not found.
* Returns:
*	if tag exists, the value of this tag, def otherwise
*****/
int
_XmHTMLTagCheckNumber(char *attributes, char *tag, int def)
{
	int ret_val = def;
	char *chPtr;

	/* get the requested tag */
	if((chPtr = _XmHTMLTagGetValue(attributes, tag)) != NULL)
	{
		/* when len is negative, a percentage has been used */
		if((strpbrk(chPtr, "%")) != NULL ||
			(strpbrk(chPtr, "*")) != NULL)
			ret_val = -1*atoi(chPtr);
		else
			ret_val = atoi(chPtr);
		free(chPtr);
	}
	return(ret_val);
}

/*****
* Name: 		_XmHTMLTagCheckValue
* Return Type: 	Boolean
* Description: 	checks whether the specified tag in the given list of attributes
*				has a certain value.
* In:
*	attributes:	attributes from an HTML element
*	tag:		tag to look for.
*	check:		value to check.
* Returns:
*	returns True if tag exists and has the correct value, False otherwise.
*****/
Boolean
_XmHTMLTagCheckValue(char *attributes, char *tag, char *check)
{
	char *buf;

	_XmHTMLDebug(4, ("StringUtil.c: _XmHTMLTagCheckValue: tag %s, check %s\n",
		tag, check));

	/* no sanity check, TagGetValue returns NULL if attributes is empty */

	if((buf = _XmHTMLTagGetValue(attributes, tag)) == NULL ||
		strcasecmp(buf, check))
	{
		if(buf != NULL)
			free(buf);
		return(False);
	}
	free(buf);		/* fix 12-21-96-01, kdh */
	return(True);
}

/*****
* Name: 		_XmHTMLGetImageAlignment
* Return Type: 	Alignment
* Description: 	returns any specified image alignment
* In:
*	attributes:	<IMG> attributes
* Returns:
*	specified image alignment. If none found, XmVALIGN_BOTTOM
*****/
Alignment
_XmHTMLGetImageAlignment(char *attributes)
{
	char *buf;
	Alignment ret_val = XmVALIGN_BOTTOM;

	/* First check if this tag does exist */
	if((buf = _XmHTMLTagGetValue(attributes, "align")) == NULL)
		return(ret_val);

	/* transform to lowercase */
	my_locase(buf);

	if(!(strcmp(buf, "left")))
		ret_val = XmHALIGN_LEFT;
	else if(!(strcmp(buf, "right")))
		ret_val = XmHALIGN_RIGHT;
	else if(!(strcmp(buf, "top")))
		ret_val = XmVALIGN_TOP;
	else if(!(strcmp(buf, "middle")))
		ret_val = XmVALIGN_MIDDLE;
	else if(!(strcmp(buf, "bottom")))
		ret_val = XmVALIGN_BOTTOM;
	else if(!(strcmp(buf, "baseline")))
		ret_val = XmVALIGN_BASELINE;

	free(buf);	/* fix 01/12/97-01; kdh */
	return(ret_val);
}

/*****
* Name: 		_XmHTMLGetHorizontalAlignment
* Return Type: 	Alignment
* Description:	Retrieve the value of the ALIGN attribute
* In:
*	attributes:	attributes to check for the ALIGN tag
*	def_align:	default alignment.
* Returns:
*	selected ALIGN enumeration type or def_align if no match is found.
*****/
Alignment
_XmHTMLGetHorizontalAlignment(char *attributes, Alignment def_align)
{
	char *buf;
	Alignment ret_val = def_align;

	/* First check if this tag does exist */
	if((buf = _XmHTMLTagGetValue(attributes, "align")) == NULL)
		return(ret_val);

	/* transform to lowercase */
	my_locase(buf);

	if(!(strcmp(buf, "center")))
		ret_val = XmHALIGN_CENTER;
	else if(!(strcmp(buf, "right")))
		ret_val = XmHALIGN_RIGHT;
	else if(!(strcmp(buf, "justify")))
		ret_val = XmHALIGN_JUSTIFY;
	else if(!(strcmp(buf, "left")))
		ret_val = XmHALIGN_LEFT;

	free(buf);	/* fix 01/12/97-01; kdh */
	return(ret_val);
}

/*****
* Name: 		_XmHTMLGetVerticalAlignment
* Return Type: 	Alignment
* Description:	Retrieve the value of the VALIGN attribute
* In:
*	attributes:	attributes to check for the VALIGN tag
* Returns:
*	selected VALIGN enumeration type or XmVALIGN_TOP when no valign tag
*	is found among the element's attributes.
*****/
Alignment
_XmHTMLGetVerticalAlignment(char *attributes, Alignment def_align)
{
	char *buf;
	Alignment ret_val = def_align;

	/* First check if this tag does exist */
	if((buf = _XmHTMLTagGetValue(attributes, "valign")) == NULL)
		return(ret_val);

	if(!(strcmp(buf, "top")))
		ret_val = XmVALIGN_TOP;
	else if(!(strcmp(buf, "middle")))
		ret_val = XmVALIGN_MIDDLE;
	else if(!(strcmp(buf, "bottom")))
		ret_val = XmVALIGN_BOTTOM;
	else if(!(strcmp(buf, "baseline")))
		ret_val = XmVALIGN_BASELINE;

	free(buf);		/* fix 01/12/97-02; kdh */
	return(ret_val);
}

TableFraming
_XmHTMLGetFraming(String attributes, TableFraming def)
{
	char *buf;
	TableFraming ret_val = def;

	/* First check if this tag does exist */
	if((buf = _XmHTMLTagGetValue(attributes, "frame")) == NULL)
		return(ret_val);

	if(!(strcmp(buf, "void")))
		ret_val = TFRAME_VOID;
	else if(!(strcmp(buf, "above")))
		ret_val = TFRAME_ABOVE;
	else if(!(strcmp(buf, "below")))
		ret_val = TFRAME_BELOW;
	else if(!(strcmp(buf, "hsides")))
		ret_val = TFRAME_HSIDES;
	else if(!(strcmp(buf, "lhs")))
		ret_val = TFRAME_LEFT;
	else if(!(strcmp(buf, "rhs")))
		ret_val = TFRAME_RIGHT;
	else if(!(strcmp(buf, "vsides")))
		ret_val = TFRAME_VSIDES;
	else if(!(strcmp(buf, "box")))
		ret_val = TFRAME_BOX;
	else if(!(strcmp(buf, "border")))
		ret_val = TFRAME_BORDER;

	free(buf);
	return(ret_val);
}

TableRuling
_XmHTMLGetRuling(String attributes, TableRuling def)
{
	char *buf;
	TableRuling ret_val = def;

	/* First check if this tag does exist */
	if((buf = _XmHTMLTagGetValue(attributes, "rules")) == NULL)
		return(ret_val);

	if(!(strcmp(buf, "none")))
		ret_val = TRULE_NONE;
	else if(!(strcmp(buf, "groups")))
		ret_val = TRULE_GROUPS;
	else if(!(strcmp(buf, "rows")))
		ret_val = TRULE_ROWS;
	else if(!(strcmp(buf, "cols")))
		ret_val = TRULE_COLS;
	else if(!(strcmp(buf, "all")))
		ret_val = TRULE_ALL;

	free(buf);
	return(ret_val);
}

/*****
* Name: 		_XmHTMLGetMaxLineLength
* Return Type: 	Dimension
* Description: 	returns an estimated guess on how wide the formatted document
*				will be based on the longest line in the document source.
* In:
*	html:		XmHTMLWidget id
* Returns:
*	guess what?
*****/
Dimension
_XmHTMLGetMaxLineLength(XmHTMLWidget html)
{
	Dimension max = 0, ret_val = 0;
	int i;
	String chPtr;
	XmHTMLObject *tmp;
	ToolkitAbstraction *tka = HTML_ATTR(tka);

	for(tmp = HTML_ATTR(elements); tmp != NULL; tmp = tmp->next)
	{
		if(tmp->id == HT_ZTEXT)
		{
			chPtr = tmp->element;
			i = 0;

			/*****
			* Count all chars. Tabs are expanded and newlines reset the
			* linewidth.
			*****/
			for(chPtr = tmp->element; *chPtr != '\0'; chPtr++)
			{
				switch(*chPtr)
				{
					case '\t':
						i = ((i/8)+1)*8;
						break;
					case '\n':
						if(ret_val < i)
							ret_val = i;
						i = -1;
					default:
						i++;
				}
			}
			/* long lines without a newline */
			if(ret_val < i)
				ret_val = i;
		}
	}

	/* assume 80 chars if no text found */
	if(ret_val == 0) ret_val = 80;

	/* assume an average width of 7 pixels per character */
	ret_val *= 7;

	/* we allow widths up to 75% of the screen width */
	max = (Dimension)(0.75*tka->width);

	ret_val = (ret_val > max ? max : ret_val);

	_XmHTMLDebug(4, ("StringUtil.c: _XmHTMLGetMaxLineLength, returning %d\n",
		ret_val));

	return(ret_val);
}

/*****
* Debugging memory functions.
* These *must* be kept at the end of this file as it overrides any
* previously defined memory allocation macros.
*
* Always included *unless* NDEBUG has been defined during compilation.
*****/

#ifndef NDEBUG

/* need to undefine them or we'll get in an endless loop */
#undef malloc
#undef calloc
#undef realloc
#undef free

char*
__rsd_malloc(size_t size, char *file, int line)
{
	static char *ret_val;

	my_assert(size != 0);

	ret_val = (char*) malloc (size);

	/* dump if failed */
	my_assert(ret_val != NULL);

	return(ret_val);
}

char*
__rsd_calloc(size_t nmemb, size_t size, char *file, int line)
{
	static char *ret_val;

	my_assert(nmemb != 0);

	ret_val = (char*) calloc (nmemb, size);

	/* dump if failed */
	my_assert(ret_val != NULL);

	return(ret_val);
}

char*
__rsd_realloc(void *ptr, size_t size, char *file, int line)
{
	static char *ret_val;

	if(size == 0)
	{
		my_assert(ptr != NULL);
		free (ptr);
		return(NULL);
	}
	ret_val = (char*) realloc (ptr, size);

	/* dump if failed */
	my_assert(ret_val != NULL);

	return(ret_val);
}

void
__rsd_free(void *ptr, char *file, int line)
{
	my_assert(ptr != NULL);
	free (ptr);
}

#endif /* !NDEBUG */

