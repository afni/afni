#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* output.c : XmHTML Text output routines.
*
* This file Version	$Revision$
*
* Creation date:		Sun Sep  7 18:53:00 GMT+0100 1997
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
*
* Copyright (C) 1994-1997 by Ripley Software Development 
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
* Revision 1.3  1998/04/27 07:01:33  newt
* tka stuff
*
* Revision 1.2  1998/04/04 06:28:16  newt
* XmHTML Beta 1.1.3
*
* Revision 1.1  1997/10/23 00:23:19  newt
* Initial Revision
*
*****/ 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Local includes */
#include "toolkit.h"
#include XmHTMLPrivateHeader

#include <X11/xpm.h>

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/

/*** Private Variable Declarations ***/

/*****
* Name:			_XmHTMLTextCheckAndConvertPaperDef
* Return Type:	XmHTMLPaperSize
* Description:	verifies the given paper definition and converts it's type
*				to XmHTML_POINT.
* In:
*	html:		XmHTMLWidget id;
*	pin:		papersize to be checked & converted;
* Returns:
*	a ptr to the checked & converted paperdefinition.
*	Must be freed by caller.
*****/
XmHTMLPaperSize*
_XmHTMLTextCheckAndConvertPaperDef(XmHTMLWidget html, XmHTMLPaperSize *pdef,
	Byte type)
{
	static XmHTMLPaperSize *pout;
	float multiplier = 1.;

	/* verify margins */
	if(pdef->left_margin + pdef->right_margin >= pdef->width)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLTextCheckAndConvertPaperDef"),
			XMHTML_MSG_86, "horizontal", "width");
		return(NULL);
	}

	if(pdef->top_margin + pdef->bottom_margin >= pdef->height)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLTextCheckAndConvertPaperDef"),
			XMHTML_MSG_86, "vertical", "height");
		return(NULL);
	}

	/* copy in to out */
	pout = (XmHTMLPaperSize*)malloc(sizeof(XmHTMLPaperSize));

	memcpy((void*)pout, (const void*)pdef, sizeof(XmHTMLPaperSize));

	if(type == XmHTMLTEXT_POSTSCRIPT)
	{
		/* postscript does everything in points */
		pout->unit_type = XmHTML_POINT;
		switch(pdef->unit_type)
		{
			case XmHTML_CENTIMETER:		/* 1cm = 28.45pt */
				multiplier = 28.45;
				break;
			case XmHTML_MILLIMETER:		/* 1mm = 2.845 pt */
				multiplier = 2.845;
				break;
			case XmHTML_INCH:			/* 1in = 72.27 pt */
				multiplier = 72.27;
				break;
			case XmHTML_PICA:			/* 1pc = 12pt */
				multiplier = 12.;
				break;
			case XmHTML_CHAR:			/* 1char = 10.5625pt */
				multiplier = 10.5625;
				break;
			case XmHTML_POINT:			/* no conversion required */
				return(pout);
			default:					/* bad spec */
				_XmHTMLWarning(__WFUNC__(html,
					"_XmHTMLTextCheckAndConvertPaperDef"), XMHTML_MSG_87);
				free(pout);
				return(NULL);
		}
	}
	else	/* plain text does everything in chars */
	{
		pout->unit_type = XmHTML_CHAR;
		switch(pdef->unit_type)
		{
			case XmHTML_CENTIMETER:		/* 1cm = 2.6934911 char */
				multiplier = 2.6934911;
				break;
			case XmHTML_MILLIMETER:		/* 1mm = 0.26934911 char */
				multiplier = 0.26934911;
				break;
			case XmHTML_INCH:			/* 1in = 6.8421302 char */
				multiplier = 6.8421302;
				break;
			case XmHTML_PICA:			/* 1pc = 1.1360947 char */
				multiplier = 1.1360947;
				break;
			case XmHTML_POINT:			/* 1pt = 0.094674556 char */
				multiplier = 0.094674556;
				break;
			case XmHTML_CHAR:			/* no conversion required */
				return(pout);
			default:					/* bad spec */
				_XmHTMLWarning(__WFUNC__(html,
					"_XmHTMLTextCheckAndConvertPaperDef"), XMHTML_MSG_87);
				free(pout);
				return(NULL);
		}
	}
	pout->width         *= multiplier;
	pout->height        *= multiplier;
	pout->left_margin   *= multiplier;
	pout->right_margin  *= multiplier;
	pout->top_margin    *= multiplier;
	pout->bottom_margin *= multiplier;

	return(pout);
}

/*****
* Name:			_XmHTMLTextGetPlain
* Return Type: 	String
* Description: 	converts text between start & end into a plain ASCII document.
* In: 
*	html:		XmHTMLWidget id;
*	pdef:		papersize definition. unittype must be XmHTML_CHAR;
*	start:		start object;
*	end:		end object;
*	options:	unused;
* Returns:
*	a String with plain ASCII content. Must be freed by caller.
*****/
String
_XmHTMLTextGetPlain(XmHTMLWidget html, XmHTMLPaperSize *pdef, 
	XmHTMLObjectTableElement start, XmHTMLObjectTableElement end,
	Byte options)
{
	int x_pos, y_pos, maxlen, width;
	XmHTMLObjectTable *elePtr;
	XmHTMLWord *words;
	String text = NULL, chPtr;
	int nchars = 0, n_words;
	int i, j, k, l, nused = 0;

	if(pdef->unit_type != XmHTML_CHAR)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLTextGetPlain"),
			XMHTML_MSG_88, "CHAR");
		return(NULL);
	}

	/* default start points */
	x_pos = pdef->left_margin;
	y_pos = pdef->top_margin;
	maxlen = pdef->width - pdef->left_margin;
	width = x_pos;

	/*****
	* Count how many words we have and compose a text buffer containing
	* all text. We ignore words of type IMG and FORM.
	*****/
	nchars = x_pos;		/* initial spacing */
	for(elePtr = start; elePtr != end; elePtr = elePtr->next)
	{
		switch(elePtr->object_type)
		{
			case OBJ_TEXT:
				n_words = elePtr->n_words;
				words = elePtr->words;
				for(i = 0; i < n_words; i++)
				{
					if(words[i].type == OBJ_TEXT || 
						words[i].type == OBJ_BLOCK)
					{
						if(words[i].type == OBJ_BLOCK)
						{
							nchars++;
							width = x_pos;
							nchars += x_pos;			/* lead spacing */
						}
						else
						{
							/* check linebreak */
							if(width + words[i].len > maxlen)
							{
								width = x_pos;
								nchars++;
								nchars += x_pos;			/* lead spacing */
							}
							if(!(words[i].spacing & TEXT_SPACE_TRAIL) && 
								i+1 < n_words &&
								!(words[i+1].spacing & TEXT_SPACE_LEAD))
							{
								k = i+1;
								while(k < n_words)
								{
									if(!(words[k].spacing & TEXT_SPACE_LEAD))
									{
										/* copy word */
										width += words[k].len;
										nchars += words[k].len;
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
								/* copy word */
								width += words[i].len;
								nchars += words[i].len;
							}
							/* add a space */
							width++;
							nchars++;
						}
					}
					else
					{
						/* images & form elements are considered as a space */
						width++;
						nchars++;
					}
				}
				break;

			case OBJ_PRE_TEXT:

				nchars++;					/* newline */
				width = x_pos;
				nchars += x_pos;			/* lead spacing */

				/* add all words */
				n_words = elePtr->n_words;
				words = elePtr->words;
				for(i = 0; i < n_words; i++)
				{
					if(words[i].type == OBJ_TEXT)
					{
						/* copy word */
						width += words[i].len;
						nchars += words[i].len;
						if(words[i].spacing != 0)
						{
							/* add newlines + lead spacing */
							nchars += words[i].spacing + x_pos;
							width = x_pos;
						}
					}
					else
					{
						/* images & form elements are considered as a space */
						width++;
						nchars++;
					}
				}
				break;
			default: /* non-text objects are converted to newlines */
				nchars++;					/* newline */
				width = x_pos;
				nchars += x_pos;			/* lead spacing */
		}
	}
	nchars++;	/* for terminator */

#ifdef DEBUG
	_XmHTMLDebug(18, ("text buffer creation, will use %ibytes\n.", nchars));
#endif

	if((text = (String)malloc(nchars * sizeof(char))) == NULL)
		return(NULL);

	/* okay, got it. Now fill the searchable table */
	j = 0;
	chPtr = text;
	width = x_pos;
	nused = 0;

	/* initial spacing */
	for(l = 0; l < x_pos; l++)
	{
		*chPtr++ = ' ';
		nused++;
	}

	for(elePtr = start; elePtr != end; elePtr = elePtr->next)
	{
		switch(elePtr->object_type)
		{
			case OBJ_TEXT:
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
							nused++;
							width = x_pos;
							for(l = 0; l < x_pos; l++)	/* lead spacing */
							{
								*chPtr++ = ' ';
								nused++;
							}
						}
						else
						{
							/* check linebreak */
							if(width + words[i].len > maxlen)
							{
								*chPtr++ = '\n';
								width = x_pos;
								nused++;
								for(l = 0; l < x_pos; l++)	/* lead spacing */
								{
									*chPtr++ = ' ';
									nused++;
								}
							}
							if(!(words[i].spacing & TEXT_SPACE_TRAIL) && 
								i+1 < n_words &&
								!(words[i+1].spacing & TEXT_SPACE_LEAD))
							{
								k = i+1;
								while(k < n_words)
								{
									if(!(words[k].spacing & TEXT_SPACE_LEAD))
									{
										/* copy word */
										memcpy(chPtr, words[k].word,
											words[k].len);
										chPtr += words[k].len;
										width += words[k].len;
										nused += words[k].len;
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
								/* copy word */
								memcpy(chPtr, words[i].word, words[i].len);
								chPtr += words[i].len;
								width += words[i].len;
								nused += words[i].len;
							}
							/* add a space */
							*chPtr++ = ' ';
							width++;
							nused++;
						}
					}
					else
					{
						/* images & form elements are considered as a space */
						*chPtr++ = ' ';
						width++;
						nused++;
					}
				}
				break;

			case OBJ_PRE_TEXT:

				*chPtr++ = '\n';					/* newline */
				width = x_pos;
				nused++;

				for(l = 0; l < x_pos; l++)	/* lead spacing */
				{
					*chPtr++ = ' ';
					nused++;
				}

				/* add all words */
				n_words = elePtr->n_words;
				words = elePtr->words;
				for(i = 0; i < n_words; i++)
				{
					if(words[i].type == OBJ_TEXT)
					{
						/* copy word */
						memcpy(chPtr, words[i].word, words[i].len);
						chPtr += words[i].len;
						width += words[i].len;
						nused += words[i].len;
						if(words[i].spacing != 0)
						{
							/* add newlines */
							for(l = 0; l < words[i].spacing; l++)
								*chPtr++ = '\n';
							nused += words[i].spacing;

							/* lead spacing */
							for(l = 0; l < x_pos; l++)
							{
								*chPtr++ = ' ';
								nused++;
							}
							width = x_pos;
						}
					}
					else
					{
						/* images & form elements are considered as a space */
						*chPtr++ = ' ';
						width++;
						nused++;
					}
				}
				break;
			default: /* non-text objects are converted to newlines */
				*chPtr++ = '\n';
				width = x_pos;
				nused++;
				for(l = 0; l < x_pos; l++)	/* lead spacing */
				{
					*chPtr++ = ' ';
					nused++;
				}
		}
	}
fprintf(stderr, "Used %i characters out of %i maximum\n", nused, nchars);
	/* terminate text */
	*chPtr++ = '\0';
	return(text);
}

/*****
* Name:			_XmHTMLTextGetFormatted
* Return Type: 	String
* Description: 	converts text between start & end into a somewhat formatted
*				ASCII document.
* In: 
*	html:		XmHTMLWidget id;
*	pdef:		papersize definition. unittype must be XmHTML_CHAR;
*	start:		start object;
*	end:		end object;
*	options:	unused;
* Returns:
*	a String with formatted ASCII content. Must be freed by caller.
*****/
String
_XmHTMLTextGetFormatted(XmHTMLWidget html, XmHTMLPaperSize *pdef, 
	XmHTMLObjectTableElement start, XmHTMLObjectTableElement end,
	Byte options)
{
	if(pdef->unit_type != XmHTML_CHAR)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLTextGetFormatted"),
			XMHTML_MSG_88, "CHAR");
		return(NULL);
	}
	return(NULL);
}

#if 0
#ifdef WITH_PS
/*****
* Name:			_XmHTMLTextGetPS
* Return Type: 	String
* Description: 	converts text between start & end into a postscript document
* In: 
*	html:		XmHTMLWidget id;
*	pdef:		papersize definition. unittype must be XmHTML_POINT;
*	start:		start object;
*	end:		end object;
*	options:	postscript options (font to use, header, footer cmds);
* Returns:
*	a String with postscript commands. Must be freed by caller.
*****/
String
_XmHTMLTextGetPS(XmHTMLWidget html, XmHTMLPaperSize *pdef, 
	XmHTMLObjectTableElement start, XmHTMLObjectTableElement end,
	Byte options)
{
	PIXMAP pixmap;
	WINDOW save;
	int scroll_x, scroll_y;
	int paint_x, paint_y, paint_w, paint_h;
	int margin_w, margin_h, work_w;
	ToolkitAbstraction *tka;
	String buf, ret_val = NULL;
	XpmAttributes xpm;
	int xpm_err = XpmSuccess;
	GC gc, bg_gc;
	int y = 0;
	XmHTMLObjectTable *pstart, *pend;
	XmImageInfo *info;

	if(pdef->unit_type != XmHTML_POINT)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLTextGetPS"),
			XMHTML_MSG_88, "POINT");
		return(NULL);
	}

	/*****
	* dirty trick: create a single pixmap in which we will draw each 
	* page. Set it as the drawable in the current tka, adjust the document
	* dimensions to reflect the selected paper properties, recalculate
	* the layout and paint each page. When a page has been rendered,
	* convert it to postscript and append to the return buffer.
	*****/

	tka = HTML_ATTR(tka);

	if(pdef->width > (Dimension)~0 || pdef->height > (Dimension)~0)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLTextGetPS"),
			"Selected paper to large. Can't convert to postscript (yet)");
		return(NULL);
	}

	if((pixmap = tka->CreatePixmap(tka->dpy, tka->DefaultRoot,
		pdef->width, pdef->height, XCCGetDepth(HTML_ATTR(xcc)))) == None)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLTextGetPS"),
			XMHTML_MSG_66, "(postscript stage 1 output)");
		return(NULL);
	}

	/* save current drawable */
	save = (WINDOW)tka->win;

	/* save all settins that will get altered */
	scroll_x = HTML_ATTR(scroll_x);
	scroll_y = HTML_ATTR(scroll_y);
	paint_y  = HTML_ATTR(paint_y);
	paint_h  = HTML_ATTR(paint_height);
	paint_x  = HTML_ATTR(paint_x);
	paint_w  = HTML_ATTR(paint_width);
	pstart   = HTML_ATTR(paint_start);
	pend     = HTML_ATTR(paint_end);
	margin_w = HTML_ATTR(margin_width);
	margin_h = HTML_ATTR(margin_height);
	work_w   = HTML_ATTR(work_width);
	gc       = HTML_ATTR(gc);
	bg_gc    = HTML_ATTR(bg_gc);

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
	HTML_ATTR(work_width) = pdef->width - pdef->right_margin;

	/* we also will be needing seperate GC's for this */
	HTML_ATTR(gc) = tka->CreateGC(tka->dpy, pixmap, 0, NULL);
	tka->SetFunction(tka->dpy, HTML_ATTR(gc), tka->gc_func[GC_GXcopy]);
	tka->SetForeground(tka->dpy, HTML_ATTR(gc), HTML_ATTR(body_fg));
	tka->SetBackground(tka->dpy, HTML_ATTR(gc), HTML_ATTR(body_bg));

	HTML_ATTR(bg_gc) = tka->CreateGC(tka->dpy, pixmap, 0, NULL);
	tka->CopyGC(tka->dpy, HTML_ATTR(gc), 0xFFFF, HTML_ATTR(bg_gc));

	/* set new drawable */
	XmHTMLTkaSetDrawable(tka, pixmap);

	/* Recompute layout for new paper definition */
	_XmHTMLComputeLayout(html);

	/* create PS header */
	ret_buf = PSopenDoc(void);

	/* and paint & convert each page in turn */
	while(HTML_ATTR(scroll_y) < HTML_ATTR(formatted_height))
	{
		/*****
		* Initialize pixmap if we don't have a body image. Refresh will
		* do perform background image stuff.
		*****/
		if(!HTML_ATTR(body_image))
		{
			tka->SetForeground(tka->dpy, HTML_ATTR(gc), HTML_ATTR(body_bg));
			tka->FillRectangle(tka->dpy, tka->win, HTML_ATTR(gc), 0, 0,
				pdef->width, pdef->height);
		}

		/* paint this page */
		_XmHTMLRefresh(html, 0, 0, pdef->width, pdef->height);

		/* move to next page */
		HTML_ATTR(scroll_y) += pdef->height;

		if((info = _XmHTMLImageCreateInfoFromPixmap(tka->dpy, pixmap,
				pdef->width, pdef->height)) == NULL)
		{
			_XmHTMLWarning(__WFUNC__(html, "_XmHTMLTextGetPS"),
				"Pixmap conversion failed (postscript output stage 2)");
			free(ret_buf);
			ret_buf = NULL;
			goto done;
		}
		ret_buf = PSImageToPage(ret_buf, info);

		/* release info, we no longer need it */
		_XmHTMLFreeImageInfo(html, info, False);
	}

done:
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

	/* free allocated gc's */
	tka->FreeGC(tka->dpy, HTML_ATTR(gc));
	tka->FreeGC(tka->dpy, HTML_ATTR(bg_gc));

	/* restore original gc's */
	HTML_ATTR(gc) = gc;
	HTML_ATTR(bg_gc) = bg_gc;

	XmHTMLTkaSetDrawable(tka, save);

	/* Do a redisplay to restore everyting correctly */
	XmHTMLRedisplay((Widget)html);

	return(ret_val);
}

#else

/*****
* Name:			_XmHTMLTextGetXPM
* Return Type: 	String
* Description: 	converts text between start & end into a postscript document
* In: 
*	html:		XmHTMLWidget id;
*	pdef:		papersize definition. unittype must be XmHTML_POINT;
*	start:		start object;
*	end:		end object;
*	options:	postscript options (font to use, header, footer cmds);
* Returns:
*	a String with postscript commands. Must be freed by caller.
*****/
String
_XmHTMLTextGetPS(XmHTMLWidget html, XmHTMLPaperSize *pdef, 
	XmHTMLObjectTableElement start, XmHTMLObjectTableElement end,
	Byte options)
{
	PIXMAP pixmap;
	WINDOW save;
	int scroll_x, scroll_y;
	int paint_x, paint_y, paint_w, paint_h;
	int margin_w, margin_h, work_w;
	ToolkitAbstraction *tka;
	String buf, ret_val = NULL;
	XpmAttributes xpm;
	int xpm_err = XpmSuccess;
	GC gc, bg_gc;
	int y = 0;
	XmHTMLObjectTable *pstart, *pend;

	if(pdef->unit_type != XmHTML_POINT)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLTextGetPS"),
			XMHTML_MSG_88, "POINT");
		return(NULL);
	}

	/*****
	* dirty trick: create a single pixmap in which we will draw each 
	* page. Set it as the drawable in the current tka, adjust the document
	* dimensions to reflect the selected paper properties, recalculate
	* the layout and paint each page. When a page has been rendered,
	* convert it to postscript and append to the return buffer.
	*****/

	tka = HTML_ATTR(tka);

	if(pdef->width > (Dimension)~0 || pdef->height > (Dimension)~0)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLTextGetPS"),
			"Selected paper to large. Can't convert to postscript (yet)");
		return(NULL);
	}

	if((pixmap = tka->CreatePixmap(tka->dpy, tka->defaultRoot,
		pdef->width, pdef->height, XCCGetDepth(HTML_ATTR(xcc)))) == None)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLTextGetPS"),
			XMHTML_MSG_66, "(postscript stage 1 output)");
		return(NULL);
	}

	/* save current drawable */
	save = (WINDOW)tka->win;

	/* save all settins that will get altered */
	scroll_x = HTML_ATTR(scroll_x);
	scroll_y = HTML_ATTR(scroll_y);
	paint_y  = HTML_ATTR(paint_y);
	paint_h  = HTML_ATTR(paint_height);
	paint_x  = HTML_ATTR(paint_x);
	paint_w  = HTML_ATTR(paint_width);
	pstart   = HTML_ATTR(paint_start);
	pend     = HTML_ATTR(paint_end);
	margin_w = HTML_ATTR(margin_width);
	margin_h = HTML_ATTR(margin_height);
	work_w   = HTML_ATTR(work_width);
	gc       = HTML_ATTR(gc);
	bg_gc    = HTML_ATTR(bg_gc);

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
	HTML_ATTR(work_width) = pdef->width - pdef->right_margin;

	/* we also will be needing seperate GC's for this */
	HTML_ATTR(gc) = tka->CreateGC(tka->dpy, pixmap, 0, NULL);
	tka->SetFunction(tka->dpy, HTML_ATTR(gc), tka->gc_func[GC_GXcopy]);
	tka->SetForeground(tka->dpy, HTML_ATTR(gc), HTML_ATTR(body_fg));
	tka->SetBackground(tka->dpy, HTML_ATTR(gc), HTML_ATTR(body_bg));

	HTML_ATTR(bg_gc) = tka->CreateGC(tka->dpy, pixmap, 0, NULL);
	tka->CopyGC(tka->dpy, HTML_ATTR(gc), 0xFFFF, HTML_ATTR(bg_gc));

	/* set new drawable */
	XmHTMLTkaSetDrawable(tka, pixmap);

	/* Recompute layout for new paper definition */
	_XmHTMLComputeLayout(html);

	/* and paint & convert each page in turn */
	while(HTML_ATTR(scroll_y) < HTML_ATTR(formatted_height))
	{
		/*****
		* Initialize pixmap if we don't have a body image. Refresh will
		* do perform background image stuff.
		*****/
		if(!HTML_ATTR(body_image))
		{
			tka->SetForeground(tka->dpy, HTML_ATTR(gc), HTML_ATTR(body_bg));
			tka->FillRectangle(tka->dpy, tka->win, HTML_ATTR(gc), 0, 0,
				pdef->width, pdef->height);
		}

		/* paint this page */
		_XmHTMLRefresh(html, 0, 0, pdef->width, pdef->height);

		/* move to next page */
		HTML_ATTR(scroll_y) += pdef->height;

		/*****
		* For now, let xpm create an XPM data buffer from the created
		* pixmap (libXpm puts the pixmap into an XImage and converts
		* the resulting data).
		* Later on we will do this by ourselves.
		*****/

		xpm.width = pdef->width;
		xpm.height = pdef->height;
		xpm.valuemask = XpmSize;
		xpm_err = XpmCreateBufferFromPixmap(tka->dpy, &buf, pixmap, None, &xpm);

		/* too bad if it fails */
		if(xpm_err != XpmSuccess)
		{
			if(buf != NULL)
				free(buf);
			switch(xpm_err)
			{
				case XpmColorError:
				case XpmColorFailed:
					fprintf(stderr, "XPM: color allocation error.\n");
					break;
				case XpmNoMemory:
					fprintf(stderr, "XPM: out of memory\n");
					break;
				default:
					fprintf(stderr, "XPM: unknown error\n");
			}
			if(ret_val)
				free(ret_val);
			ret_val = NULL;
			goto done;
		}
		else
		{
			/*****
			* TODO
			* Convert the xpm data to an XmImageInfo structure and convert
			* that to postscript.
			*
			* For now, we append the xpm data to the already received
			* pages. This will result in a multi-image pixmap, which
			* only few programs support (ImageMagick is one if I'm not
			* mistaking). Postscript output can be obtained by splitting
			* the return buffer into seperate pages and then running
			* each page through netpbm
			* (xpmtoppm image.xpm | pnmtops > page.ps)
			*****/
			if(ret_val)
			{
				ret_val = realloc(ret_val, strlen(ret_val) + strlen(buf) + 1);
				strcat(ret_val, buf);
			}
			else
				ret_val = strdup(buf);
			free(buf);
		}
	}

done:
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

	/* free allocated gc's */
	tka->FreeGC(tka->dpy, HTML_ATTR(gc));
	tka->FreeGC(tka->dpy, HTML_ATTR(bg_gc));

	/* restore original gc's */
	HTML_ATTR(gc) = gc;
	HTML_ATTR(bg_gc) = bg_gc;

	XmHTMLTkaSetDrawable(tka, save);

	/* Do a redisplay to restore everyting correctly */
	XmHTMLRedisplay((Widget)html);

	return(ret_val);
}

#endif	/* WITH_PS */
#endif	/* 0 */
