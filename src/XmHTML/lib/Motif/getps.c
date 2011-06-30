/* public.c */

/*****
* Name:			XmHTMLTextGetFormatted
* Return Type: 	String
* Description: 	returns a formatted copy of the current document.
* In: 
*	w:			XmHTMLWidget id;
*	papertype:	type of paper to use (any of the XmHTMLTEXT_PAPERSIZE enums);
*	papersize:	size of paper for custom stuff, or default overrides;
*	type:		type of output wanted, plain, formatted or PS;
*	PSoptions:	options to use when creating postscript output.
* Returns:
*	a string which needs to be freed by the caller.
*****/
String
XmHTMLTextGetFormatted(Widget w, unsigned char papertype,
	XmHTMLPaperSize *paperdef, unsigned char type, unsigned char PSoptions)
{
	XmHTMLWidget html;
	XmHTMLPaperSize *pdef, pbase;
	String ret_val = NULL;

	/* sanity check */
	if(!w || !XmIsHTML(w))
	{
		_XmHTMLBadParent(w, "TextGetFormatted");
		return(NULL);
	}

	/* custom papersize requires a paper definition. */
	if(papertype == XmHTMLTEXT_PAPERSIZE_CUSTOM && paperdef == NULL)
	{
		_XmHTMLWarning(__WFUNC__(w, "XmHTMLTextGetFormatted"), XMHTML_MSG_23);
		return(NULL);
	}

	/* widget ptr */
	html = (XmHTMLWidget)w;

	/*****
	* get appropriate papersize definitions if not given.
	*****/
	if(papertype != XmHTMLTEXT_PAPERSIZE_CUSTOM && paperdef == NULL)
	{
		/* formatting routines use point size */
		if(papertype == XmHTMLTEXT_PAPERSIZE_A4)
		{
			pbase.unit_type     = XmHTML_POINT;
			pbase.paper_type    = XmHTMLTEXT_PAPERSIZE_A4;
			pbase.width         = 597;	/* 210mm */
			pbase.height        = 845;	/* 297mm */
			pbase.left_margin   = 57;	/* 20mm  */
			pbase.right_margin  = 57;
			pbase.top_margin    = 57;
			pbase.bottom_margin = 57;
		}
		else 	/* XmHTMLTEXT_PAPERSIZE_LETTER */
		{
			pbase.unit_type     = XmHTML_POINT;
			pbase.paper_type    = XmHTMLTEXT_PAPERSIZE_LETTER;
			pbase.width         = 614;	/* 8.5in */
			pbase.height        = 795;	/* 11in  */
			pbase.left_margin   = 65;	/* 0.9in */
			pbase.right_margin  = 65;
			pbase.top_margin    = 65;
			pbase.bottom_margin = 51;	/* 0.7in */
		}
		/* convert to correct output type */
		pdef = _XmHTMLTextCheckAndConvertPaperDef(html, &pbase, type);
	}
	else	/* check validity of paper definition and convert to correct type */
		pdef = _XmHTMLTextCheckAndConvertPaperDef(html, paperdef, type);

	if(pdef == NULL)
		return(NULL);

	switch(type)
	{
		case XmHTMLTEXT_PLAIN:
			ret_val = _XmHTMLTextGetPlain(html, pdef, HTML_ATTR(formatted),
				NULL, 0);
			break;
		case XmHTMLTEXT_FORMATTED:
			ret_val = _XmHTMLTextGetFormatted(html, pdef, HTML_ATTR(formatted),
				NULL, 0);
			break;
		case XmHTMLTEXT_POSTSCRIPT:
			ret_val = _XmHTMLTextGetPS(html, pdef, HTML_ATTR(formatted),
				NULL, PSoptions);
			break;
		default:
			_XmHTMLWarning(__WFUNC__(w, "XmHTMLTextGetFormatted"),
				XMHTML_MSG_24);
	}
	/* no longer needed */
	free(pdef);

	return(ret_val);
}

/* output.c */

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
	_XmHTMLToolkitAbstractionSetDrawable(tka, pixmap);

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

	_XmHTMLToolkitAbstractionSetDrawable(tka, save);

	/* Do a redisplay to restore everyting correctly */
	XmHTMLRedisplay((Widget)html);

	return(ret_val);
}


