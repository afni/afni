/* 
LiteClue.c - LiteClue widget
	See LiteClue documentation
	Version 1.4

Copyright 1996 COMPUTER GENERATION, INC.,

The software is provided "as is", without warranty of any kind, express
or implied, including but not limited to the warranties of
merchantability, fitness for a particular purpose and noninfringement.
In no event shall Computer Generation, inc. nor the author be liable for
any claim, damages or other liability, whether in an action of contract,
tort or otherwise, arising from, out of or in connection with the
software or the use or other dealings in the software.

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation.

Author:
Gary Aviv 
Computer Generation, Inc.,
gary@compgen.com
www.compgen.com/widgets

Thanks to Contributers:
J Satchell, Eric Marttila 
*/
/* Revision History:
$Log$
Revision 1.18  2009/03/05 21:55:24  rickr
Cput

Revision 1.17  2005/10/18 22:00:53  rwcox
Cput

Revision 1.16  2003/12/19 22:39:00  rwcox
Cput

Revision 1.15  2003/12/16 16:13:12  rhammett
Cput

Revision 1.14  2003/08/05 17:20:46  rwcox
Cput

Revision 1.13  2000/12/21 16:10:54  cox
AFNI

Revision 1.3  1997/11/06 16:26:48  cox
Fixes so it won't crash so easily

Revision 1.2  1997/11/05 21:28:04  cox
*** empty log message ***

Revision 1.13  1997/07/07 14:55:04  gary
Cancel timeouts when XcgLiteClueDeleteWidget is called to prevent
errant timeout event on deleted widget.

Revision 1.12  1997/06/20 20:09:09  gary
Add XcgLiteClueDispatchEvent to enable clues for insensitive widgets.

Revision 1.11  1997/06/15 14:10:24  gary
Add XcgLiteClueDispatchEvent to enable clues for insensitive widgets.

Revision 1.10  1997/04/14 13:02:33  gary
Attempt to fix problem when we get multiple enter events bu no leave event.

Revision 1.9  1997/03/10 14:42:41  gary
Attempt to fix problem when we get multiple enter events bu no leave event.
Add C++ wrapper to allow linking with C++ programs. (In HView.h)

Revision 1.8  1997/01/17 13:44:14  gary
Support of cancelWaitPeriod resource: this is a period from the point
a help popdown occurs in which the normal waitPeriod is suspended
for the next popup

Revision 1.7  1996/12/16 22:35:38  gary
Fix double entry problem

Revision 1.6  1996/11/18 14:52:21  gary
remove some compile warnings pointed out by a user

Revision 1.5  1996/11/12 20:56:43  gary
remove some compile warnings

Revision 1.4  1996/10/20  13:38:16  gary
Version 1.2 freeze

Revision 1.3  1996/10/19 16:16:30  gary
Compile warning removed with cast

Revision 1.2  1996/10/19 16:07:38  gary
a) R4 back compatibility
b) Delay before pop up of help, waitPeriod resource (def 500 ms).
	Thanks to J Satchell for this.
c) Button press in watched widget pops down help

Revision 1.1  1996/10/18 23:14:58  gary
Initial


$log
Cancel timeouts when XcgLiteClueDeleteWidget is called to prevent
errant timeout event on deleted widget.
$log
*/

/******************************************************************
  Modified by RWCox of MCW, to draw a rectangle around the text,
  and to make the widget popup on screen if it would go off.
  These changes can be found by searching for the string "RWC".
  These modifications are released to the public domain.
*******************************************************************/

#include <unistd.h>
#include <signal.h>
/* #include <Xm/XmP.h> */
#include <X11/IntrinsicP.h> 
#include <X11/StringDefs.h>
extern void RWC_xineramize( Display *,int,int,int,int,int *,int *) ;

#include "LiteClueP.h"

#include <stdio.h>

#if 0
#define CheckWidgetClass(routine) \
	if (XtClass(w) != xcgLiteClueWidgetClass) \
		wrong_widget(routine)

#else
#define CheckWidgetClass(routine) \
	if (XtClass(w) != xcgLiteClueWidgetClass) \
		return BADVALUE
#endif

/* extern _XmSelectColorDefault(); */ /* cgi */
static Boolean setValues( Widget _current, Widget _request, Widget _new, ArgList args, Cardinal * num_args);
static void Initialize(Widget treq, Widget tnew, ArgList args, Cardinal *num_args);
struct liteClue_context_str * alloc_liteClue_context(void);

/* keep information about each widget we are keeping track of */
struct liteClue_context_str
{
	ListThread next;	/* next in list */
	Widget watched_w;	/* the widget we are watching */
	XcgLiteClueWidget cw;	/* pointer back to the liteClue widget */
	Position  abs_x, abs_y;
	Boolean sensitive;	/* if False, liteClue is suppressed */
	char * text;		/* text to display */
	short text_size;	/* its size */
};

void free_widget_context(XcgLiteClueWidget cw, struct liteClue_context_str * obj);
/*
Widget resources: eg to set LiteClue box background:
 *XcgLiteClue.background: yellow
       
*/
#define offset(field) XtOffsetOf(LiteClueRec, field)
static XtResource resources[] =
{
	{XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
		offset(liteClue.foreground), XtRString, "black"},
#if XtSpecificationRelease < 5
	{XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
		offset(liteClue.font), XtRString, 
          "-adobe-new century schoolbook-bold-r-normal-*-14-*-*-*-*-*-*-*"},
#else
	{XtNfontSet, XtCFontSet, XtRFontSet, sizeof(XFontSet),
		offset(liteClue.fontset), XtRString, "-adobe-new century schoolbook-bold-r-normal-*-12-*"},
#endif
	{XgcNwaitPeriod, XgcCWaitPeriod, XtRInt , sizeof(int),
		offset(liteClue.waitPeriod),XtRString, "500" },

	{XgcNcancelWaitPeriod, XgcCCancelWaitPeriod, XtRInt , sizeof(int),
		offset(liteClue.cancelWaitPeriod),XtRString, "2000" },
};

#undef offset

#if 0
static XtActionsRec actions[] = 
}; /* actions */
#endif


LiteClueClassRec xcgLiteClueClassRec =
{
    {
	(WidgetClass)&overrideShellClassRec,	/* superclass */
	"XcgLiteClue",				/* class_name */
	(Cardinal)sizeof(LiteClueRec),		/* widget size */
	NULL,	/* classInit, */		/* class_init */
	(XtWidgetClassProc)NULL, /* classPartInit, */	/* class_part_init */
	(XtEnum)FALSE,				/* class_inited */
	(XtInitProc)Initialize,			/* initialize */
	(XtArgsProc)NULL,			/* init_hook */
	XtInheritRealize,			/* realize */
	(XtActionList)0,			/* actions */
	(Cardinal)0,			/* num_actions */
	(XtResourceList)resources,		/* resources */
	(Cardinal)XtNumber(resources),		/* num_resources */
	NULLQUARK,				/* xrm_class */
	TRUE,					/* compress_motion */
	(XtEnum)FALSE,				/* compress_exposur */
	TRUE,					/* compress enterleave */
	FALSE,					/* visibility_interest */
	(XtWidgetProc)NULL,	/* destroy, */			/* destroy */
	XtInheritResize,
	XtInheritExpose,	/* expose, */
	(XtSetValuesFunc)setValues,		/* set_values */
	(XtArgsFunc)NULL,			/* set_values_hook */
	XtInheritSetValuesAlmost,		/* set_values_almost */
	(XtArgsProc)NULL,			/* get_values_hook */
	XtInheritAcceptFocus,		/* accept_focus */
	XtVersion,				/* version */
	(XtPointer)NULL,			/* callback_private */
	XtInheritTranslations,
	XtInheritQueryGeometry,			/* query_geometry */
	XtInheritDisplayAccelerator,		/* display_accelerator */
	(XtPointer)0,			/* extension */
    },
    { /*** composite-Class ***/
	XtInheritGeometryManager,	/* geometry_manager   	*/    	
	XtInheritChangeManaged,	/* change_managed		*/	
	XtInheritInsertChild,	/* insert_child		*/	
	XtInheritDeleteChild,	/* delete_child		*/	
	NULL	/* extension		*/	
    }, 
	{ /* Shell */
	(XtPointer) NULL,       /* pointer to extension record      */
	},
	/* Override Shell */
	{
	0,
	},
	/* LiteClue */
	{
	0,
	},
};

WidgetClass xcgLiteClueWidgetClass = (WidgetClass) & xcgLiteClueClassRec;

/* doubly linked list processing */

/*
	 initialize header - both pointers point to it
*/
static void xcgListInit(ListThread *newbuf)
{
	newbuf->back = newbuf;
	newbuf->forw = newbuf;
}


/*
	 insert newbuf before posbuf
*/
static void xcgListInsertBefore(ListThread *newlist, ListThread *poslist)
{
	ListThread *prevbuf;

	prevbuf = poslist->back;

	poslist->back = newlist;
	newlist->forw = poslist;
	newlist->back = prevbuf;
	prevbuf->forw = newlist;
}


/*
	remove rembuf from queue
*/
static ListThread * xcgListRemove(ListThread *rembuf)
{
	ListThread *prevbuf, *nextbuf;

	prevbuf = rembuf->back;
	nextbuf = rembuf->forw;

	prevbuf->forw = nextbuf;
	nextbuf->back = prevbuf;		

	rembuf->back = (ListThread *) NULL;	/* for safety to cause trap if ..*/
	rembuf->forw = (ListThread *) NULL;	/* .. mistakenly refed */
	return rembuf;
}

/*
The font_information is derived 
*/

#if XtSpecificationRelease >= 5
/* R5 and above code */
static void compute_font_info(XcgLiteClueWidget cw)
{
	XRectangle ink;
	XRectangle logical;

	if (!cw->liteClue.fontset)
		return;
	XmbTextExtents(cw->liteClue.fontset, "1", 1,&ink, &logical);

	cw->liteClue.font_baseline = -logical.y;	/* y offset from top to baseline, 
			don't know why this is returned as negative */
	cw->liteClue.font_width = logical.width;	/* the width and height of the object */
	cw->liteClue.font_height = logical.height;
}

#else
/* R4 and below code */
static void compute_font_info(XcgLiteClueWidget cw)
{
	int direction_return;
	int font_ascent_return, font_descent_return; 
	XCharStruct oret;
	if ( cw->liteClue.font == NULL )
		return;
	XTextExtents( cw->liteClue.font, "1", 1,
		&direction_return,
		&font_ascent_return, &font_descent_return, &oret);

	cw->liteClue.font_baseline = oret.ascent;	/* y offset from top to baseline, 
			don't know why this is returned as negative */
	cw->liteClue.font_width = oret.width;	/* the width and height of the object */
	cw->liteClue.font_height = oret.ascent+oret.descent;
}
#endif

/*
 Creates the various graphic contexts we will need 
*/
static void create_GC(XcgLiteClueWidget cw )
{
	XtGCMask valuemask;
	XGCValues myXGCV;


	valuemask = GCForeground | GCBackground | GCFillStyle ;
	myXGCV.foreground = cw->liteClue.foreground;
	myXGCV.background = cw->core.background_pixel;
	myXGCV.fill_style = FillSolid; 

#if XtSpecificationRelease < 5		/* R4 hack */
	myXGCV.font = cw->liteClue.font->fid; 
#endif	/* end R4 hack */

	if (cw->liteClue.text_GC )
		XtReleaseGC((Widget) cw, cw->liteClue.text_GC );
	cw->liteClue.text_GC = XtGetGC((Widget)cw, valuemask, &myXGCV);
}


/* a routine to halt execution and force  
a core dump for debugging analysis	
when a public routine is called with the wrong class of widget
*/
static void wrong_widget(char * routine)
{
	int mypid = getpid(); 
	fprintf(stderr, "Wrong class of widget passed to %s\n", routine);
	fflush(stderr); 
	kill(mypid, SIGABRT); 
}

/*
Find the target in the widget list. Return context pointer if found,
NULL if not
*/
static struct liteClue_context_str * find_watched_widget(XcgLiteClueWidget cw,
	Widget target)
{
	struct liteClue_context_str * obj;

	for (obj = (struct liteClue_context_str *) cw->liteClue.widget_list.forw; 
		obj != (struct liteClue_context_str *) & cw->liteClue.widget_list; 
		obj = (struct liteClue_context_str *)obj->next.forw )
	{
		if (target == obj->watched_w)
			return obj;
	}
	return NULL;
}

/*
	allocate and initialize a widget context
*/
struct liteClue_context_str * alloc_liteClue_context(void)
{
	struct liteClue_context_str * out;
	out = (struct liteClue_context_str *) XtMalloc(sizeof(struct liteClue_context_str));
	memset(out, 0, sizeof(struct liteClue_context_str));
	xcgListInit(&out->next);	
	return out ;
}

/*
	allocate, initialize and link a liteClue context to the list
*/
static struct liteClue_context_str * alloc_link_liteClue_context(XcgLiteClueWidget cw )
{
	struct liteClue_context_str * out = alloc_liteClue_context();

	/* link as new last */
	xcgListInsertBefore(&out->next, &cw->liteClue.widget_list);
	out->cw = cw;	/* initialize this emeber - its always the same */
	return out;
}

/*
	free a widget context
*/
void free_widget_context(XcgLiteClueWidget cw, struct liteClue_context_str * obj)
{
	xcgListRemove((ListThread *)obj);
	/* free up all things object points to */
	obj->sensitive = False;
	if (obj->text )
		XtFree(obj->text);
	XtFree((char *) obj);
}


/* -------------------- Widget Methods ---------------------- */
/* Initialize method */
static void Initialize(Widget treq, Widget tnew, ArgList args, 
Cardinal *num_args)
{
	XcgLiteClueWidget cw = (XcgLiteClueWidget) tnew;


	cw->liteClue.text_GC = NULL;
	cw->liteClue.HelpIsUp = False;
	cw->liteClue.HelpPopDownTime = 0;
	cw->liteClue.interval_id = (XtIntervalId)0;
	xcgListInit(&cw->liteClue.widget_list);	/* initialize empty list */
	compute_font_info(cw);
	create_GC(cw );
}

static Boolean setValues( Widget _current, Widget _request, Widget _new, ArgList args, Cardinal * num_args)
{
	XcgLiteClueWidget cw_new = (XcgLiteClueWidget) _new;
	XcgLiteClueWidget cw_cur = (XcgLiteClueWidget) _current;

	/* values of cw_new->liteClue.cancelWaitPeriod and
	   cw_new->liteClue.waitPeriod are accepted without checking */

	if (cw_new->liteClue.foreground != cw_cur->liteClue.foreground 
	||  cw_new->core.background_pixel != cw_cur->core.background_pixel )
	{
		create_GC(cw_new);
	}

	return FALSE;
}

/* ----------------- Event handlers ------------------------*/

/* At this point the help may be popup 
*/
static void timeout_event( XtPointer client_data, XtIntervalId *id)
{
#define BorderPix 3
	struct liteClue_context_str * obj = (struct liteClue_context_str *) client_data;
	XcgLiteClueWidget cw = obj->cw;
	Position  abs_x, abs_y;

	XRectangle ink;
	XRectangle logical;
	Position   w_height=0;
	Widget w;

        int RWC_width , RWC_height ;

	if (cw->liteClue.interval_id == (XtIntervalId)0)
		return;	/* timeout was removed but callback happened anyway */
	cw->liteClue.interval_id = (XtIntervalId)0;
	if (obj->sensitive == False)
		return;

	w = obj->watched_w;
	XtVaGetValues(w, XtNheight, &w_height, NULL );
	/* position just below the widget */
	XtTranslateCoords(w, 0, w_height, &abs_x, &abs_y);

#if XtSpecificationRelease < 5		/* R4 hack */
	{
	int direction_return;
	int font_ascent_return, font_descent_return; 
	XCharStruct oret;
	XTextExtents( cw->liteClue.font ,obj->text , obj->text_size,
		&direction_return,
		&font_ascent_return, &font_descent_return, &oret); 
	logical.width = oret.width;
	}
#else
	XmbTextExtents(cw->liteClue.fontset, obj->text , obj->text_size ,&ink, &logical);
#endif

        RWC_width  = 2*BorderPix +logical.width ;
        RWC_height = 2*BorderPix + cw->liteClue.font_height ;
	XtResizeWidget((Widget) cw, RWC_width , RWC_height ,
			cw->core.border_width );

/** RWCox change to widget location **/
        { int scw = WidthOfScreen(XtScreen(cw)) , sch = HeightOfScreen(XtScreen(cw)) ;
          int newx = abs_x + 1 , newy = abs_y+1 ; int xx,yy ;
          if( newx + logical.width > scw ) newx = scw - logical.width - 2*BorderPix ;
          if( newx < 0 )                   newx = 0 ;
          if( newy + cw->liteClue.font_height + 2 >= sch )
             newy = newy - w_height - cw->liteClue.font_height - 2*BorderPix ;
          if( newy < 0 ) newy = 0 ;

#if 1
          RWC_xineramize( XtDisplay(w) ,      /* 27 Sep 2000 - see xutil.[ch] */
                          newx,newy , RWC_width,RWC_height , &xx,&yy ) ;
          if( yy < newy )
             yy = newy - w_height - cw->liteClue.font_height - 2*BorderPix ;
	  XtMoveWidget((Widget) cw, xx,yy);
#else
	  XtMoveWidget((Widget) cw, newx,newy);
#endif
        }

	XtPopup((Widget) cw, XtGrabNone); RWC_sleep(1);
	cw->liteClue.HelpIsUp = True;

#if XtSpecificationRelease < 5		/* R4 hack */
	XDrawImageString(XtDisplay((Widget) cw), XtWindow((Widget) cw), 
		cw->liteClue.text_GC , BorderPix, 
		BorderPix + cw->liteClue.font_baseline, obj->text , obj->text_size);
#else
	XmbDrawImageString(XtDisplay((Widget) cw), XtWindow((Widget) cw), 
		cw->liteClue.fontset,
		cw->liteClue.text_GC , BorderPix, 
		BorderPix + cw->liteClue.font_baseline, obj->text , obj->text_size);
#endif

/** RWCox change to add a rectangle **/
        XDrawRectangle( XtDisplay((Widget) cw) , XtWindow((Widget) cw) ,
                        cw->liteClue.text_GC ,
                        0,0 , RWC_width-1,RWC_height-1 ) ;
}

/*
Pointer enters watched widget, set a timer at which time it will
popup the help
*/
static void Enter_event(Widget w, XtPointer client_data, XEvent * xevent, Boolean * continue_to_dispatch )
{
	struct liteClue_context_str * obj = (struct liteClue_context_str *) client_data;
	XcgLiteClueWidget cw = obj->cw;
	XEnterWindowEvent * event = & xevent->xcrossing;
	int current_waitPeriod ;

	if (obj->sensitive == False)
		return;
	/* check for two enters in a row - happens when widget is
	   exposed under a pop-up */
	if (cw->liteClue.interval_id != (XtIntervalId)0) 
		return;
	if(event->mode != NotifyNormal)
		return;

	/* if a help was recently popped down, don't delay in poping up
	   help for next watched widget
	*/
	if ((event->time -  cw->liteClue.HelpPopDownTime) > 
			cw->liteClue.cancelWaitPeriod ) 
		current_waitPeriod = cw->liteClue.waitPeriod,timeout_event;
	else
		current_waitPeriod = 0;

	cw->liteClue.interval_id = XtAppAddTimeOut(
			XtWidgetToApplicationContext(w),
			current_waitPeriod, timeout_event, client_data);
}

/*
Remove timer, if its pending. Then popdown help.
*/
static void Leave_event(Widget w, XtPointer client_data, XEvent * xevent, Boolean * continue_to_dispatch )
{
	struct liteClue_context_str * obj = (struct liteClue_context_str *) client_data;
	XcgLiteClueWidget cw = obj->cw;
	XEnterWindowEvent * event = & xevent->xcrossing;

	if (cw->liteClue.interval_id != (XtIntervalId)0) 
	{
		XtRemoveTimeOut(cw->liteClue.interval_id);
		cw->liteClue.interval_id= (XtIntervalId)0;
	}

	if (obj->sensitive == False)
		return;
	if (cw->liteClue.HelpIsUp)
	{
		XtPopdown((Widget) cw);
		cw->liteClue.HelpIsUp = False;
		cw->liteClue.HelpPopDownTime = event->time;
	}
}

/* ---------------- Widget API ---------------------------- */

/*
;+
XcgLiteClueAddWidget -- Add a widget to be watched. LiteClue will be given for this widget

Func:	A widget will be added to the LiteClue watched list. Clues are given for
	sensitive watched widgets when the pointer enters its window. If the
	widget is already watched, the passed text replaces its current clue
	text. If text is null, the widget is still added, if it is not already
	in the list, but no clue will appear. Text may be specified with
	XcgLiteClueAddWidget in a subsequent call. When text is null and the
	widget is already in the list, its text is not changed. When a widget
	will is added to the watched list, it automatically becomes sensitive.
	Otherwise, its sensitivity is not changed. A watched widget which is not
	sensitive retains its context but clues are suppressed.
	None of this affects the behaviour of the watched widget itself.
	LiteClue monitors enter and leave events of the watched widget's
	window passively.

Input:	w - LiteClue widget
	watch - the widget to give liteClues for
	text - pointer to liteClue text. (May be NULL)
	size - size of text. May be zero
		in which case a strlen will be done.
	option - option mask, future use, zero for now.
Output: 

Return:	

;-
*/
void XcgLiteClueAddWidget(Widget w, Widget watch,  char * text, int size, int option )
{
#	define ROUTINE "XcgLiteClueAddWidget"
#       define BADVALUE /* nada */
	XcgLiteClueWidget cw = (XcgLiteClueWidget) w;
	struct liteClue_context_str * obj;
	Boolean exists = False;

	CheckWidgetClass(ROUTINE);	/* make sure we are called with a LiteClue widget */

	obj = find_watched_widget(cw, watch);
	if (obj)
	{
		exists = True;
		if (text)
		{
			if(obj->text)
				XtFree(obj->text);
			obj->text = NULL;
		}
	}
	else
	{
		obj = alloc_link_liteClue_context(cw );
		obj->watched_w = watch;
	}
	if (text && !(obj->text))
	{
		if (!size)
			size = strlen(text);
		obj->text = (char*) XtMalloc(size+1);
		memcpy(obj->text, text, size);
		obj->text[size] = 0;
		obj->text_size = size;
	}
	if (!exists)	/* was created */
	{
		XtAddEventHandler(watch, EnterWindowMask, False, 
			Enter_event, (XtPointer) obj);
		XtAddEventHandler(watch, LeaveWindowMask|ButtonPressMask, 
			False, Leave_event, (XtPointer) obj);
		obj->sensitive = True;
	}

#	undef ROUTINE
#       undef BADVALUE
}


/*
;+
XcgLiteClueDeleteWidget -- Delete a widget that is watched. 

Func:	A widget is deleted from the watched list and its resources are
	freed. LiteClue is no longer given for the widget.
	If the widget is not watched, nothing is done.

Input:	w - LiteClue widget
	watch - the widget to delete
Output: 

Return:	

;-
*/
void XcgLiteClueDeleteWidget(Widget w, Widget watch)
{
#	define ROUTINE "XcgLiteClueDeleteWidget"
#       define BADVALUE  /* nada */
	XcgLiteClueWidget cw = (XcgLiteClueWidget) w;
	struct liteClue_context_str * obj;

	CheckWidgetClass(ROUTINE);	/* make sure we are called with a LiteClue widget */
	obj = find_watched_widget(cw, watch);
	if (obj)
	{
		XtRemoveEventHandler(watch, EnterWindowMask, False, 
			Enter_event, (XtPointer) obj);
		XtRemoveEventHandler(watch, LeaveWindowMask|ButtonPressMask, 
			False, Leave_event, (XtPointer) obj);
		if (cw->liteClue.interval_id != (XtIntervalId)0) 
		{
			XtRemoveTimeOut(cw->liteClue.interval_id);
			cw->liteClue.interval_id= (XtIntervalId)0;
		}
		free_widget_context(cw, obj);
	}

#	undef ROUTINE
#	undef BADVALUE
}


/*
;+
XcgLiteClueSetSensitive -- Enable/disable sensitivity for watched widget. 

Func:	When a watched widget is sensitive, a clue is poped up when the pointer
	enters its window. When a watched widget is insensitive, the widget is
	retained in the watched list but no clue is poped. The sensitivity of a
	watched widget relative to clues is set or reset by this function. The
	Xt sensitivity of the watched widget is not altered by this function.

Input:	w - LiteClue widget
	watch - the widget to make sensitive or insensitive or NULL
		to change all watched widgets
	sensitive - True or False
Output: 

Return:	

;-
*/
void XcgLiteClueSetSensitive(Widget w, Widget watch, Boolean sensitive)
{
#	define ROUTINE "XcgLiteClueSetSensitive"
#       define BADVALUE  /* nada */
	XcgLiteClueWidget cw = (XcgLiteClueWidget) w;
	struct liteClue_context_str * obj;

	CheckWidgetClass(ROUTINE);	/* make sure we are called with a LiteClue widget */
	if (watch)
	{
		obj = find_watched_widget(cw, watch);
		if (obj)
		{
			obj->sensitive = sensitive;
			return;
		}
		else
			return;
	}

	/* do them all */
	for (obj = (struct liteClue_context_str *) cw->liteClue.widget_list.forw; 
		obj != (struct liteClue_context_str *) & cw->liteClue.widget_list; 
		obj = (struct liteClue_context_str *)obj->next.forw )
	{
		obj->sensitive = sensitive;
	}

#	undef ROUTINE
#	undef BADVALUE
}

/*
;+
XcgLiteClueGetSensitive -- Get sensitivity mode for watched widget. 

Func:	When a watched widget is sensitive, a clue is poped up when the pointer
	enters its window. When a watched widget is insensitive, the widget is
	retained in the watched list but no clue is poped. The sensitivity state
	of a watched widget relative to clues is returned by this function. The
	Xt sensitivity of a widget is a totally independent concept.

Input:	w - LiteClue widget
	watch - the widget for which to get sensitivity state. If NULL
		first watched widget is used. If there are no watched widgets,
		False is returned.
Output: 

Return:	sensitive - True or False

;-
*/
Boolean XcgLiteClueGetSensitive(Widget w, Widget watch)
{
#	define ROUTINE "XcgLiteClueGetSensitive"
#       define BADVALUE  False

	XcgLiteClueWidget cw = (XcgLiteClueWidget) w;
	struct liteClue_context_str * obj;

	CheckWidgetClass(ROUTINE);	/* make sure we are called with a LiteClue widget */
	if (watch)
	{
		obj = find_watched_widget(cw, watch);
		if (obj)
			return obj->sensitive;
		else
			return False;
	}
	/* do the first one */
	obj = (struct liteClue_context_str *) cw->liteClue.widget_list.forw; 
	if (obj != (struct liteClue_context_str *) & cw->liteClue.widget_list)
		return obj->sensitive;
	else
		return False;

#	undef ROUTINE
#	undef BADVALUE
}


/*
;+
XcgLiteClueDispatchEvent -- Dispatch event from main X event loop

Func:	This function may be used to enable clues for insensitive
	watched widgets. Normally, XtAppMainLoop (which calls
	XtDispatchEvent) will not deliver EnterNotify and LeaveNotify
	events to widgets that are not sensitive (XtSetSensitive). This
	prevents clues from poping up for these widgets. To bypass this
	limitation, you can break out XtAppMainLoop and add a call to
	XcgLiteClueDispatchEvent ass follows:

	MyXtAppMainLoop(XtAppContext app) 
	{
	    XEvent event;

	    for (;;) {
	        XtAppNextEvent(app, &event);
		XcgLiteClueDispatchEvent(w, event) ;
	        XtDispatchEvent(&event);
	    }
	} 

Input:	w - LiteClue widget
	event - received event, normally from call to XtAppNextEvent.

Output: void

Return:	True - event was dispatched to non-sensitive watched widget.
	False - not a EnterNotify or LeaveNotify event or window in
		event is not a non-sensitive watched widget.

;-
*/
Boolean XcgLiteClueDispatchEvent(Widget w, XEvent  *event)
{
#	define ROUTINE "XcgLiteClueDispatchEvent"
#       define BADVALUE  False

	XcgLiteClueWidget cw = (XcgLiteClueWidget) w;
	struct liteClue_context_str * obj;
	Boolean continue_to_dispatch;

	if (event->type != EnterNotify && event->type != LeaveNotify)
		return False;
	CheckWidgetClass(ROUTINE);	/* make sure we are called with a LiteClue widget */

	/* scan list */
	for (obj = (struct liteClue_context_str *) cw->liteClue.widget_list.forw; 
		obj != (struct liteClue_context_str *) & cw->liteClue.widget_list; 
		obj = (struct liteClue_context_str *)obj->next.forw )
	{
		if ((XtWindow(obj->watched_w) != event->xany.window)
		||  (XtIsSensitive(obj->watched_w)) )
			continue;
		/* found one */
		if (event->type == EnterNotify )
			Enter_event(obj->watched_w, obj, event,  &continue_to_dispatch);
		else
			Leave_event(obj->watched_w, obj, event,  &continue_to_dispatch);
		return True;
	}
	return False;

#	undef ROUTINE
#	undef BADVALUE
}

