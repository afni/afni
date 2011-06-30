#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* frames.c : XmHTML frame support
*
* This file Version	$Revision$
*
* Creation date:		Tue Mar 25 18:53:12 GMT+0100 1997
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
* Note:
* Many thanks to Eric Bello <belloer@gemse.fr> for fixing the original
* code!!
*****/
/*****
* ChangeLog 
* $Log$
* Revision 1.1  2011/06/30 16:10:30  rwcox
* Cadd
*
* Revision 1.10  1998/04/27 06:59:39  newt
* tka stuff and a few bugfixes in argument checking
*
* Revision 1.9  1998/04/04 06:28:10  newt
* XmHTML Beta 1.1.3
*
* Revision 1.8  1997/10/23 00:25:01  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.7  1997/08/31 17:35:37  newt
* Several fixes in form creation & destruction and widget reuse. kd & rr
*
* Revision 1.6  1997/08/30 01:04:16  newt
* _XmHTMLWarning proto & color changes: XmHTML now uses manager's color fields.
*
* Revision 1.5  1997/08/01 13:01:40  newt
* my_strdup -> strdup, minor bugfixes and updated comments.
*
* Revision 1.4  1997/05/28 01:48:13  newt
* Sped up _XmHTMLCheckForFrames considerably.
*
* Revision 1.3  1997/04/29 14:27:00  newt
* Header files modifications.
*
* Revision 1.2  1997/04/03 05:35:36  newt
* Changed default name from _top to _frame appended with a number
*
* Revision 1.1  1997/03/28 07:02:46  newt
* Initial Revision
*
*****/ 
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "toolkit.h"
#include XmHTMLPrivateHeader
#include "stack.h"

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/
/* how many times may we retry frame alignment? */
#define MAX_FRAME_ITERATIONS	100
#define ROW	1
#define COL	2
#define ROW_COL	4

/* usefull defines */
#define IS_FRAMESET(F) \
		((F)->is_frameset)
#define IS_FRAME_SIZE_RELATIVE(F) \
		((F)->size_type == FRAME_SIZE_RELATIVE)
#define IS_FRAME_SIZE_OPTIONAL(F) \
		((F)->size_type == FRAME_SIZE_OPTIONAL)
#define IS_FRAME_SIZE_FIXED(F) \
		((F)->size_type == FRAME_SIZE_FIXED)
#define IS_FRAMESET_LAYOUT_ROWS(F) \
		(IS_FRAMESET(F) && ((F)->layout == FRAMESET_LAYOUT_ROWS))
#define IS_FRAMESET_LAYOUT_COLS(F) \
		(IS_FRAMESET(F) && ((F)->layout == FRAMESET_LAYOUT_COLS))
#define IS_FRAMESET_LAYOUT_ROW_COLS(F) \
		(IS_FRAMESET(F) && ((F)->layout == FRAMESET_LAYOUT_ROW_COLS))


/*** Private Datatype Declarations ****/

/* definition of a HTML frameset */
typedef struct _frameSet{
	int type;					/* type of this set, either ROW or COL */
	int border;					/* frame border value */
	int *sizes;					/* array of child sizes */
	FrameSize *size_types;		/* array of possible size specifications */
	int nchilds;				/* max no of childs */
	int childs_done;			/* no of childs processed so far */
	int insert_pos;				/* insertion position of current child */
	struct _frameSet *parent;	/* parent frameset of this frameset */
	struct _frameSet *childs;	/* list of childs */
	struct _frameSet *next;		/* ptr to next frameSet */
	XmHTMLFrameWidget *actualFrameSet; /* ptr to saved FrameSet */
}frameSet;

/* stack of framesets */
typedef struct _frameStack{
	frameSet  *frame_set;
	struct _frameStack *next;
}frameStack;

/*** Private Function Prototype Declarations ****/
static frameSet *popFrameSet(void);
static void	pushFrameSet(frameSet *frame_set);
static frameSet	*doFrameSet(String attributes);
static XmHTMLFrameWidget *doFrame(XmHTMLWidget html, String attributes);
static void	insertFrameSetChild(frameSet *parent, frameSet *child);
static void	insertFrameChild(frameSet *current_set, XmHTMLFrameWidget *frame);
static void	makeFrameSets(XmHTMLWidget html, XmHTMLObject *frameset);
static void	adjustConstraints(XmHTMLWidget html);
static void	adjustFrame(XmHTMLFrameWidget *parent, int *p_width, int *p_height);
static void	destroyFrameSets(frameSet *set);
static void	mapFrames(XmHTMLWidget html);
static void frameDestroyCallback(XmHTMLWidget html, XmHTMLFrameWidget *frame);

/*** Private Variable Declarations ***/
static int current_frame;		/* running frame counter */
static frameSet *frame_sets;	/* list of all framesets processed */
static frameStack frame_base, *frame_stack;

/*****
* Name: 		pushFrameSet
* Return Type: 	void
* Description: 	pushes a frameset on the stack
* In: 
*	frame_set:	frameset to push
* Returns:
*	nothing
*****/
static void
pushFrameSet(frameSet *frame_set)
{
	frameStack *tmp;

	tmp = (frameStack*)malloc(sizeof(frameStack));
	tmp->frame_set = frame_set;
	tmp->next = frame_stack;
	frame_stack = tmp;
}

/*****
* Name: 		popFrameSet
* Return Type: 	frameSet*
* Description: 	pops a frameset of the stack
* In: 
*	nothing
* Returns:
*	the next frameset on the stack, or NULL when stack is empty
*****/
static frameSet*
popFrameSet(void)
{
	frameStack *tmp;
	frameSet *frame_set;

	if(frame_stack->next)
	{
		tmp = frame_stack;
		frame_stack = frame_stack->next;
		frame_set = tmp->frame_set;
		free(tmp);
		return(frame_set);
	}
	return(NULL);
}

/*****
* Name: 		doFrameSet
* Return Type: 	frameSet*
* Description: 	creates and fills a frameSet structure with the info in it's
*				attributes
* In: 
*	attributes:	attributes for this frameset
* Returns:
*	a newly created frameset.
* Note:
*	this routine inserts each frameset it creates in a linked list which
*	is used for stack purposes.
*****/
static frameSet*
doFrameSet(String attributes)
{
	frameSet *list, *tmp;
	String chPtr, tmpPtr, ptr;
	int i;

	/* nothing to do if no attributes */
	if(attributes == NULL)
		return(frame_sets);

	/* create new entry */
	list = (frameSet*)malloc(sizeof(frameSet));
	(void)memset(list, 0, sizeof(frameSet));

	list->type = ROW;

	if((chPtr = _XmHTMLTagGetValue(attributes, "rows")) == NULL)
	{
		if((chPtr = _XmHTMLTagGetValue(attributes, "cols")) == NULL)
		{
			/* useless sanity, should be catched upon entry */
			free(list);
			return(frame_sets);
		}
		else
			list->type = COL;
	}

	/*
	* count how many childs this frameset has: the no of childs is given by
	* the no of entries within the COLS or ROWS tag
	* Note that childs can be frames and/or framesets as well.
	*/
	for(tmpPtr = chPtr; *tmpPtr != '\0'; tmpPtr++)
		if(*tmpPtr == ',')
			list->nchilds++;
	list->nchilds++;

	list->sizes = (int*)calloc(list->nchilds, sizeof(int));
	list->size_types = (FrameSize*)calloc(list->nchilds, sizeof(FrameSize));
	list->childs = (frameSet*)calloc(list->nchilds, sizeof(frameSet));

	/*****
	* get dimensions: when we encounter a ``*'' in a size definition it
	* means we are free to choose any size we want. When its a number 
	* followed by a ``%'' we must choose the size relative against the total
	* width of the render area. When it's a number not followed by anything
	* we have an absolute size.
	*****/
	tmpPtr = ptr = chPtr;
	i = 0;
	while(True)
	{
		if(*tmpPtr == ',' || *tmpPtr == '\0')
		{
			if(*(tmpPtr-1) == '*')
				list->size_types[i] = FRAME_SIZE_OPTIONAL;
			else if(*(tmpPtr-1) == '%')
				list->size_types[i] = FRAME_SIZE_RELATIVE;
			else
				list->size_types[i] = FRAME_SIZE_FIXED;

			list->sizes[i++] = atoi(ptr);

			if(*tmpPtr == '\0')
				break;
			ptr = tmpPtr+1;
		}
		tmpPtr++;
		/* sanity */
		if(i == list->nchilds)
			break;
	}
	free(chPtr);

	/*****
	* Frame borders can be specified by both frameborder or border, they
	* are equal.
	*****/
	if((chPtr = _XmHTMLTagGetValue(attributes, "frameborder")) != NULL)
	{
		/*
		* Sigh, stupid Netscape frameset definition allows a tag to have
		* a textvalue or a number.
		*/
		if(!(strcasecmp(chPtr, "no")) || *chPtr == '0')
			list->border = 0;
		else
			list->border = atoi(chPtr);
		free(chPtr);
	}
	else
		list->border = _XmHTMLTagGetNumber(attributes, "border", 5);

	/* insert this new frame in the overal frameset list. */
	if(frame_sets == NULL)
		frame_sets = list;
	else
	{
		for(tmp = frame_sets; tmp != NULL && tmp->next != NULL; 
			tmp = tmp->next);
		tmp->next = list;
	}

	/* create actual representation of frameset */
	{
	  XmHTMLFrameWidget *actualFrameSet = NULL ;
	  actualFrameSet = (XmHTMLFrameWidget*)calloc(1, sizeof(XmHTMLFrameWidget));
	  actualFrameSet->is_frameset = True ;
	  actualFrameSet->layout =
			(list->type == ROW ? FRAMESET_LAYOUT_ROWS : FRAMESET_LAYOUT_COLS);
	  list->actualFrameSet = actualFrameSet ;
	}
	return(list);
}

/*****
* Name: 		doFrame
* Return Type: 	XmHTMLFrameWidget*
* Description: 	fills a HTML frame structure with data from it's attributes
* In: 
*	html:		XmHTMLWidget id;
*	attributes:	frame attributes
* Returns:
*	updated frame
* Note:
*	this routine takes the frame to update from an already allocated list
*	of frames and increments the running frame counter when it returns.
*****/
static XmHTMLFrameWidget*
doFrame(XmHTMLWidget html, String attributes)
{
	XmHTMLFrameWidget *frame;
	String chPtr;

	frame = html->html.frames[current_frame];

	/* default frame sizing & scrolling */
	frame->size_type = FRAME_SIZE_FIXED;
	frame->scroll_type = FRAME_SCROLL_AUTO;

	/* get frame name, default to _frame if not present */
	if(!attributes ||
		(frame->name = _XmHTMLTagGetValue(attributes, "name")) == NULL)
	{
		char buf[24];
		sprintf(buf, "_frame%i", current_frame);
		frame->name = strdup(buf);
	}

	/* pick up all remaining frame attributes */
	if(attributes)
	{
		frame->src = _XmHTMLTagGetValue(attributes, "src");
		frame->margin_width = (Dimension)_XmHTMLTagGetNumber(attributes,
			"marginwidth", 5);
		frame->margin_height = (Dimension)_XmHTMLTagGetNumber(attributes,
			"marginheight", 5);

		/* inherit margins from parent if we'd gotten an invalid spec */
		if(!frame->margin_width)
			frame->margin_width = html->html.margin_width;
		if(!frame->margin_height)
			frame->margin_height = html->html.margin_height;

		/*
		* This is useless as we don't support frame resizing. I think this is
		* a thing the caller must be able to do. A possible way could be to
		* overlay the render area with a PanedWidget and store these HTML
		* widgets as childs of this paned widget.
		*/
		frame->resize = !_XmHTMLTagCheck(attributes, "noresize");

		/* what about scrolling? */
		if((chPtr = _XmHTMLTagGetValue(attributes, "scrolling")) != NULL)
		{
			if(!(strcasecmp(chPtr, "yes")))
				frame->scroll_type = FRAME_SCROLL_YES;
			else if(!(strcasecmp(chPtr, "no")))
				frame->scroll_type = FRAME_SCROLL_NONE;
			free(chPtr);
		}
	}
	else
	{
		frame->src           = NULL;
		frame->margin_width  = 5;
		frame->margin_height = 5;
		frame->resize        = True;
	}

	_XmHTMLDebug(11, ("frames.c: doFrame, frame %i created\n"
		"\tname: %s\n"
		"\tsrc : %s\n"
		"\tmargin width : %i\n"
		"\tmargin height: %i\n"
		"\tresize       : %s\n"
		"\tscrolling    : %s\n", current_frame, frame->name,
		frame->src ? frame->src : "<none>", frame->margin_width,
		frame->margin_height, frame->resize ? "yes" : "no",
		frame->scroll_type == FRAME_SCROLL_AUTO ? "auto" :
		(frame->scroll_type == FRAME_SCROLL_YES ? "always" : "none")));
		
	/*
	* Actual widget creation is postponed until the very last moment
	* of _XmHTMLCreateFrames
	*/

	/* increment running frame counter */
	current_frame++;
	return(frame);
}

/*****
* Name: 		insertFrameSetChild
* Return Type: 	void
* Description: 	inserts a child frameset in it's parent list
* In: 
*	parent:		parent of this frameset
*	child:		obvious
* Returns:
*	nothing
*****/
static void
insertFrameSetChild(frameSet *parent, frameSet *child)
{
	if(parent && parent->childs_done < parent->nchilds)
	{
		int idx = parent->childs_done;
		XmHTMLFrameWidget *c, *dad, *son;

		child->parent = parent;
		child->insert_pos = idx;

		dad = parent->actualFrameSet;
		son = child->actualFrameSet;

		son->size_s = parent->sizes[child->insert_pos];
		son->size_type = parent->size_types[child->insert_pos];

		if(son->size_s == 0)
			son->size_type = FRAME_SIZE_OPTIONAL;

		/* set additional constraints for this frame */
		son->border = parent->border;

		/* disable resizing if we don't have a border */
		if(!son->border)
			son->resize = False;

		for(c = dad->children ; c != NULL ; c = c->next)
			if(!c->next)
				break;
		if(c)
			c->next = son;
		else
			dad->children = son ;
		son->prev = c ;
		son->frameset = dad ;

		parent->childs[parent->childs_done] = *child;
		parent->childs_done++;
	}
}

/*****
* Name: 		insertFrameChild
* Return Type: 	void
* Description: 	sets the geometry constraints on a HTML frame
* In: 
*	frame_set:	frameset parent of this frame;
*	frame:		frame for which to set the constraints
* Returns:
*	nothing, but frame is updated.
*****/
static void
insertFrameChild(frameSet *frame_set, XmHTMLFrameWidget *frame)
{
	XmHTMLFrameWidget *c, *dad;
	int insert_pos = frame_set->childs_done;

	frame->size_s = frame_set->sizes[insert_pos];
	frame->size_type = frame_set->size_types[insert_pos];

	if(frame->size_s == 0)
		frame->size_type = FRAME_SIZE_OPTIONAL;

	/* set additional constraints for this frame */
	frame->border = frame_set->border;

	/* disable resizing if we don't have a border */
	if(!frame->border)
	  frame->resize = False;
	
	dad = frame_set->actualFrameSet;
	for(c = dad->children ; c != NULL ; c = c->next)
		if(!c->next)
			break;
	if(c)
		c->next = frame;
	else
		dad->children = frame;
	frame->prev = c;
	frame->frameset = dad;

	frame_set->childs_done++;
}

/*****
* Name: 		makeFrameSets
* Return Type: 	void
* Description: 	creates all HTML framesets and sets the geometry constraints
*				on each frame.
* In: 
*	html:		XmHTMLWidget id;
*	frameset:	XmHTMLObject data;
* Returns:
*	nothing
* Note:
*	This routine was *very* difficult to conceive, so don't let the simplicity
*	of it deceive you.
*****/
static void
makeFrameSets(XmHTMLWidget html, XmHTMLObject *frameset)
{
	XmHTMLObject *tmp;
	XmHTMLFrameWidget *frame;
	frameSet *current_set = NULL, *parent_set = NULL;
	int idx = 0;

	for(tmp = frameset; tmp != NULL; tmp = tmp->next)
	{
		switch(tmp->id)
		{
			case HT_FRAMESET:
				if(tmp->is_end)
				{
					/* frameset terminated, pop from stack */
					current_set = popFrameSet();
					/*
					* no more sets left on the stack: we've reached the
					* end of the outermost frameset and are done here.
					*/
					if(current_set == NULL)
						return;
				}
				else
				{
					/* A new frameset, push the current frameset on the stack */
					pushFrameSet(current_set);
					parent_set = frame_stack->frame_set;

					/* Check if we still have room for this thing. */
					if(!parent_set ||
						parent_set->childs_done < parent_set->nchilds)
					{
						/* create a new frameset */
						current_set = doFrameSet(tmp->attributes);
						insertFrameSetChild(parent_set, current_set);
						idx = 0;
					}
					else
					{
						/*
						* No more room available, this is an unspecified
						* frameset, kill it and all childs it might have.
						*/
						int depth = 1;
						int start_line = tmp->line;
						for(tmp = tmp->next; tmp != NULL; tmp = tmp->next)
						{
							if(tmp->id == HT_FRAMESET)
							{
								if(tmp->is_end)
								{
									if(--depth == 0)
										break;
								}
								else	/* child frameset */
									depth++;
							}
						}
						_XmHTMLWarning(__WFUNC__(html, "doFrameSets"),
							XMHTML_MSG_58, start_line, tmp ? tmp->line : -1);
					}
				}
				break;
			case HT_FRAME:
				/* check if we have room left */
				if(current_set->childs_done < current_set->nchilds)
				{
					/* insert child in current frameset */
					frame = doFrame(html, tmp->attributes);
					insertFrameChild(current_set, frame);
					idx++;
				}
				else
					_XmHTMLWarning(__WFUNC__(html, "doFrameSets"),
						XMHTML_MSG_59, tmp->line);
				/*****
				* Note: </FRAME> doesn't exist. The parser is smart enough
				* to kick these out.
				*****/
				/* fall thru */
			default:
				break;
		}
		if(idx == html->html.nframes)
			return;
	}
}

static XmHTMLFrameWidget*
getRootFrameset(XmHTMLWidget html)
{
	XmHTMLFrameWidget *frame;

	for (frame = html->html.frames[0];
		frame != NULL && frame->frameset != NULL; frame = frame->frameset);

	return(frame);
}


static void
adjustFramesetRows(XmHTMLFrameWidget *parent, int *p_width, int *p_height)
{
	XmHTMLFrameWidget *child = NULL ;
	int width, height ;
	int cum_fixed_size = 0, cum_rel_size = 0, cum_opt_size = 0 ;

	/* Begin with fixed-sized children */
	cum_fixed_size = 0 ;
	for (child = parent->children ; child != NULL ; child = child->next)
	{
		if(IS_FRAME_SIZE_FIXED(child)) 
		{
			width = *p_width ;
			height = child->size_s ;

			adjustFrame(child, &width, &height);

			child->width = width ;
			child->height = height ;
			cum_fixed_size += height ;
		}
	}

	/* Then do relative-sized children */
	cum_rel_size = 0 ;
	for (child = parent->children ; child != NULL ; child = child->next)
	{
		if(IS_FRAME_SIZE_RELATIVE(child)) 
		{
			width = *p_width ;
			height = child->size_s * (*p_height) / 100 ;

			adjustFrame(child, &width, &height);

			child->width = width ;
			child->height = height ;
			cum_rel_size += height ;
		}
	}

	/* Finally, end up with optional-sized children */
	cum_opt_size = 0 ;
	{
		int nb_opt = 0;

		/* count how many optional they are */
		for (child = parent->children ; child != NULL ; child = child->next)
			if(IS_FRAME_SIZE_OPTIONAL(child)) 
				++nb_opt;
    
		if(nb_opt > 0)
		{
			int cum_size, remain_size, mean_opt_size ;

			/*****
			* stupid hack : equal sizes for all optional fields.
			* FIXME! find sth smarter than that!
			*****/
			cum_size = cum_fixed_size + cum_rel_size ;
			remain_size = *p_height - cum_size ;
			if(remain_size <= nb_opt)
				remain_size = nb_opt ;
			mean_opt_size = remain_size / nb_opt ;

			/* go adjust */
			for(child = parent->children ; child != NULL ; child = child->next)
			{
				if(IS_FRAME_SIZE_OPTIONAL(child)) 
				{
					width = *p_width ;
					height = mean_opt_size ;
	    
					adjustFrame(child, &width, &height);
	    
					child->width = width ;
					child->height = height ;
					cum_opt_size += height ;	    
				}
			}
		}
	} /* end of optional-sized children mgt */

#ifdef FEEDBACK_SIZES
	*p_height = cum_fixed_size + cum_rel_size + cum_opt_size ;
	if(*p_height <= 0)
		*p_height = 1 ;
#endif
}

static void
adjustFramesetColumns(XmHTMLFrameWidget *parent, int *p_width, int *p_height)
{
	XmHTMLFrameWidget *child = NULL ;
	int width, height ;
	int cum_fixed_size = 0, cum_rel_size = 0, cum_opt_size = 0 ;

	/* Begin with fixed-sized children */
	cum_fixed_size = 0 ;
	for(child = parent->children ; child != NULL ; child = child->next)
	{
		if(IS_FRAME_SIZE_FIXED(child)) 
		{
			width = child->size_s ;
			height = *p_height ;

			adjustFrame(child, &width, &height);

			child->width = width ;
			child->height = height ;
			cum_fixed_size += width ;
		}
	}

	/* Then do relative-sized children */
	cum_rel_size = 0 ;
	for (child = parent->children ; child != NULL ; child = child->next)
    {
		if(IS_FRAME_SIZE_RELATIVE(child)) 
		{
			width = child->size_s * (*p_width) / 100 ;
			height = *p_height ;

			adjustFrame(child, &width, &height);

			child->width = width ;
			child->height = height ;
			cum_rel_size += width ;
		}
	}

	/* Finally, end up with optional-sized children */
	cum_opt_size = 0 ;
	{
		int nb_opt = 0;

		/* count how many optional they are */
		for (child = parent->children ; child != NULL ; child = child->next)
			if(IS_FRAME_SIZE_OPTIONAL(child)) 
				++nb_opt;
    
		if(nb_opt > 0)
		{
			int cum_size, remain_size, mean_opt_size ;

			/*****
			* stupid hack : equal sizes for all optional fields.
			* FIXME! find sth smarter than that!
			*****/
			cum_size = cum_fixed_size + cum_rel_size ;
			remain_size = *p_width - cum_size ;
			if(remain_size <= nb_opt)
				remain_size = nb_opt ;
			mean_opt_size = remain_size / nb_opt ;

			/* go adjust */
			for(child = parent->children ; child != NULL ; child = child->next)
			{
				if(IS_FRAME_SIZE_OPTIONAL(child)) 
				{
					width = mean_opt_size ;
					height = *p_height ;
	    
					adjustFrame(child, &width, &height);
	    
					child->width = width ;
					child->height = height ;
					cum_opt_size += width ;	    
				}
			}
		}
	} /* end of optional-sized children mgt */

#ifdef FEEDBACK_SIZES
	*p_width = cum_fixed_size + cum_rel_size + cum_opt_size ;
	if(*p_width <= 0)
		*p_width = 1 ;
#endif
}

static void
adjustFrame(XmHTMLFrameWidget *parent, int *p_width, int *p_height)
{
	if(*p_width <= 0)
		*p_width = 1 ;
	if(*p_height <= 0)
		*p_height = 1 ;
  
	if(IS_FRAMESET(parent)) /* do recursion only if it is a frameset */
	{
		if(parent->layout == FRAMESET_LAYOUT_ROWS)
			adjustFramesetRows(parent, p_width, p_height);
		else if(parent->layout == FRAMESET_LAYOUT_COLS)
			adjustFramesetColumns(parent, p_width, p_height);
	}
}

static void 
locateFrame(XmHTMLFrameWidget *parent, int x, int y)
{
	parent->x = x;
	parent->y = y;

	if(IS_FRAMESET(parent)) /* do recursion only if it is a frameset */
	{
		XmHTMLFrameWidget *frame ;

		if(IS_FRAMESET_LAYOUT_ROWS(parent))
		{
			for(frame = parent->children ; frame != NULL ; frame = frame->next)
			{
				locateFrame(frame, x, y);
				y += frame->height ;
			}
		}
      
		if(IS_FRAMESET_LAYOUT_COLS(parent))
		{
			for(frame = parent->children ; frame != NULL ; frame = frame->next)
			{
				locateFrame(frame, x, y);
				x += frame->width ;
			}
		}
	}
}


static void
adjustConstraints(XmHTMLWidget html)
{
	XmHTMLFrameWidget *root_frame;
	int work_width, work_height;
  
	/* this uses the core dimensions */
	work_width = html->core.width;
	work_height = html->core.height;
  
	/* get the root frame */
	root_frame = getRootFrameset(html);

	/* adjust frames' dimensions */
	adjustFrame(root_frame, &work_width, &work_height);

	/* adjust frames' positions */
	locateFrame(root_frame, 0, 0);
}


/*****
* Name: 		destroyFrameSets
* Return Type: 	void
* Description: 	destroys the memory used by the framesets
* In: 
*	set:		list of framesets to be destroyed
* Returns:
*	nothing
*****/
static void
destroyFrameSets(frameSet *set)
{
	frameSet *tmp;

	while(set)
	{
		tmp = set->next;
		if(set->sizes)
			free(set->sizes);
		if(set->size_types)
			free(set->size_types);
		if(set->childs)
			free(set->childs);
		free(set);
		set = tmp;
	}
	set = NULL;
}

/*****
* Name: 		mapFrames
* Return Type: 	void
* Description: 	map's all XmHTML frame childs to screen
* In: 
*	html:		XmHTMLWidget id
* Returns:
*	nothing
*****/
static void
mapFrames(XmHTMLWidget html)
{
	XmHTMLFrameWidget *frame;
	int i;

	/* map all XmHTML frame childs */
	for(i = 0; i < html->html.nframes; i++)
	{
		frame = html->html.frames[i];
		/* map to screen */
		HTML_ATTR(tka)->SetMappedWhenManaged(frame->frame, True);
		/* call notifier */
		_XmHTMLFrameDoneCallback(html, frame, frame->frame);
	}
	/* resync */
	if(HTML_ATTR(gc))
		HTML_ATTR(tka)->Sync(HTML_ATTR(tka)->dpy, False);
}

/*****
* Name: 		frameDestroyCallback
* Return Type: 	void
* Description: 	frame destruction notifier
* In: 
*	html:		XmHTMLWidget id;
*	frame:		frame data;
* Returns:
*	nothing
*****/
static void
frameDestroyCallback(XmHTMLWidget html, XmHTMLFrameWidget *frame)
{
	int ret_val;

	if((ret_val = _XmHTMLFrameDestroyCallback(html, frame)) == -1)
		return;

	/* always destroy this */
	if(frame->src) {
	  free(frame->src);
	  frame->src = NULL; /* sanity */
	}
	if(frame->name) {
	  free(frame->name);
	  frame->name = NULL; /* sanity */
	}
	frame->frameset = NULL; /* sanity */

	/* return if we may not destroy this frame */
	if(ret_val == 0)
	{
		/* destroy frame data, but keep the widget alive */
		free(frame);
		frame = NULL;
		return;
	}

	/* destroy everything */
	if(frame->frame)
		HTML_ATTR(tka)->DestroyWidget(frame->frame);
	free(frame);
	frame = NULL;
}

static void
recursiveDestroyFrameset(XmHTMLFrameWidget *frame)
{
	if (!frame) /* sanity */
    return ;

	if (IS_FRAMESET(frame))
	{
		XmHTMLFrameWidget *child, *tmp ;
		for(child = frame->children ; child != NULL ; )
		{
			tmp = child->next;
			recursiveDestroyFrameset(child);
			child = tmp ;
		}
		frame->children = NULL ;

		if(frame->src)
		{
			free(frame->src);
			frame->src = NULL; /* sanity */
		}
		if(frame->name)
		{
			free(frame->name);
			frame->name = NULL; /* sanity */
		}
		frame->frameset = NULL; /* sanity */

		free(frame);
		frame = NULL ;
	}
}

static Boolean
areAllSizesOptional(XmHTMLFrameWidget *frameset)
{
	Boolean all_opt = True ;
	if (IS_FRAMESET(frameset))
	{
   		XmHTMLFrameWidget *frame;

		for(frame = frameset->children ; frame != NULL ; frame = frame->next)
		{
			if(IS_FRAME_SIZE_OPTIONAL(frame))
			{
				all_opt = False;
				break;
			}
		}
	}
	return(all_opt);
}

static Boolean
areAllSizesRelative(XmHTMLFrameWidget *frameset)
{
	Boolean all_rel = False ;
	if(IS_FRAMESET(frameset))
	{
		XmHTMLFrameWidget *frame;

		all_rel = True ;
		for(frame = frameset->children ; frame != NULL ; frame = frame->next)
		{
			if (IS_FRAME_SIZE_RELATIVE(frame))
			{
				all_rel = False;
				break;
			}
		}
	}
	return(all_rel);
}

static int
relativeSizesSum(XmHTMLFrameWidget *frameset)
{
	int rel_sum = 0 ;
	if(IS_FRAMESET(frameset))
	{
		XmHTMLFrameWidget *frame;
		for(frame = frameset->children ; frame != NULL ; frame = frame->next)
		{
			if (IS_FRAME_SIZE_RELATIVE(frame))
			{
				rel_sum += frame->size_s ;
			}
		}
	}
	return(rel_sum);
}

/********
****** Public Functions
********/

/*****
* Name: 		_XmHTMLCheckForFrames
* Return Type: 	int
* Description: 	checks if the given list of objects contains HTML frames
* In: 
*	html:		XmHTMLWidget id;
*	objects:	parser output to check
* Returns:
*	no of frames found in the current document.
*****/
int
_XmHTMLCheckForFrames(XmHTMLWidget html, XmHTMLObject *objects)
{
	XmHTMLObject *tmp;
	int nframes = 0;

	/* we only support frames if user has attached a frame callback */
	if(!html->html.frame_callback)
		return(0);

	/*
	* frames are not allowed to appear inside the BODY tag.
	* So we never have to walk the entire contents of the current document
	* but simply break out of the loop once we encounter the <BODY> tag.
	* This is a fairly huge performance increase.
	*/
	for(tmp = objects; tmp != NULL && tmp->id != HT_BODY; tmp = tmp->next)
		if(tmp->id == HT_FRAME)
			nframes++;

	return(nframes);
}

/*****
* Name: 		_XmHTMLDestroyFrames
* Return Type: 	void
* Description: 	frame destroyer
* In: 
*	html:		XmHTMLWidget id
*	nframes:	no of frames to destroy;
* Returns:
*	nothing, but the frames list of the widget is destroyed.
*****/
void
_XmHTMLDestroyFrames(XmHTMLWidget html, int nframes)
{
	int i = 0;
	XmHTMLFrameWidget *root_frame = NULL;

	/* unmap all XmHTML frame childs */
	for(i = 0; i < html->html.nframes; i++)
		HTML_ATTR(tka)->SetMappedWhenManaged(html->html.frames[i]->frame,False);

	/* free them */
	root_frame = getRootFrameset(html);
	recursiveDestroyFrameset(root_frame);

	for(i = 0; i < nframes; i++)
	{
		frameDestroyCallback(html, html->html.frames[i]);
		html->html.frames[i] = NULL ;/* sanity */
	}
	free(html->html.frames);
	html->html.frames = NULL;
	html->html.nframes = 0;
}

/*****
* Name: 		_XmHTMLReconfigureFrames
* Return Type: 	void
* Description: 	resize method for XmHTML frame childs
* In: 
*	html:		XmHTMLWidget id
* Returns:
*	nothing
*****/
void
_XmHTMLReconfigureFrames(XmHTMLWidget html)
{
	XmHTMLFrameWidget *frame;
	int i;

	_XmHTMLDebug(11, ("frames.c: _XmHTMLReconfigureFrames Start\n"));
	/* compute new screen positions */
	adjustConstraints(html);

	/* reconfigure all widgets */
	for(i = 0; i < html->html.nframes; i++)
	{
		frame = html->html.frames[i];

		_XmHTMLDebug(11, ("frames.c: _XmHTMLReconfigureFrames doing frame "
			"%s.\n", frame->name));

		HTML_ATTR(tka)->ConfigureWidget(frame->frame, frame->x, frame->y,
			frame->width - frame->border,
			frame->height - frame->border, frame->border);
	}
	_XmHTMLDebug(11, ("frames.c: _XmHTMLReconfigureFrames End.\n"));
}

/*****
* Name:			_XmHTMLCreateFrame
* Return Type: 	Widget
* Description: 	creates a htmlWidgetClass widget for use in HTML frames.
* In: 
*	html:		parent XmHTMLWidget id;
*	frame:		data for frame (dimensions, name, ...)
*	fptr:		callback data from the XmNframeCallback callback function.
* Returns:
*	A newly created XmHTMLWidget.
*****/
Widget
_XmHTMLCreateFrame(XmHTMLWidget html, XmHTMLFrameWidget *frame,
	XmHTMLFrameCallbackStruct *fptr)
{
	Arg args[20];
	Dimension argc = 0;
	static Widget widget;
	XmHTMLWidget html_widget;
	ToolkitAbstraction *tka = HTML_ATTR(tka); 

	/* set constraints and other frame stuff */
	XtSetArg(args[argc], XmNx, frame->x); argc++;
	XtSetArg(args[argc], XmNy, frame->y); argc++;
	XtSetArg(args[argc], XmNwidth, frame->width - frame->border); argc++;
	XtSetArg(args[argc], XmNheight, frame->height - frame->border); argc++;
	XtSetArg(args[argc], XmNmarginWidth, frame->margin_width); argc++;
	XtSetArg(args[argc], XmNmarginHeight, frame->margin_height); argc++;
	XtSetArg(args[argc], XmNborderWidth, frame->border); argc++;
	XtSetArg(args[argc], XmNborderColor, html->manager.top_shadow_color);argc++;
	XtSetArg(args[argc], XmNmappedWhenManaged, False); argc++;

	/* scrolling gets handled in the widget code itself, so don't set it */

	/*
	* Create when we have to, the widget is NULL or the widget isn't a
	* XmHTML widget.
	*/
	if(fptr->doit == True || fptr->html == NULL)
		widget = XmCreateHTML(HTML_ATTR(work_area), fptr->name, args, argc);
	else if(!XmIsHTML(fptr->html))
	{
		/* not a HTML widget, spit out a warning and create one ourselves */
		_XmHTMLWarning(__WFUNC__(fptr->html, "_XmHTMLFrameCreateCallback"),
			XMHTML_MSG_60);
		widget = XmCreateHTML(HTML_ATTR(work_area), fptr->name, args, argc);
	}
	else
	{
		widget = fptr->html;

		/* first unmanage if it's still up */
		if(tka->IsManaged(widget))
			tka->UnmanageChild(widget);

		/* check if we need to clear any existing source */
		if(ATTR_HTML(widget, source) != NULL)
		{
			XtSetArg(args[argc], XmNvalue, NULL);
			argc++;
		}

		/* reconfigure this widget so it'll fit our purposes */
		XtSetValues(widget, args, argc);

		/* unmanage scrollbars as well */
		ATTR_HTML(widget, needs_vsb) = False;
		ATTR_HTML(widget, needs_hsb) = False;
		tka->UnmanageChild(ATTR_HTML(widget, hsb));
		tka->UnmanageChild(ATTR_HTML(widget, vsb));
	}
	
	html_widget = (XmHTMLWidget)widget;
	ATTR_HTML(html_widget, is_frame) = True;
	ATTR_HTML(html_widget, frame_border) = frame->border;
	ATTR_HTML(html_widget, scroll_type) = frame->scroll_type;

	/* manage it */
	tka->ManageChild(widget);

	return(widget);
}

/*****
* Name:			_XmHTMLCreateFrames
* Return Type:	Boolean
* Description:	main frame creator
* In:
*	html_old:	previous XmHTMLWidget id;
*	html:		XmHTMLWidget id;
* Returns:
*	True when all frames could be created, False otherwise.
*****/
Boolean
_XmHTMLCreateFrames(XmHTMLWidget old, XmHTMLWidget html)
{
	int i;
	XmHTMLObject *tmp;
	static Widget frame;

	frame_stack = &frame_base;
	frame_stack->next = NULL;
	frame_stack->frame_set = NULL;

	/* first destroy all previous frames of this widget */
	if(old && old->html.nframes)
		_XmHTMLDestroyFrames(old, old->html.nframes);

	if(frame_sets)
		destroyFrameSets(frame_sets);
	frame_sets = NULL;

	/*
	* Don't do a thing if we are destroying the previous list, we don't have
	* a frame callback or the new widget doesn't have any frames at all
	*/
	if(html == NULL || !html->html.frame_callback || html->html.nframes == 0)
		return(False);

	frame = NULL;

	/* create the list of HTML frame childs */
	html->html.frames = (XmHTMLFrameWidget**)calloc(html->html.nframes,
		sizeof(XmHTMLFrameWidget*));

	/* create individual HTML frame child ptrs */
	for(i = 0; i < html->html.nframes; i++)
	{
		XmHTMLFrameWidget *frame_w;
		frame_w = (XmHTMLFrameWidget*)malloc(sizeof(XmHTMLFrameWidget));
		(void)memset(frame_w, 0, sizeof(XmHTMLFrameWidget));
		html->html.frames[i] = frame_w;
	}

	/* move to the first frameset declaration */
	for(tmp = html->html.elements; tmp != NULL && tmp->id != HT_FRAMESET; 
		tmp = tmp->next);

	current_frame = 0;

	/* create all frames (and possibly nested framesets also) */
	makeFrameSets(html, tmp);

	/* adjust framecount, makeFrameSets might have found some invalid sets */
	html->html.nframes = current_frame;

#ifdef DEBUG
	_XmHTMLDebug(11, ("frames.c: _XmHTMLCreateFrames, raw frame listing\n"));
	for(i = 0; i < html->html.nframes; i++)
	{
		_XmHTMLDebug(11, ("frame %i\n"
			"\tname           : %s\n"
			"\tsrc            : %s\n"
			"\tsize           : %i\n",
			i, html->html.frames[i]->src, html->html.frames[i]->name,
			html->html.frames[i]->size_s));
	}
#endif

	adjustConstraints(html);

#ifdef DEBUG
	_XmHTMLDebug(11, ("frames.c: _XmHTMLCreateFrames, adjusted frame "
		"listing\n"));
	for(i = 0; i < html->html.nframes; i++)
	{
		_XmHTMLDebug(11, ("frame %i\n"
			"\tname           : %s\n"
			"\tsrc            : %s\n"
			"\twidth by height: %ix%i\n"
			"\tx offset       : %i\n"
			"\ty offset       : %i\n",
			i, html->html.frames[i]->src, html->html.frames[i]->name,
			html->html.frames[i]->width, html->html.frames[i]->height,
			html->html.frames[i]->x, html->html.frames[i]->y));
	}
#endif
	/* and now create all frames */
	for(i = 0; i < html->html.nframes; i++)
	{
		html->html.frames[i]->frame = _XmHTMLFrameCreateCallback(html,
			html->html.frames[i]);
	}
	/* erase a few glitches by calling adjustConstraints again */
	_XmHTMLReconfigureFrames(html);

	/* and now map them to screen */
	mapFrames(html);

	return(True);
}

/* doesn't work yet */
#if 0
void
_XmHTMLDrawFrameBorder(XmHTMLWidget html)
{
	int x = html->core.x;
	int y = html->core.y;
	int width = html->html.frame_border;
	int height = html->core.height;
	Display *dsp = XtDisplay((Widget)html);
	GC gc;
	Window win = XtWindow((Widget)html);
	
	gc = html->manager.bottom_shadow_GC;
	XFillRectangle(dsp, win, gc, x, y, width, 1);
	XFillRectangle(dsp, win, gc, x, y, 1, height-1);

	gc = html->manager.top_shadow_GC;
	XFillRectangle(dsp, win, gc, x+1, y + height-1, width-1, 1);
	XFillRectangle(dsp, win, gc, x + width - 1, y + 1, 1, height-2);
}
#endif

/********
****** Public XmHTML Functions
********/

/*****
* Name: 		XmHTMLFrameGetChild
* Return Type: 	Widget
* Description: 	returns the Widget id of a frame child given it's name.
* In: 
*	w:			XmHTMLWidget
*	name:		name of frame to locate.
* Returns:
*	If found, the widget id of the requested frame, NULL otherwise. 
*****/
Widget
XmHTMLFrameGetChild(Widget w, String name)
{
	XmHTMLWidget html;
	int i;

	/* sanity check */
	if(!w || !XmIsHTML(w) || name == NULL)
	{
		String func = "FrameGetChild";
		if(name == NULL)
			_XmHTMLWarning(__WFUNC__(w, func),
				XMHTML_MSG_21, "NULL frame name", func);
		else
			_XmHTMLBadParent(w, func);
		return(NULL);
	}

	html = (XmHTMLWidget)w;

	for(i = 0; i < html->html.nframes; i++)
	{
		if(!(strcmp(html->html.frames[i]->name, name)))
			return(html->html.frames[i]->frame);
	}
	return(NULL);
}
