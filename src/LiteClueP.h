/* 
LiteClueP.h - Private definitions for LiteClue widget
	See LiteClue documentation

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

*/
/* Revision History:
$Log$
Revision 1.1  1997/11/05 01:14:31  cox
Initial revision

Revision 1.2  1997/06/15 14:08:14  gary
Support for cancel wait period

Revision 1.1  1996/10/19 16:08:51  gary
Initial


$log
Support for cancel wait period
$log
*/

#ifndef _DEF_LiteClueP_h
#define _DEF_LiteClueP_h

#include <X11/ShellP.h>
/* Include public header file for this widget. */
#ifndef __VMS
# include <LiteClue.h>
#else
# include "LiteClue.h"
#endif
    
/* Doubly Linked List Processing */
struct list_thread_str
{
	struct list_thread_str * forw;	/* next pointer */
	struct list_thread_str * back;	/* prev pointer */
};
typedef struct list_thread_str  ListThread; 


typedef struct {
	int	nothing;	/* place holder */
} LiteClueClassPart;

/* Full class record declaration */
typedef struct _LiteClueClassRec {
	CoreClassPart	core_class;
	CompositeClassPart  composite_class;
	ShellClassPart  shell_class;
	OverrideShellClassPart  override_shell_class;
	LiteClueClassPart	LiteClue_class;
} LiteClueClassRec;

extern LiteClueClassRec xcgLiteClueClassRec;

/* New fields for the LiteClue widget record */
typedef struct {
	/* resources */
	Pixel foreground;

#if XtSpecificationRelease < 5
	XFontStruct *font;	/* the font for text in box */
#else
	XFontSet fontset;	/* the font for text in box */
#endif
        int  waitPeriod;	/* the delay resource - pointer must be
				   in watched widget this long before
				   help is poped - in millisecs
				*/
        int  cancelWaitPeriod;	/* after help is popped-down - normal
				   wait period is cancelled for this
				   period - in millisecs
				*/

	/* -------- private state --------- */
	ListThread widget_list; 	/* list of widgets we are liteClue-ing */
	Dimension font_width;	/* width of '1' character */
	Dimension font_height;	/* height of font, rows are spaced using this */
	Dimension font_baseline;	/* relative displacement to baseline from top */
	GC text_GC;		/* for drawing text */
	XtIntervalId interval_id;	/* New field, holds timer id */
	Boolean	HelpIsUp;	/* the help is popup is up */
	Time	HelpPopDownTime;	/* the time at which help popup was popped down */
} LiteCluePart;


/*
 * Full instance record declaration
 */
typedef struct _LiteClueRec {
	CorePart		core;
	CompositePart  composite;
	ShellPart 	shell;
	OverrideShellPart override;
	LiteCluePart	liteClue;
} LiteClueRec;


#endif /* _DEF_LiteClueP_h */

