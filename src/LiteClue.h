/* 
LiteClue.h - Public definitions for LiteClue widget
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

Revision 1.4  1997/06/15 14:07:56  gary
Added XcgLiteClueDispatchEvent

Revision 1.3  1997/04/14 13:03:25  gary
Added XgcNwaitperiod XgcNcancelWaitPeriod and c++ wrappers

Revision 1.2  1996/10/20 13:39:25  gary
Version 1.2 freeze

Revision 1.1  1996/10/19 16:08:04  gary
Initial


$log
Added XcgLiteClueDispatchEvent
$log
*/

#ifndef _DEF_LiteClue_h
#define _DEF_LiteClue_h
#include <X11/StringDefs.h>

/*
 * New resource names
 */

#define XgcNcancelWaitPeriod	 "cancelWaitPeriod"
#define XgcNwaitPeriod	 "waitPeriod"
/*
 * New resource classes
 */
#define XgcCCancelWaitPeriod	"cancelWaitPeriod"
#define XgcCWaitPeriod	"WaitPeriod"

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" { 
#endif

extern WidgetClass xcgLiteClueWidgetClass; 
typedef struct _LiteClueClassRec *XcgLiteClueWidgetClass;
typedef struct _LiteClueRec      *XcgLiteClueWidget;
void XcgLiteClueAddWidget(Widget w, Widget watch,  char * text, int size, int option );
void XcgLiteClueDeleteWidget(Widget w, Widget watch);
void XcgLiteClueSetSensitive(Widget w, Widget watch, Boolean sensitive);
Boolean XcgLiteClueGetSensitive(Widget w, Widget watch);
Boolean XcgLiteClueDispatchEvent(Widget w, XEvent  *event);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
