/*****
* stack.h : generic stack routines
*
* This file Version	$Revision$
*
* Creation date:		Thu Apr  2 12:26:52 GMT+0100 1998
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
*
* Copyright (C) 1994-1998 by Ripley Software Development
* All Rights Reserved
*
* This file is part of the XmHTML Widget Library
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
* $Source$
*****/
/*****
* ChangeLog
* $Log$
* Revision 1.1  2011/06/30 16:08:57  rwcox
* Cadd
*
* Revision 1.1  1998/04/04 06:27:26  newt
* Initial Revision
*
*****/

#ifndef _stack_h_
#define _stack_h_

/* Stack is an opaque definition, defined in stack.c */
typedef struct _StackRegistry *Stack;

/*****
* Create a stack.
* fallback: data to return when the stack is becoming negative.
* destroy_data_func is a function that will be called when the stack
* is destroyed while items are still remaining on the stack.
*****/
extern Stack StackCreate(void *fallback, void (*destroy_data_func)(void*));

/* create a stack that can stack two sets of data */
extern Stack StackCreateDouble(void *fallback_data1, void *fallback_data2,
	void (*destroy_data_func)(void*),
	void (*second_destroy_data_func)(void*));

/*****
* Destroy the given stack (and any data remaining). Returns the no
* of items that still remained on the stack.
*****/
extern int StackDestroy(Stack stack);

/*****
* Push data onto a stack. data is the data to be pushed, and should be cast
* to void* by the caller. Returns 1 when data was successfully pushed, 0
* if not.
*****/
extern int StackPushData(Stack stack, void *data);

/* push two sets of data on the stack */
extern int StackPushDoubleData(Stack stack, void *data1, void *data2);

/*****
* Pop data from the stack. Returned data should be cast to an appropriate
* type by the caller.
*****/
extern void* StackPopData(Stack stack);

/* pop two sets of data from the stack */
extern void* StackPopDoubleData(Stack stack, void **data);

/* return size of the stack */
extern int StackSize(Stack stack);

/* convenient macros, take care of typecasting when pushing & popping data */
#define StackPush(S,D)				StackPushData(S,(void*)(D))

#define StackPop(S)					StackPopData(S)

#define StackPushDouble(S,D1,D2) \
	StackPushDoubleData(S,(void*)(D1), (void*)(D2))

#define StackPopDouble(S,D) \
	StackPopDoubleData(S,(void**)&(D))

/* Don't add anything after this endif! */
#endif /* _stack_h_ */
