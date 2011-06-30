#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* stack.c : Generic Stack routines
*
* This file Version	$Revision$
*
* Creation date:		Thu Apr  2 12:26:46 GMT+0100 1998
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
* ChangeLog 
* $Log$
* Revision 1.1  2011/06/30 16:10:38  rwcox
* Cadd
*
* Revision 1.1  1998/04/04 06:27:25  newt
* Initial Revision
*
*****/ 
#include <stdio.h>
#include <stdlib.h>

#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif /* WITH_DMALLOC */

#include "stack.h"

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/* external data destructor */
typedef void (*destructorProc)(void*);

/* a single data element on the stack */
typedef struct stackRec{
	void *data;
	struct stackRec *next;
}StackRec;

/* full stack definition (including housekeeping) */
typedef struct _StackRegistry{
	int size;						/* current stack size					*/
	int is_double;					/* double stack?						*/
	StackRec base[2];				/* first element in stack (fallback)	*/
	StackRec *stack;				/* stacked data							*/
	StackRec *stack2;				/* double stack							*/
	destructorProc destroyer;		/* data destructor proc					*/
	destructorProc destroyer2;		/* data destructor proc					*/
}StackRegistry;

/*** Private Function Prototype Declarations ****/

/*** Private Variable Declarations ***/

/*****
* Name:			StackCreate
* Return Type:	Stack
* Description:	create a stack
* In:
*	fallback..:	fallback data to be returned when stack is becoming
*				negative;
*	destroyer:	function to use to destroy the stacked data.
* Returns:
*	a new stack
*****/
Stack
StackCreate(void *fallback_data, destructorProc destroyer)
{
	static Stack stack;

	/* allocate a new stack */
	if((stack = (Stack)calloc(1, sizeof(StackRegistry))) == NULL)
		return(NULL);

	/* initially empty */
	stack->size = 0;
	stack->base[0].data = fallback_data;
	stack->stack = &(stack->base[0]);
	stack->destroyer = destroyer;

	return(stack);
}

/*****
* Name:			StackCreateDouble
* Return Type:	Stack
* Description:	create a double stack
* In:
*	fallback..:	fallback data to be returned when stack is becoming
*				negative;
*	destroyer:	function to use to destroy the stacked data.
* Returns:
*	a new stack
*****/
Stack
StackCreateDouble(void *fallback_data1, void *fallback_data2,
	destructorProc destroyer, destructorProc second_destroyer)
{
	static Stack stack;

	/* allocate a new stack */
	if((stack = (Stack)calloc(1, sizeof(StackRegistry))) == NULL)
		return(NULL);

	/* initially empty */
	stack->size = 0;
	stack->is_double = 1;
	stack->base[0].data = fallback_data1;
	stack->base[1].data = fallback_data2;
	stack->stack  = &(stack->base[0]);
	stack->stack2 = &(stack->base[1]);
	stack->destroyer  = destroyer;
	stack->destroyer2 = second_destroyer;

	return(stack);
}
/*****
* Name:			StackDestroy
* Return Type:	int
* Description:	Destroy the given stack (and any data remaining)
* In: 
*	stack:		stack to be destroyed
* Returns:
*	no of items that remained on the stack.
*****/
int
StackDestroy(Stack stack)
{
	int popped = 0;

	/* clear stack */
	if(stack->size != 0)
	{
		/*****
		* Split into two parts for performance reasons
		*****/ 
		if(stack->destroyer != NULL)
		{
			/* pop & destroy data */
			while(stack->size != 0)
			{
				stack->destroyer(StackPopData(stack));
				popped++;
			}
		}
		else
		{
			while(stack->size != 0)
			{
				(void)StackPopData(stack);
				popped++;
			}
		}
		/* release fallback data as well */
		if(stack->destroyer)
		{
			stack->destroyer(stack->base[0].data);
			if(stack->destroyer2)
				stack->destroyer2(stack->base[1].data);
		}
	}

	/* free this stack */
	free(stack);

	return(popped);
}

/*****
* Name:			StackPushData
* Return Type: 	int
* Description: 	push the given data on the given stack.
* In: 
*	stack:		current stack
*	data:		data to be pushed. Should be cast to void* by the caller.
* Returns:
*	1 when element was successfully pushed, 0 if not.
*****/
int
StackPushData(Stack stack, void *data)
{
	StackRec *rec;

	if((rec = (StackRec*)malloc(sizeof(StackRec))) == NULL)
		return(0);
	rec->data = data;
	rec->next = stack->stack;
	stack->stack = rec;
	stack->size++;
	return(1);
}

/*****
* Name: 		StackPopData
* Return Type: 	void*
* Description:	Pop data from the stack
* In: 
*	stack:		stack to be popped.
* Returns:
*	popped data. Should be cast to the appropriate type by the caller
*****/
void*
StackPopData(Stack stack)
{
	void *data;

	/* do we have a next element? */
	if(stack->size != 0)
	{
		/* yes we have, pop it */
		StackRec *rec = stack->stack;
		stack->stack = stack->stack->next;
		data = rec->data;
		free(rec);
		stack->size--;
	}
	else
	{
		/* nope, stack becoming negative. Return default data */
		data = stack->base[0].data;
	}
	return(data);
}

/*****
* Name:			StackDoublePushData
* Return Type: 	int
* Description: 	push the given data on the given stack.
* In: 
*	stack:		current stack
*	data1:		data to be pushed. Should be cast to void* by the caller.
*	data2:		data to be pushed. Should be cast to void* by the caller.
* Returns:
*	1 when element was successfully pushed, 0 if not.
*****/
int
StackPushDoubleData(Stack stack, void *data1, void *data2)
{
	StackRec *rec, *rec1;

	if((rec = (StackRec*)calloc(2,sizeof(StackRec))) == NULL)
		return(0);
	rec1         = rec + 1;
	rec->data    = data1;
	rec1->data   = data2;
	rec->next    = stack->stack;
	stack->stack = rec;
	stack->size++;
	return(1);
}

/*****
* Name: 		StackPopDoubleData
* Return Type: 	void*
* Description:	Pop data from the stack
* In: 
*	stack:		stack to be popped.
*	data:		associated data to be popped. 
* Returns:
*	popped data. Should be cast to the appropriate type by the caller
*****/
void*
StackPopDoubleData(Stack stack, void **data)
{
	void *ret_val;

	/* do we have a next element? */
	if(stack->size != 0)
	{
		/* yes we have, pop it */
		StackRec *rec = stack->stack;
		stack->stack = stack->stack->next;
		ret_val = rec->data;
		*data = (rec+1)->data;
		free(rec);
		stack->size--;
	}
	else
	{
		/* nope, stack becoming negative. Return default data */
		ret_val = stack->base[0].data;
		*data   = stack->base[1].data;
	}
	return(ret_val);
}

/*****
* Name: 		StackSize
* Return Type: 	int
* Description: 	return the size of the given stack
* In: 
*	stack:		current stack
* Returns:
*	size of the stack.
*****/
int StackSize(Stack stack)
{
	return(stack->size);
}
