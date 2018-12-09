/*****
* hash.h : generic hash routines
*
* This file Version	$Revision$
*
* Creation date:		Sat Oct 24 01:35:48 CEST 1998
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				XmHTML Developers Account
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
*
*****/

#ifndef _hash_h_
#define _hash_h_

/* Hashtable is an opaque type defined in hash.c */
typedef struct _HashTable HashTable;

/*****
* Hash key comparison function prototype. This function is optional,
* but if present, it should return a positive, non-zero value on a valid
* match. Otherwise it should return 0.
*****/
typedef int (*HashCompareFunc)(unsigned long, unsigned long);

/* Create a new hashtable */
extern HashTable *HashCreate(int hsize, HashCompareFunc comparer);

/* initialize the given hashtable. */
extern HashTable *HashInit(HashTable *table, int hsize, HashCompareFunc
	comparer);

/* put a new entry in the hashtable */
extern void HashPut(HashTable *table, unsigned long key, unsigned long data);

/* get an entry from the hashtable */
extern int HashGet(HashTable *table, unsigned long key, unsigned long *data);

/* delete an entry from the hashtable */
extern void HashDelete(HashTable *table, unsigned long key);

/* completely wipe the given hashtable */
extern void HashDestroy(HashTable *table);

/* Don't add anything after this endif! */
#endif /* _hash_h_ */

