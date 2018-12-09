#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* hash.c : Generic hashing routines
*
* This file Version	$Revision$
*
* Creation date:		Sat Oct 24 01:09:35 CEST 1998
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
* ChangeLog
* $Log$
* Revision 1.1  2011/06/30 16:10:38  rwcox
* Cadd
*
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif

#include "hash.h"

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/* a single entry in the hashtable */
typedef struct _HashEntry{
	struct _HashEntry *nptr;
	struct _HashEntry *pptr;
	unsigned long key;
	unsigned long data;
	struct _HashEntry *next;	/* next on collision list */
}HashEntry;

/* The hashtable itself */
struct _HashTable{
	int elements;				/* elements in table		*/
	int size;					/* size of table			*/
	HashEntry **table;
	HashEntry *last;			/* last on the linked list	*/
	HashCompareFunc comparer;	/* data comparison function	*/
};

/*** Private Function Prototype Declarations ****/

/*** Private Variable Declarations ***/

/*****
* Private routines.
*****/

/*****
* Name: 		hashDestroy
* Return Type: 	void
* Description: 	frees the table of a given hashtable. Only used when table
*				is being rebuild.
* In:
*	table:		table to be destroyed;
* Returns:
*	nothing.
*****/
static void
hashDestroy(HashTable *table)
{
	HashEntry *entry, *next;
	int i;

	for (i=0; i<table->size; i++)
	{
		entry=table->table[i];
		while (entry)
		{
			next = entry->next;
			entry = next;
		}
	}
	free(table->table);
}

/*****
* Name: 		hashRemoveEntry
* Return Type: 	HashEntry
* Description: 	deletes a given entry from the given hashtable.
* In:
*	table:		table from which an entry should be deleted.
*	entry:		entry to be deleted;
*	key:		entry identifier
* Returns:
*	entry following the deleted entry. This can be non-null if a hashvalue
*	contains multiple keys.
*****/
static HashEntry *
hashRemoveEntry(HashTable *table, HashEntry *entry, unsigned long key)
{
	HashEntry *next;

	if(entry==NULL)
		return NULL;

	if((table->comparer && (*table->comparer)(entry->key, key)) ||
		entry->key == key)
	{
		if(table->last == entry)
			table->last = entry->pptr;
		if(entry->nptr)
			entry->nptr->pptr = entry->pptr;
		if(entry->pptr)
			entry->pptr->nptr = entry->nptr;
		next = entry->next;
		free(entry);
		return next;
	}
	entry->next = hashRemoveEntry(table, entry->next, key);
	return entry;
}

/*****
* Name: 		hashRebuild
* Return Type: 	void
* Description: 	enlarges & rebuilds the given hashtable. Used when the
*				size of the current hashtable is becoming to small to store
*				new info efficiently.
* In:
*	table:		table to rebuild
* Returns:
*	nothing.
*****/
static void
hashRebuild(HashTable *table)
{
	HashTable newtable;
	HashEntry *entry;
	int i;

	newtable.last = NULL;
	newtable.elements = 0;
	newtable.size = table->size*2;
	newtable.table = (HashEntry**)malloc(newtable.size * sizeof(HashEntry*));
	memset(newtable.table, 0, newtable.size * sizeof(HashEntry*));
	for (i=0; i<table->size; i++)
	{
		entry = table->table[i];
		while(entry)
		{
			HashPut(&newtable, entry->key, entry->data);
			entry=entry->next;
		}
	}
	hashDestroy(table);
	table->elements = newtable.elements;
	table->size = newtable.size;
	table->table = newtable.table;
}


/*************
****** Hashing
*************/

/*****
* Name: 		HashCreate
* Return Type: 	HashTable*
* Description: 	creates & initializes a new hashing table.
* In:
*	hsize:		initial hashtable size;
*	comparer:	key comparison function, optional. If present, a positive
*				match should return a non-zero value and a negative match
*				should return a zero value.
* Returns:
*	The newly created hashtable.
*****/
HashTable*
HashCreate(int hsize, HashCompareFunc comparer)
{
	static HashTable *table;

	/* allocate a new table */
	if((table = (HashTable*)malloc(sizeof(HashTable))) == NULL)
		return(NULL);

	/* initialize & return to caller */
	return(HashInit(table, hsize, comparer));
}

/*****
* Name: 		HashInit
* Return Type: 	HashTable
* Description: 	Initializes a hashtable with an initial size hsize.
*				The table must already be allocated.
* In:
*	table:		hashtable to be initialized;
*	hsize:		size of hashtable.
*	comparer:	key comparison function, optional.
* Returns:
*	initialized table.
*****/
HashTable *
HashInit(HashTable *table, int hsize, HashCompareFunc comparer)
{
	table->elements = 0;
	table->size  = hsize;
	table->table = (HashEntry**)malloc(hsize*sizeof(HashEntry*));
	table->last  = NULL;
	table->comparer = comparer;
	memset(table->table, 0, hsize * sizeof(HashEntry*));

	return(table);
}

/*****
* Name: 		HashPut
* Return Type: 	void
* Description: 	puts a new entry in the hash table
* In:
*	key:		handle to data to be stored;
*	data:		data to be stored;
* Returns:
*	nothing.
*****/
void
HashPut(HashTable *table, unsigned long key, unsigned long data)
{
	unsigned long hkey;
	HashEntry *nentry;

	nentry = (HashEntry*)malloc(sizeof(HashEntry));

	nentry->key = key;
	nentry->data = data;
	hkey = key % table->size;

	/* Aaie, collided */
	if (table->table[hkey]!=NULL)
	{
		nentry->next = table->table[hkey];
		table->table[hkey] = nentry;
    }
	else
	{
		nentry->next = NULL;
		table->table[hkey] = nentry;
    }
	table->elements++;

	nentry->nptr = NULL;
	nentry->pptr = table->last;
	if(table->last)
		table->last->nptr = nentry;
	table->last = nentry;

	if(table->elements > (table->size*3)/2)
	{
		/* humpf, table getting too small, resize and rebuild. */
		hashRebuild(table);
    }
}

/*****
* Name: 		HashGet
* Return Type: 	int
* Description: 	retrieves a hash entry.
* In:
*	key:		id of entry to retrieve;
*	*data:		object in which to store data reference;
* Returns:
*	True when entry was found, False if not.
*****/
int
HashGet(HashTable *table, unsigned long key, unsigned long *data)
{
	unsigned long hkey;
	HashEntry *entry;

	hkey = key % table->size;
	entry = table->table[hkey];

	/* split for performance reasons */
	if(table->comparer)
	{
		while (entry!=NULL)
		{
			if((table->comparer(entry->key, key)))
			{
				*data=entry->data;
				return(1);
			}
			entry = entry->next;
		}
	}
	else
	{
		while (entry!=NULL)
		{
			if(entry->key==key)
			{
				*data=entry->data;
				return(1);
			}
			entry = entry->next;
		}
	}
	return(0);
}

/*****
* Name: 		HashDelete
* Return Type: 	void
* Description: 	deletes the hash entry for the given key.
* In:
*	table:		hashtable from which to delete an entry;
*	key:		id of entry to be deleted.
* Returns:
*	nothing.
*****/
void
HashDelete(HashTable *table, unsigned long key)
{
    unsigned long hkey;

    hkey = key % table->size;
    table->table[hkey] = hashRemoveEntry(table, table->table[hkey], key);
    table->elements--;
}

/*****
* Name: 		HashDestroy
* Return Type: 	void
* Description: 	completely destroys the given hashtable contents. Table
*				and contents are not destroyed.
* In:
*	table:		table to be destroyed;
* Returns:
*	nothing.
*****/
void
HashDestroy(HashTable *table)
{
	int i;

	/* first remove all entries in the hash table */
	for(i = 0; i < table->size; i++)
	{
		if(table->table[i]!=NULL)
			while((table->table[i] = hashRemoveEntry(table, table->table[i],
				table->table[i]->key)) != NULL);
	}
	/* delete table */
	free(table->table);

	/* sanity */
	table->table = NULL;
}
