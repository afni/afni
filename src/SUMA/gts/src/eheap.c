/* GTS - Library for the manipulation of triangulated surfaces
 * Copyright (C) 1999 Stéphane Popinet
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdlib.h>
#include "gts.h"

#define PARENT(i) ((i) >= 2 ? (i)/2 : 0)
#define LEFT_CHILD(i) (2*(i))
#define RIGHT_CHILD(i) (2*(i) + 1)

struct _GtsEHeap {
  GPtrArray * elts;
  GtsKeyFunc func;
  gpointer data;
  gboolean frozen, randomized;
  GMemChunk * mem_chunk;
};

/**
 * gts_eheap_new:
 * @key_func: a #GtsKeyFunc or %NULL.
 * @data: user data to be passed to @key_func.
 *
 * Returns: a new #GtsEHeap using @key_func as key.
 */
GtsEHeap * gts_eheap_new (GtsKeyFunc key_func,
			  gpointer data)
{
  GtsEHeap * heap;

  heap = g_malloc (sizeof(GtsEHeap));
  heap->elts = g_ptr_array_new ();
  heap->func = key_func;
  heap->data = data;
  heap->frozen = FALSE;
  heap->randomized = FALSE;
  heap->mem_chunk = g_mem_chunk_create (GtsEHeapPair, 512, G_ALLOC_AND_FREE);
  return heap;
}

static void sift_up (GtsEHeap * heap, guint i)
{
  GtsEHeapPair * parent, * child;
  guint p;
  gpointer * pdata = heap->elts->pdata;
  gdouble key;

  child = pdata[i - 1];
  key = child->key;
  while ((p = PARENT (i))) {
    parent = pdata[p - 1];
    if (parent->key > key ||
	(heap->randomized && parent->key == key && rand () < RAND_MAX/2)) {
      pdata[p - 1] = child;
      pdata[i - 1] = parent;
      child->pos = p;
      parent->pos = i;
      i = p;
    }
    else
      i = 0;
  }
}

/**
 * gts_eheap_insert:
 * @heap: a #GtsEHeap.
 * @p: a pointer to add to the heap.
 *
 * Inserts a new element @p in the heap.
 *
 * Returns: a #GtsEHeapPair describing the position of the element in the heap.
 * This pointer is necessary for gts_eheap_remove() and 
 * gts_eheap_decrease_key().
 */
GtsEHeapPair * gts_eheap_insert (GtsEHeap * heap, gpointer p)
{
  GtsEHeapPair * pair;
  GPtrArray * elts;

  g_return_val_if_fail (heap != NULL, NULL);
  g_return_val_if_fail (heap->func != NULL, NULL);

  elts = heap->elts;
  pair = g_chunk_new (GtsEHeapPair, heap->mem_chunk);
  g_ptr_array_add (elts, pair);
  pair->data = p;
  pair->pos = elts->len;
  pair->key = (*heap->func) (p, heap->data);
  if (!heap->frozen)
    sift_up (heap, elts->len);
  return pair;
}

/**
 * gts_eheap_insert_with_key:
 * @heap: a #GtsEHeap.
 * @p: a pointer to add to the heap.
 * @key: the value of the key associated to @p.
 *
 * Inserts a new element @p in the heap.
 *
 * Returns: a #GtsEHeapPair describing the position of the element in the heap.
 * This pointer is necessary for gts_eheap_remove() and 
 * gts_eheap_decrease_key().
 */
GtsEHeapPair * gts_eheap_insert_with_key (GtsEHeap * heap, 
					  gpointer p, 
					  gdouble key)
{
  GtsEHeapPair * pair;
  GPtrArray * elts;

  g_return_val_if_fail (heap != NULL, NULL);

  elts = heap->elts;
  pair = g_chunk_new (GtsEHeapPair, heap->mem_chunk);
  g_ptr_array_add (elts, pair);
  pair->data = p;
  pair->pos = elts->len;
  pair->key = key;
  if (!heap->frozen)
    sift_up (heap, elts->len);
  return pair;
}

static void sift_down (GtsEHeap * heap, guint i)
{
  GtsEHeapPair * left_child, * right_child, * child, * parent;
  guint lc, rc, c;
  gpointer * pdata = heap->elts->pdata;
  guint len = heap->elts->len;
  gdouble key;

  lc = LEFT_CHILD (i);
  rc = RIGHT_CHILD (i);
  left_child = lc <= len ? pdata[lc - 1] : NULL;
  right_child = rc <= len ? pdata[rc - 1] : NULL;

  parent = pdata[i - 1];
  key = parent->key;
  while (left_child != NULL) {
    if (right_child == NULL || left_child->key  < right_child->key) {
      child = left_child;
      c = lc;
    }
    else {
      child = right_child;
      c = rc;
    }
    if (key > child->key) {
      pdata[i - 1] = child;
      child->pos = i;
      pdata[c - 1] = parent;
      parent->pos = c;
      i = c;
      lc = LEFT_CHILD (i);
      rc = RIGHT_CHILD (i);
      left_child = lc <= len ? pdata[lc - 1] : NULL;
      right_child = rc <= len ? pdata[rc - 1] : NULL;      
    }
    else
      left_child = NULL;
  }
}

/**
 * gts_eheap_remove_top:
 * @heap: a #GtsEHeap.
 * @key: a pointer on a gdouble or %NULL.
 *
 * Removes the element at the top of the heap and optionally (if @key is not
 * %NULL) returns the value of its key.
 *
 * Returns: the element at the top of the heap.
 */
gpointer gts_eheap_remove_top (GtsEHeap * heap, gdouble * key)
{
  gpointer root;
  GPtrArray * elts;
  guint len;
  GtsEHeapPair * pair;

  g_return_val_if_fail (heap != NULL, NULL);

  elts = heap->elts; 
  len = elts->len;

  if (len == 0)
    return NULL;
  if (len == 1) {
    pair = g_ptr_array_remove_index (elts, 0);
    root = pair->data;
    if (key) 
      *key = pair->key;
    g_mem_chunk_free (heap->mem_chunk, pair);
    return root;
  }

  pair = elts->pdata[0];
  root = pair->data;
  if (key) 
    *key = pair->key;
  g_mem_chunk_free (heap->mem_chunk, pair);
  pair = g_ptr_array_remove_index (elts, len - 1);
  elts->pdata[0] = pair;
  pair->pos = 1;
  sift_down (heap, 1);
  return root;
}

/**
 * gts_eheap_top:
 * @heap: a #GtsEHeap.
 * @key: a pointer on a gdouble or %NULL.
 *
 * Returns: the element at the top of the heap and optionally (if @key is not
 * %NULL) its key.
 */
gpointer gts_eheap_top (GtsEHeap * heap, gdouble * key)
{
  GtsEHeapPair * pair;
  GPtrArray * elts;

  g_return_val_if_fail (heap != NULL, NULL);

  elts = heap->elts;

  if (elts->len == 0)
    return NULL;

  pair = elts->pdata[0];
  if (key)
    *key = pair->key;
  return pair->data;
}

/**
 * gts_eheap_destroy:
 * @heap: a #GtsEHeap.
 * 
 * Free all the memory allocated for @heap.
 */
void gts_eheap_destroy (GtsEHeap * heap)
{
  g_return_if_fail (heap != NULL);

  g_ptr_array_free (heap->elts, TRUE);
  g_mem_chunk_destroy (heap->mem_chunk);
  g_free (heap);
}

/**
 * gts_eheap_thaw:
 * @heap: a #GtsEHeap.
 *
 * If @heap has been frozen previously using gts_eheap_freeze(), reorder it
 * in O(n) time and unfreeze it.
 */
void gts_eheap_thaw (GtsEHeap * heap)
{
  guint i;
  
  g_return_if_fail (heap != NULL);

  if (!heap->frozen)
    return;

  for (i = heap->elts->len/2; i > 0; i--)
    sift_down (heap, i);

  heap->frozen = FALSE;
}

/**
 * gts_eheap_foreach:
 * @heap: a #GtsEHeap.
 * @func: the function to call for each element in the heap.
 * @data: to pass to @func.
 */
void gts_eheap_foreach (GtsEHeap * heap, 
			GFunc func,
			gpointer data)
{
  guint i;
  GPtrArray * elts;
  
  g_return_if_fail (heap != NULL);
  g_return_if_fail (func != NULL);

  elts = heap->elts;
  for (i = 0; i < elts->len; i++)
    (*func) (((GtsEHeapPair *) elts->pdata[i])->data, data);
}

/**
 * gts_eheap_remove:
 * @heap: a #GtsEHeap.
 * @p: a #GtsEHeapPair.
 *
 * Removes element corresponding to @p from @heap in O(log n).
 *
 * Returns: the element just removed from @heap.
 */
gpointer gts_eheap_remove (GtsEHeap * heap, GtsEHeapPair * p)
{
  GtsEHeapPair ** pdata;
  GtsEHeapPair * parent;
  guint i, par;
  gpointer data;

  g_return_val_if_fail (heap != NULL, NULL);
  g_return_val_if_fail (p != NULL, NULL);

  pdata = (GtsEHeapPair **)heap->elts->pdata;
  i = p->pos;
  data = p->data;

  g_return_val_if_fail (i > 0 && i <= heap->elts->len, NULL);
  g_return_val_if_fail (p == pdata[i - 1], NULL);

  /* move element to the top */
  while ((par = PARENT (i))) {
    parent = pdata[par - 1];
    pdata[par - 1] = p;
    pdata[i - 1] = parent;
    p->pos = par;
    parent->pos = i;
    i = par;
  }

  gts_eheap_remove_top (heap, NULL);

  return data;
}

/**
 * gts_eheap_decrease_key:
 * @heap: a #GtsEHeap.
 * @p: a #GtsEHeapPair.
 * @new_key: the new value of the key for this element. Must be smaller than
 * the current key.
 *
 * Decreases the value of the key of the element at position @p.
 */
void gts_eheap_decrease_key (GtsEHeap * heap, 
			     GtsEHeapPair * p,
			     gdouble new_key)
{
  guint i;

  g_return_if_fail (heap != NULL);
  g_return_if_fail (p != NULL);

  i = p->pos;
  g_return_if_fail (i > 0 && i <= heap->elts->len);
  g_return_if_fail (p == heap->elts->pdata[i - 1]);

  g_return_if_fail (new_key <= p->key);

  p->key = new_key;
  if (!heap->frozen)
    sift_up (heap, i);
}

/**
 * gts_eheap_freeze:
 * @heap: a #GtsEHeap.
 *
 * Freezes the heap. Any subsequent operation will not preserve the heap
 * property. Used in conjunction with gts_eheap_insert() and gts_eheap_thaw()
 * to create a heap in O(n) time.
 */
void gts_eheap_freeze (GtsEHeap * heap)
{
  g_return_if_fail (heap != NULL);

  heap->frozen = TRUE;
}

/**
 * gts_eheap_size:
 * @heap: a #GtsEHeap.
 *
 * Returns: the number of items in @heap.
 */
guint gts_eheap_size (GtsEHeap * heap)
{
  g_return_val_if_fail (heap != NULL, 0);

  return heap->elts->len;
}

/**
 * gts_eheap_update:
 * @heap: a #GtsEHeap.
 *
 * Updates the key of each element of @heap and reorders it.
 */
void gts_eheap_update (GtsEHeap * heap)
{
  guint i, len;
  GtsEHeapPair ** pairs;
  gpointer data;
  GtsKeyFunc func;

  g_return_if_fail (heap != NULL);
  g_return_if_fail (heap->func != NULL);

  heap->frozen = TRUE;

  len = heap->elts->len;
  pairs = (GtsEHeapPair **) heap->elts->pdata;
  data = heap->data;
  func = heap->func;

  for (i = 0; i < len; i++) {
    GtsEHeapPair * pair = pairs[i];
    pair->key = (*func) (pair->data, data);
  }
  
  gts_eheap_thaw (heap);
}

/**
 * gts_eheap_key:
 * @heap: a #GtsEHeap.
 * @p: a pointer to be tested;
 *
 * Returns: the value of the key for pointer @p.
 */
gdouble gts_eheap_key (GtsEHeap * heap, gpointer p)
{
  g_return_val_if_fail (heap != NULL, 0.);
  g_return_val_if_fail (heap->func != NULL, 0.);

  return (* heap->func) (p, heap->data);
}

/**
 * gts_eheap_randomized:
 * @heap: a #GtsEHeap.
 * @randomized: whether @heap should be randomized.
 */
void gts_eheap_randomized (GtsEHeap * heap, gboolean randomized)
{
  g_return_if_fail (heap != NULL);

  heap->randomized = randomized;
}
