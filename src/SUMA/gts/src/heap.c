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

struct _GtsHeap {
  GPtrArray * elts;
  GCompareFunc func;
  gboolean frozen;
};

/**
 * gts_heap_new:
 * @compare_func: a GCompareFunc.
 *
 * Returns: a new #GtsHeap using @compare_func as a sorting function.
 */
GtsHeap * gts_heap_new (GCompareFunc compare_func)
{
  GtsHeap * heap;

  g_return_val_if_fail (compare_func != NULL, NULL);
  
  heap = g_malloc (sizeof(GtsHeap));
  heap->elts = g_ptr_array_new ();
  heap->func = compare_func;
  heap->frozen = FALSE;
  return heap;
}

static void sift_up (GtsHeap * heap, guint i)
{
  gpointer parent, child;
  guint p;
  gpointer * pdata = heap->elts->pdata;
  GCompareFunc func = heap->func;

  child = pdata[i - 1];
  while ((p = PARENT (i))) {
    parent = pdata[p - 1];
    if ((*func) (parent, child) > 0) {
      pdata[p - 1] = child;
      pdata[i - 1] = parent;
      i = p;
    }
    else
      i = 0;
  }
}

/**
 * gts_heap_insert:
 * @heap: a #GtsHeap.
 * @p: a pointer to add to the heap.
 *
 * Inserts a new element @p in the heap.
 */
void gts_heap_insert (GtsHeap * heap, gpointer p)
{
  g_return_if_fail (heap != NULL);

  g_ptr_array_add (heap->elts, p);
  if (!heap->frozen)
    sift_up (heap, heap->elts->len);
}

static void sift_down (GtsHeap * heap, guint i)
{
  gpointer left_child, right_child, child, parent;
  guint lc, rc, c;
  gpointer * pdata = heap->elts->pdata;
  guint len = heap->elts->len;
  GCompareFunc func = heap->func;

  lc = LEFT_CHILD (i);
  rc = RIGHT_CHILD (i);
  left_child = lc <= len ? pdata[lc - 1] : NULL;
  right_child = rc <= len ? pdata[rc - 1] : NULL;

  parent = pdata[i - 1];
  while (left_child != NULL) {
    if (right_child == NULL ||
	(*func) (left_child, right_child) < 0) {
      child = left_child;
      c = lc;
    }
    else {
      child = right_child;
      c = rc;
    }
    if ((*func) (parent, child) > 0) {
      pdata[i - 1] = child;
      pdata[c - 1] = parent;
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
 * gts_heap_remove_top:
 * @heap: a #GtsHeap.
 *
 * Removes the element at the top of the heap.
 *
 * Returns: the element at the top of the heap.
 */
gpointer gts_heap_remove_top (GtsHeap * heap)
{
  gpointer root;
  GPtrArray * elts;
  guint len;

  g_return_val_if_fail (heap != NULL, NULL);

  elts = heap->elts; len = elts->len;

  if (len == 0)
    return NULL;
  if (len == 1)
    return g_ptr_array_remove_index (elts, 0);

  root = elts->pdata[0];
  elts->pdata[0] = g_ptr_array_remove_index (elts, len - 1);
  sift_down (heap, 1);
  return root;
}

/**
 * gts_heap_top:
 * @heap: a #GtsHeap.
 *
 * Returns: the element at the top of the heap.
 */
gpointer gts_heap_top (GtsHeap * heap)
{
  GPtrArray * elts;
  guint len;

  g_return_val_if_fail (heap != NULL, NULL);

  elts = heap->elts; 
  len = elts->len;
  if (len == 0)
    return NULL;
  return elts->pdata[0];
}

/**
 * gts_heap_destroy:
 * @heap: a #GtsHeap.
 * 
 * Free all the memory allocated for @heap.
 */
void gts_heap_destroy (GtsHeap * heap)
{
  g_return_if_fail (heap != NULL);

  g_ptr_array_free (heap->elts, TRUE);
  g_free (heap);
}

/**
 * gts_heap_thaw:
 * @heap: a #GtsHeap.
 *
 * If @heap has been frozen previously using gts_heap_freeze(), reorder it
 * in O(n) time and unfreeze it.
 */
void gts_heap_thaw (GtsHeap * heap)
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
 * gts_heap_foreach:
 * @heap: a #GtsHeap.
 * @func: the function to call for each element in the heap.
 * @user_data: to pass to @func.
 */
void gts_heap_foreach (GtsHeap * heap, 
		       GFunc func,
		       gpointer user_data)
{
  guint i;
  GPtrArray * elts;
  
  g_return_if_fail (heap != NULL);
  g_return_if_fail (func != NULL);

  elts = heap->elts;
  for (i = 0; i < elts->len; i++)
    (*func) (elts->pdata[i], user_data);
}

/**
 * gts_heap_freeze:
 * @heap: a #GtsHeap.
 *
 * Freezes the heap. Any subsequent operation will not preserve the heap
 * property. Used in conjunction with gts_heap_insert() and gts_heap_thaw()
 * to create a heap in O(n) time.
 */
void gts_heap_freeze (GtsHeap * heap)
{
  g_return_if_fail (heap != NULL);

  heap->frozen = TRUE;
}

/**
 * gts_heap_size:
 * @heap: a #GtsHeap.
 *
 * Returns: the number of items in @heap.
 */
guint gts_heap_size (GtsHeap * heap)
{
  g_return_val_if_fail (heap != NULL, 0);

  return heap->elts->len;
}
