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
#include <string.h>
#include "gts.h"

#define HEAP_INSERT_HSPLIT(h, e) ((e)->index = gts_eheap_insert (h, e))
#define HEAP_REMOVE_HSPLIT(h, e) (gts_eheap_remove (h, (e)->index),\
				  (e)->index = NULL)

static void hsplit_init (GtsHSplit * hsplit)
{
  hsplit->index = NULL;
  hsplit->parent = NULL;
  hsplit->nchild = 0;
}

/**
 * gts_hsplit_class:
 *
 * Returns: the #GtsHSplitClass.
 */
GtsHSplitClass * gts_hsplit_class (void)
{
  static GtsHSplitClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo hsplit_info = {
      "GtsHSplit",
      sizeof (GtsHSplit),
      sizeof (GtsHSplitClass),
      (GtsObjectClassInitFunc) NULL,
      (GtsObjectInitFunc) hsplit_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_split_class ()), 
				  &hsplit_info);
  }

  return klass;
}

/**
 * gts_hsplit_new:
 * @klass: a #GtsHSplitClass.
 * @vs: a #GtsSplit.
 *
 * Returns: a new #GtsHSplit, hierarchical extension of @vs.
 */
GtsHSplit * gts_hsplit_new (GtsHSplitClass * klass, GtsSplit * vs)
{
  GtsHSplit * hs;

  g_return_val_if_fail (vs != NULL, NULL);

  hs = GTS_HSPLIT (gts_object_new (GTS_OBJECT_CLASS (klass)));
  memcpy (hs, vs, sizeof (GtsSplit));
  GTS_OBJECT (hs)->reserved = NULL;

  return hs;
}

/**
 * gts_hsplit_collapse:
 * @hs: a #GtsHSplit.
 * @hsurface: a #GtsHSurface.
 *
 * Collapses the #GtsSplit defined by @hs, updates the expandable and
 * collapsable priority heaps of @hsurface.  
 */
void gts_hsplit_collapse (GtsHSplit * hs,
			  GtsHSurface * hsurface)
{
  GtsHSplit * parent;
  GtsSplit * vs;

  g_return_if_fail (hs != NULL);
  g_return_if_fail (hs->nchild == 2);
  g_return_if_fail (hsurface != NULL);

  gts_split_collapse (GTS_SPLIT (hs), hsurface->s->edge_class, NULL);

  hsurface->nvertex--;
  hs->nchild = 0;
  HEAP_REMOVE_HSPLIT (hsurface->collapsable, hs);
  HEAP_INSERT_HSPLIT (hsurface->expandable, hs);

  vs = GTS_SPLIT (hs);
  if (GTS_IS_HSPLIT (vs->v1))
    HEAP_REMOVE_HSPLIT (hsurface->expandable, GTS_HSPLIT (vs->v1));
  if (GTS_IS_HSPLIT (vs->v2))
    HEAP_REMOVE_HSPLIT (hsurface->expandable, GTS_HSPLIT (vs->v2));

  parent = hs->parent;
  if (parent && ++parent->nchild == 2)
    HEAP_INSERT_HSPLIT (hsurface->collapsable, parent);
}

/**
 * gts_hsplit_expand:
 * @hs: a #GtsHSplit.
 * @hsurface: a #GtsHSurface.
 *
 * Expands the #GtsSplit defined by @hs (which must be expandable)
 * and updates the priority heaps of @hsurface.
 */
void gts_hsplit_expand (GtsHSplit * hs,
			GtsHSurface * hsurface)
{
  GtsHSplit * parent;
  GtsSplit * vs;

  g_return_if_fail (hs != NULL);
  g_return_if_fail (hsurface != NULL);
  g_return_if_fail (hs->nchild == 0);

  gts_split_expand (GTS_SPLIT (hs), hsurface->s, hsurface->s->edge_class);
  hsurface->nvertex++;
  hs->nchild = 2;
  HEAP_REMOVE_HSPLIT (hsurface->expandable, hs);
  HEAP_INSERT_HSPLIT (hsurface->collapsable, hs);

  vs = GTS_SPLIT (hs);
  if (GTS_IS_HSPLIT (vs->v1))
    HEAP_INSERT_HSPLIT (hsurface->expandable, GTS_HSPLIT (vs->v1));
  if (GTS_IS_HSPLIT (vs->v2))
    HEAP_INSERT_HSPLIT (hsurface->expandable, GTS_HSPLIT (vs->v2));

  parent = hs->parent;
  if (parent && parent->nchild-- == 2)
    HEAP_REMOVE_HSPLIT (hsurface->collapsable, parent);
}

static void hsurface_destroy (GtsObject * object)
{
  GtsHSurface * hs = GTS_HSURFACE (object);

  gts_hsurface_traverse (hs, G_POST_ORDER, -1,
			 (GtsSplitTraverseFunc) gts_object_destroy, 
			 NULL);
  g_slist_free (hs->roots);
  if (hs->expandable)
    gts_eheap_destroy (hs->expandable);
  if (hs->collapsable)
    gts_eheap_destroy (hs->collapsable);
  g_ptr_array_free (hs->split, TRUE);

  (* GTS_OBJECT_CLASS (gts_hsurface_class ())->parent_class->destroy) (object);
}

static void hsurface_class_init (GtsObjectClass * klass)
{
  klass->destroy = hsurface_destroy;
}

static void hsurface_init (GtsHSurface * hsurface)
{
  hsurface->s = NULL;
  hsurface->roots = NULL;
  hsurface->expandable = hsurface->collapsable = NULL;
  hsurface->split = g_ptr_array_new ();
  hsurface->nvertex = 0;
}

/**
 * gts_hsurface_class:
 *
 * Returns: the #GtsHSurfaceClass.
 */
GtsHSurfaceClass * gts_hsurface_class (void)
{
  static GtsHSurfaceClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo hsurface_info = {
      "GtsHSurface",
      sizeof (GtsHSurface),
      sizeof (GtsHSurfaceClass),
      (GtsObjectClassInitFunc) hsurface_class_init,
      (GtsObjectInitFunc) hsurface_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (), 
				  &hsurface_info);
  }

  return klass;
}

/**
 * gts_hsurface_new:
 * @klass: a #GtsHSurfaceClass.
 * @hsplit_class: a #GtsHSplitClass.
 * @psurface: a #GtsPSurface.
 * @expand_key: a #GtsKeyFunc used to order the priority heap of expandable 
 * #GtsHSplit.
 * @expand_data: data to be passed to @expand_key.
 * @collapse_key: a #GtsKeyFunc used to order the priority heap of collapsable
 * #GtsHSplit.
 * @collapse_data: data to be passed to @collapsed_key.
 *
 * Returns: a new #GtsHSurface, hierarchical extension of @psurface
 * and using #GtsHSplit of class @hsplit_class. Note that @psurface is
 * destroyed in the process.
 */
GtsHSurface * gts_hsurface_new (GtsHSurfaceClass * klass,
				GtsHSplitClass * hsplit_class,
				GtsPSurface * psurface,
				GtsKeyFunc expand_key,
				gpointer expand_data,
				GtsKeyFunc collapse_key,
				gpointer collapse_data)
{
  GtsHSurface * hsurface;

  g_return_val_if_fail (klass != NULL, NULL);
  g_return_val_if_fail (hsplit_class != NULL, NULL);
  g_return_val_if_fail (psurface != NULL, NULL);
  g_return_val_if_fail (expand_key != NULL, NULL);
  g_return_val_if_fail (collapse_key != NULL, NULL);

  hsurface = GTS_HSURFACE (gts_object_new (GTS_OBJECT_CLASS (klass)));
  hsurface->s = psurface->s;
  hsurface->expandable = gts_eheap_new (expand_key, expand_data);
  hsurface->collapsable = gts_eheap_new (collapse_key, collapse_data);
  g_ptr_array_set_size (hsurface->split, psurface->split->len);

  while (gts_psurface_remove_vertex (psurface))
    ;
  while (psurface->pos) {
    GtsSplit * vs = g_ptr_array_index (psurface->split, psurface->pos - 1);
    GtsHSplit * hs = gts_hsplit_new (hsplit_class, vs);

    g_ptr_array_index (hsurface->split, psurface->pos - 1) = hs;
    psurface->pos--;

    hs->parent = GTS_OBJECT (vs)->reserved;
    if (hs->parent) {
      GtsSplit * vsp = GTS_SPLIT (hs->parent);

      if (vsp->v1 == GTS_OBJECT (vs)) {
	g_assert (vsp->v2 != GTS_OBJECT (vs));
	vsp->v1 = GTS_OBJECT (hs);
      }
      else {
	g_assert (vsp->v2 == GTS_OBJECT (vs));
	vsp->v2 = GTS_OBJECT (hs);
      }
    }
    else
      hsurface->roots = g_slist_prepend (hsurface->roots, hs);

    hs->nchild = 0;
    if (GTS_IS_SPLIT (vs->v1))
      GTS_OBJECT (vs->v1)->reserved = hs;
    else
      hs->nchild++;
    if (GTS_IS_SPLIT (vs->v2))
      GTS_OBJECT (vs->v2)->reserved = hs;
    else
      hs->nchild++;
    
    gts_split_expand (vs, psurface->s, psurface->s->edge_class);

    if (hs->nchild == 2)
      HEAP_INSERT_HSPLIT (hsurface->collapsable, hs);
  }

  hsurface->nvertex = gts_surface_vertex_number (hsurface->s);
  gts_object_destroy (GTS_OBJECT (psurface));

  return hsurface;
}

/**
 * gts_hsurface_traverse:
 * @hsurface: a #GtsHSurface.
 * @order: the order in which nodes are visited - G_PRE_ORDER or G_POST_ORDER.
 * @depth: the maximum depth of the traversal. Nodes below this depth
 * will not be visited. If max_depth is -1 all nodes in the tree are
 * visited. If depth is 1, only the root is visited. If depth is 2,
 * the root and its children are visited. And so on.
 * @func: the function to call for each visited #GtsHSplit.
 * @data: user data to pass to the function.
 *
 * Traverses a hierarchical surface starting from its roots. It calls
 * the given function for each #GtsHSplit visited. 
 * See also gts_split_traverse().
 */
void gts_hsurface_traverse (GtsHSurface *    hsurface,
			    GTraverseType    order,
			    gint             depth,
			    GtsSplitTraverseFunc func,
			    gpointer         data)
{
  GSList * i;

  g_return_if_fail (hsurface != NULL);
  g_return_if_fail (func != NULL);
  g_return_if_fail (order < G_LEVEL_ORDER);
  g_return_if_fail (depth == -1 || depth > 0);

  i = hsurface->roots;
  while (i) {
    gts_split_traverse (i->data, order, depth, func, data);
    i = i->next;
  }
}

/**
 * gts_hsurface_foreach:
 * @hsurface: a #GtsHSurface.
 * @order: the order in which #GtsHSplit are visited - G_PRE_ORDER or 
 * G_POST_ORDER.
 * @func: the function to call for each visited #GtsHSplit.
 * @data: user data to pass to the function.
 *
 * Starts by expanding all the #GtsHSplit of @hsurface. If @order is
 * G_PRE_ORDER, calls @func for each #GtsHSplit and collapses it. If
 * order is G_POST_ORDER, collapses each #GtsHSplit first and then
 * calls @func. The traversal can be halted at any point by returning
 * TRUE from func.  
 */
void gts_hsurface_foreach (GtsHSurface * hsurface,
			   GTraverseType order,
			   GtsFunc       func,
			   gpointer      data)
{
  GtsHSplit * hs;
  guint i = 0, len;
  gboolean stop = FALSE;

  g_return_if_fail (hsurface != NULL);
  g_return_if_fail (func != NULL);
  g_return_if_fail (order == G_PRE_ORDER || order == G_POST_ORDER);

  while ((hs = gts_eheap_top (hsurface->expandable, NULL))) 
    gts_hsplit_expand (hs, hsurface);

  len = hsurface->split->len;
  switch (order) {
  case G_PRE_ORDER:
    while (i < len && !stop) {
      GtsHSplit * hs = g_ptr_array_index (hsurface->split, i);
      stop = (*func) (hs, data);
      if (!stop)
	gts_hsplit_collapse (hs, hsurface);
      i++;
    }
    break;
  case G_POST_ORDER:
    while (i < len && !stop) {
      GtsHSplit * hs = g_ptr_array_index (hsurface->split, i);
      gts_hsplit_collapse (hs, hsurface);
      stop = (*func) (hs, data);
      i++;
    }
    break;
  default:
    g_assert_not_reached ();
  }
}

/**
 * gts_hsurface_height:
 * @hsurface: a #GtsHSurface.
 *
 * Returns: the maximum height of the tree described by @hsurface.
 */
guint gts_hsurface_height (GtsHSurface * hsurface)
{
  GSList * i;
  guint height = 0;

  g_return_val_if_fail (hsurface != NULL, 0);

  i = hsurface->roots;
  while (i) {
    guint tmp_height = gts_split_height (i->data);
    if (tmp_height > height)
      height = tmp_height;
    i = i->next;
  }

  return height;
}
