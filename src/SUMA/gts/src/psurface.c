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
#include <math.h>
#include "gts.h"

#define HEAP_INSERT_OBJECT(h, e) (GTS_OBJECT (e)->reserved =\
                                   gts_eheap_insert (h, e))
#define HEAP_REMOVE_OBJECT(h, e) (gts_eheap_remove (h, GTS_OBJECT (e)->reserved),\
				  GTS_OBJECT (e)->reserved = NULL)

static void psurface_destroy (GtsObject * object)
{
  GtsPSurface * ps = GTS_PSURFACE (object);
  guint i;

  if (!GTS_PSURFACE_IS_CLOSED (ps))
    gts_psurface_close (ps);

  for (i = 0; i < ps->split->len; i++)
    if (g_ptr_array_index (ps->split, i))
      gts_object_destroy (GTS_OBJECT (g_ptr_array_index (ps->split, i)));
  g_ptr_array_free (ps->split, TRUE);

  (* GTS_OBJECT_CLASS (gts_psurface_class ())->parent_class->destroy) (object);
}

static void psurface_class_init (GtsObjectClass * klass)
{
  klass->destroy = psurface_destroy;
}

static void psurface_init (GtsPSurface * psurface)
{
  psurface->s = NULL;
  psurface->split = g_ptr_array_new ();
  psurface->split_class = gts_split_class ();
  psurface->pos = psurface->min = 0;
  psurface->vertices = psurface->faces = NULL;
}

/**
 * gts_psurface_class:
 * 
 * Returns: the #GtsPSurfaceClass.
 */
GtsPSurfaceClass * gts_psurface_class (void)
{
  static GtsPSurfaceClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo psurface_info = {
      "GtsPSurface",
      sizeof (GtsPSurface),
      sizeof (GtsPSurfaceClass),
      (GtsObjectClassInitFunc) psurface_class_init,
      (GtsObjectInitFunc) psurface_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (), 
				  &psurface_info);
  }

  return klass;
}

static GtsVertex * edge_collapse (GtsPSurface * ps,
				  GtsEdge * e,
				  GtsEHeap * heap,
				  GtsCoarsenFunc coarsen_func,
				  gpointer coarsen_data,
				  gdouble maxcosine2)
{
  GtsVertex  * v1 = GTS_SEGMENT (e)->v1, * v2 = GTS_SEGMENT (e)->v2, * mid;
  GtsSplit * vs;
  GtsObject * o1, * o2;

  /* if the edge is degenerate (i.e. v1 == v2), destroy and return */
  if (v1 == v2) {
    gts_object_destroy (GTS_OBJECT (e));
    return NULL;
  }

  if (!gts_edge_collapse_is_valid (e) ||
      /* check that a non-manifold edge is not a contact edge */
      (g_slist_length (e->triangles) > 2 && gts_edge_is_contact (e) > 1)) {
    GTS_OBJECT (e)->reserved = 
      gts_eheap_insert_with_key (heap, e, G_MAXDOUBLE);
    return NULL;
  }

  mid = (*coarsen_func) (e, ps->s->vertex_class, coarsen_data);

  if (gts_edge_collapse_creates_fold (e, mid, maxcosine2)) {
    GTS_OBJECT (e)->reserved = 
      gts_eheap_insert_with_key (heap, e, G_MAXDOUBLE);
    gts_object_destroy (GTS_OBJECT (mid));
    return NULL;
  }

  if (GTS_OBJECT (v1)->reserved)
    o1 = GTS_OBJECT (v1)->reserved;
  else
    o1 = GTS_OBJECT (v1);
  if (GTS_OBJECT (v2)->reserved)
    o2 = GTS_OBJECT (v2)->reserved;
  else
    o2 = GTS_OBJECT (v2);
  vs = gts_split_new (ps->split_class, mid, o1, o2);
  gts_split_collapse (vs, ps->s->edge_class, heap);
  GTS_OBJECT (vs->v)->reserved = vs;
  g_ptr_array_add (ps->split, vs);

  return mid;
}

static void update_2nd_closest_neighbors (GtsVertex * v, GtsEHeap * heap)
{
  GSList * i = v->segments;
  GSList * list = NULL;
  
  while (i) {
    GtsSegment * s = i->data;
    if (GTS_IS_EDGE (s)) {
      GtsVertex * v1 = s->v1 == v ? s->v2 : s->v1;
      GSList * j = v1->segments;
      while (j) {
	GtsSegment * s1 = j->data;
	if (GTS_IS_EDGE (s1) && !g_slist_find (list, s1))
	  list = g_slist_prepend (list, s1);
	j = j->next;
      }
    }
    i = i->next;
  }

  i = list;
  while (i) {
    GtsEdge * e = i->data;
    if (GTS_OBJECT (e)->reserved)
      HEAP_REMOVE_OBJECT (heap, e);
    HEAP_INSERT_OBJECT (heap, e);
    i = i->next;
  }

  g_slist_free (list);
}

static gdouble edge_length2 (GtsEdge * e)
{
  return gts_point_distance2 (GTS_POINT (GTS_SEGMENT (e)->v1), 
			      GTS_POINT (GTS_SEGMENT (e)->v2));
}

static void create_heap_coarsen (GtsEdge * e, GtsEHeap * heap)
{
  HEAP_INSERT_OBJECT (heap, e);
}

/* #define DEBUG_FOLD */
/* #define DEBUG_CONTACT_VERTEX */

#ifdef DEBUG_FOLD
static void check_fold (GtsTriangle * t, gdouble * maxcosine2)
{
  GtsEdge * e1 = t->e1, * e2 = t->e2, * e3 = t->e3;

  
  if (gts_triangles_are_folded (e1->triangles, 
				GTS_SEGMENT (e1)->v1,
				GTS_SEGMENT (e1)->v2,
				*maxcosine2) ||
      gts_triangles_are_folded (e2->triangles, 
				GTS_SEGMENT (e2)->v1,
				GTS_SEGMENT (e2)->v2,
				*maxcosine2) ||
      gts_triangles_are_folded (e3->triangles, 
				GTS_SEGMENT (e3)->v1,
				GTS_SEGMENT (e3)->v2,
				*maxcosine2)) {
    fprintf (stderr, "triangle %p:(%p,%p,%p) is folded\n", t, e1, e2, e3);
    g_assert_not_reached ();
  }
}
#endif

/**
 * gts_psurface_new:
 * @klass: a #GtsPSurfaceClass.
 * @surface: a #GtsSurface.
 * @split_class: a #GtsSplitClass to use for the new progressive surface.
 * @cost_func: cost function for the edge collapse algorithm.
 * @cost_data: data to pass to @cost_func.
 * @coarsen_func: the function returning the vertex replacement for the edge 
 * collapse.
 * @coarsen_data: data to pass to @coarsen_func.
 * @stop_func: the function to call to decide whether to stop the coarsening
 * process.
 * @stop_data: data to pass to @stop_func.
 * @minangle: the minimum angle allowable between two neighboring triangles. 
 * This is used to avoid introducing folds in the mesh during simplification.
 *
 * This function works in exactly the same way as the
 * gts_surface_coarsen() function, except that the history of edge
 * collapse is saved in an array of #GtsSplit objects. This allows for
 * dynamic continuous multiresolution control of the input @surface.
 *
 * Returns: a new progressive surface.
 */
GtsPSurface * gts_psurface_new (GtsPSurfaceClass * klass,
				GtsSurface * surface,
				GtsSplitClass * split_class,
				GtsKeyFunc cost_func,
				gpointer cost_data,
				GtsCoarsenFunc coarsen_func,
				gpointer coarsen_data,
				GtsStopFunc stop_func,
				gpointer stop_data,
				gdouble minangle)
{
  GtsPSurface * psurface;
  GtsEHeap * heap;
  GtsEdge * e;
  gdouble top_cost, maxcosine2;
  guint i;

  g_return_val_if_fail (klass != NULL, NULL);
  g_return_val_if_fail (surface != NULL, NULL);
  g_return_val_if_fail (split_class != NULL, NULL);
  g_return_val_if_fail (stop_func != NULL, NULL);

  psurface = GTS_PSURFACE (gts_object_new (GTS_OBJECT_CLASS (klass)));
  psurface->s = surface;
  psurface->split_class = split_class;

  if (cost_func == NULL)
    cost_func = (GtsKeyFunc) edge_length2;
  if (coarsen_func == NULL)
    coarsen_func = (GtsCoarsenFunc) gts_segment_midvertex;

  heap = gts_eheap_new (cost_func, cost_data);
  maxcosine2 = cos (minangle); maxcosine2 *= maxcosine2;

  gts_eheap_freeze (heap);
  gts_surface_foreach_edge (surface, (GtsFunc) create_heap_coarsen, heap);
  gts_eheap_thaw (heap);
  /* we want to control edge destruction manually */
  gts_allow_floating_edges = TRUE;
  while ((e = gts_eheap_remove_top (heap, &top_cost)) &&
	 (top_cost < G_MAXDOUBLE) &&
	 !(*stop_func) (top_cost, gts_eheap_size (heap) - 
			gts_edge_face_number (e, surface), stop_data)) {
    GtsVertex * v = edge_collapse (psurface, e, heap, 
				   coarsen_func, coarsen_data, maxcosine2);
    if (v != NULL) {
      update_2nd_closest_neighbors (v, heap);
#ifdef DEBUG_FOLD
      {
	GSList * triangles = gts_vertex_triangles (v, NULL), * i;
	fprintf (stderr, "\n---- Check for folds ----\n%p: ", v);
	i = triangles;
	while (i) {
	  GtsTriangle * t = i->data;
	  fprintf (stderr, "%p:(%p,%p,%p) ", t, t->e1, t->e2, t->e3);
	  i = i->next;
	}
	fprintf (stderr, "\n");
	g_slist_free (triangles);
	gts_surface_foreach_face (surface, (GtsFunc) check_fold, &maxcosine2);
      }
#endif
#ifdef DEBUG_CONTACT_VERTEX
      if (gts_vertex_is_contact (v, FALSE) != 1) {
	FILE * fptr = fopen ("after", "wt");
	GSList * triangles = gts_vertex_triangles (v, NULL), * i;

	fprintf (stderr, "collapse of %p created a contact vertex\n", e);
		 
	fprintf (fptr, 
		 "(geometry \"sphere\" { = SPHERE 0.1 0. 0. 0. })\n"
		 "(normalization \"sphere\" none)\n");
	i = triangles;
	while (i) {
	  gts_write_triangle (i->data, GTS_POINT (v), fptr);
	  i = i->next;
	}
	g_assert_not_reached ();
      }
#endif
    }
  }
  gts_allow_floating_edges = FALSE;

  /* set reserved field of remaining edges back to NULL */
  if (e) GTS_OBJECT (e)->reserved = NULL;
  gts_eheap_foreach (heap, (GFunc) gts_object_reset_reserved, NULL);

  gts_eheap_destroy (heap);

  psurface->pos = psurface->split->len;
  psurface->min = gts_surface_vertex_number (psurface->s);

  /* set reserved field of vertices (used to build the hierarchy) 
     back to NULL */
  for (i = 0; i < psurface->split->len; i++) {
    GtsSplit * vs = g_ptr_array_index (psurface->split, i);
    gts_object_reset_reserved (GTS_OBJECT (vs->v));
  }

  return psurface;
}

/**
 * gts_psurface_add_vertex:
 * @ps: a #GtsPSurface.
 *
 * Adds a vertex to the progressive surface @ps by expanding the next
 * available #GtsSplit.
 *
 * Returns: the expanded #GtsSplit or %NULL if all the #GtsSplit have already
 * been expanded.
 */
GtsSplit * gts_psurface_add_vertex (GtsPSurface * ps) 
{ 
  GtsSplit * vs;

  g_return_val_if_fail (ps != NULL, NULL);
  g_return_val_if_fail (GTS_PSURFACE_IS_CLOSED (ps), NULL);

  if (ps->pos == 0)
    return NULL;

  vs = g_ptr_array_index (ps->split, --ps->pos);
  gts_split_expand (vs, ps->s, ps->s->edge_class);

  return vs;
}

/**
 * gts_psurface_remove_vertex:
 * @ps: a #GtsPSurface.
 *
 * Removes one vertex from the progressive surface @ps by collapsing the first
 * available #GtsSplit.
 *
 * Returns: the collapsed #GtsSplit or %NULL if all the #GtsSplit have already
 * been collapsed.
 */
GtsSplit * gts_psurface_remove_vertex (GtsPSurface * ps)
{
  GtsSplit * vs;

  g_return_val_if_fail (ps != NULL, NULL);
  g_return_val_if_fail (GTS_PSURFACE_IS_CLOSED (ps), NULL);

  if (ps->pos == ps->split->len)
    return NULL;

  vs = g_ptr_array_index (ps->split, ps->pos++);
  gts_split_collapse (vs, ps->s->edge_class, NULL);

  return vs;
}

/**
 * gts_psurface_max_vertex_number:
 * @ps: a #GtsPSurface.
 *
 * Returns: the maximum number of vertices of @ps i.e. the number of vertices
 * if all the #GtsSplit were expanded.
 */
guint gts_psurface_max_vertex_number (GtsPSurface * ps)
{
  g_return_val_if_fail (ps != NULL, 0);

  return ps->min + ps->split->len;
}

/**
 * gts_psurface_min_vertex_number:
 * @ps: a #GtsPSurface.
 *
 * Returns: the minimum number of vertices of @ps i.e. the number of vertices
 * if all the #GtsSplit were collapsed.
 */
guint gts_psurface_min_vertex_number (GtsPSurface * ps)
{
  g_return_val_if_fail (ps != NULL, 0);

  return ps->min;
}

/**
 * gts_psurface_set_vertex_number:
 * @ps: a #GtsPSurface.
 * @n: a number of vertices.
 *
 * Performs the required number of collapses or expansions to set the number
 * of vertices of @ps to @n.
 */
void gts_psurface_set_vertex_number (GtsPSurface * ps, guint n)
{
  g_return_if_fail (ps != NULL);
  g_return_if_fail (GTS_PSURFACE_IS_CLOSED (ps));

  n = ps->min + ps->split->len - n;
  while (ps->pos > n && gts_psurface_add_vertex (ps))
    ;
  while (ps->pos < n && gts_psurface_remove_vertex (ps))
    ;
}

/**
 * gts_psurface_get_vertex_number:
 * @ps: a #GtsPSurface.
 *
 * Returns: the current number of vertices of @ps.
 */
guint gts_psurface_get_vertex_number (GtsPSurface * ps)
{
  g_return_val_if_fail (ps != NULL, 0);
  
  if (!GTS_PSURFACE_IS_CLOSED (ps))
    return ps->min + ps->pos;
  else
    return ps->min + ps->split->len - ps->pos;
}

/**
 * gts_psurface_foreach_vertex:
 * @ps: a #GtsPSurface.
 * @func: a function to call for each vertex of @ps.
 * @data: data to be passed to @func.
 *
 * Calls @func for each (potential) vertex of @ps, whether actually used
 * or not. The vertices are called in the order they were created during the
 * edge collapse operation.
 */
void gts_psurface_foreach_vertex (GtsPSurface * ps, 
				  GtsFunc func, 
				  gpointer data)
{
  guint i;

  g_return_if_fail (ps != NULL);
  g_return_if_fail (func != NULL);
  g_return_if_fail (GTS_PSURFACE_IS_CLOSED (ps));
  
  for (i = 0; i < ps->split->len; i++) {
    GtsSplit * vs = g_ptr_array_index (ps->split, i);
    (*func) (vs->v, data);
  }
}
