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

#include <math.h>
#include "gts.h"

/**
 * gts_vertex_encroaches_edge:
 * @v: a #GtsVertex.
 * @e: a #GtsEdge.
 *
 * Returns: %TRUE if @v is strictly contained in the diametral circle of @e,
 * %FALSE otherwise.
 */
gboolean gts_vertex_encroaches_edge (GtsVertex * v, GtsEdge * e)
{
  GtsPoint * p, * p1, * p2;

  g_return_val_if_fail (v != NULL, FALSE);
  g_return_val_if_fail (e != NULL, FALSE);

  p = GTS_POINT (v);
  p1 = GTS_POINT (GTS_SEGMENT (e)->v1);
  p2 = GTS_POINT (GTS_SEGMENT (e)->v2);

  if ((p1->x - p->x)*(p2->x - p->x) + (p1->y - p->y)*(p2->y - p->y) < 0.0)
    return TRUE;
  return FALSE;
}

/**
 * gts_edge_is_encroached:
 * @e: a #GtsEdge.
 * @s: a #GtsSurface describing a (constrained) Delaunay triangulation.
 * @encroaches: a #GtsEncroachFunc.
 * @data: user data to be passed to @encroaches.
 *
 * Returns: a #GtsVertex belonging to @s and encroaching upon @e
 * (as defined by @encroaches) or %NULL if there is none.  
 */
GtsVertex * gts_edge_is_encroached (GtsEdge * e,
				    GtsSurface * s,
				    GtsEncroachFunc encroaches,
				    gpointer data)
{
  GSList * i;

  g_return_val_if_fail (e != NULL, NULL);
  g_return_val_if_fail (s != NULL, NULL);
  g_return_val_if_fail (encroaches != NULL, NULL);

  i = e->triangles;
  while (i) {
    GtsFace * f = i->data;
    if (GTS_IS_FACE (f) && gts_face_has_parent_surface (f, s)) {
      GtsVertex * v = gts_triangle_vertex_opposite (GTS_TRIANGLE (f), e);
      if ((* encroaches) (v, e, s, data))
	return v;
    }
    i = i->next;
  }

  return NULL;
}

#define ALREADY_ENCROACHED(c) (GTS_OBJECT (c)->reserved)

static void vertex_encroaches (GtsVertex * v,
			       GtsSurface * surface,
			       GtsFifo * encroached,
			       GtsEncroachFunc encroaches,
			       gpointer data)
{
  GSList * triangles, * i;

  g_return_if_fail (v != NULL);
  g_return_if_fail (surface != NULL);
  g_return_if_fail (encroached != NULL);
  g_return_if_fail (encroaches != NULL);

  i = triangles = gts_vertex_triangles (v, NULL);
  while (i) {
    GtsFace * f = i->data;
    if (GTS_IS_FACE (f) && gts_face_has_parent_surface (f, surface)) {
      GtsEdge * e = gts_triangle_edge_opposite (i->data, v);
      if (!ALREADY_ENCROACHED (e) && 
	  GTS_IS_CONSTRAINT (e) &&
	  (* encroaches) (v, e, surface, data)) {
	gts_fifo_push (encroached, e);
	ALREADY_ENCROACHED (e) = encroached;
      }
    }
    i = i->next;
  }
  g_slist_free (triangles);
}

static void make_encroached_fifo (GtsEdge * e, gpointer * datas)
{
  GtsFifo * fifo = datas[0];
  GtsSurface * s = datas[1];
  GtsEncroachFunc encroaches = (GtsEncroachFunc) datas[2];
  gpointer data = datas[3];

  if (GTS_IS_CONSTRAINT (e) && 
      gts_edge_is_encroached (e, s, encroaches, data)) {
    gts_fifo_push (fifo, e);
    ALREADY_ENCROACHED (e) = fifo;
  }
}

#define SQUARE_ROOT_TWO 1.41421356237309504880168872420969807856967187
#define DISTANCE_2D(v1, v2) (sqrt ((GTS_POINT (v2)->x - GTS_POINT (v1)->x)*\
                                   (GTS_POINT (v2)->x - GTS_POINT (v1)->x) +\
                                   (GTS_POINT (v2)->y - GTS_POINT (v1)->y)*\
                                   (GTS_POINT (v2)->y - GTS_POINT (v1)->y)))

/* finds where to split the given edge to avoid infinite cycles. (see
   Shewchuk's thesis for details */
static GtsVertex * split_edge (GtsEdge * e,
			       GtsSurface * surface)
{
  GSList * i = e->triangles;
  GtsEdge * c = NULL;

  /* look for constraints touching e */
  while (i && !c) {
    GtsTriangle * t = i->data;
    if (GTS_IS_FACE (t) && 
	gts_face_has_parent_surface (GTS_FACE (t), surface)) {
      GtsEdge * e1, * e2;
      if (t->e1 == e) { e1 = t->e2; e2 = t->e3; }
      else if (t->e2 == e) { e1 = t->e1; e2 = t->e3; }
      else { e1 = t->e1; e2 = t->e2; }
      if (GTS_IS_CONSTRAINT (e1) && !GTS_IS_CONSTRAINT (e2))
	c = e1;
      else if (GTS_IS_CONSTRAINT (e2) && !GTS_IS_CONSTRAINT (e1))
	c = e2;
    }
    i = i->next;
  }
  if (c) {
    /* use power of two concentric shells */
    GtsVertex * v1 = GTS_SEGMENT (e)->v1;
    GtsVertex * v2 = GTS_SEGMENT (e)->v2;
    gdouble l = DISTANCE_2D (v1, v2);
    gdouble nearestpower = 1., split;

    while (l > SQUARE_ROOT_TWO*nearestpower)
      nearestpower *= 2.;
    while (l < SQUARE_ROOT_TWO*nearestpower/2.)
      nearestpower /= 2.;
    split = nearestpower/l/2.;

    if (GTS_SEGMENT (c)->v1 == v2 || GTS_SEGMENT (c)->v2 == v2)
      split = 1. - split;
    return gts_vertex_new (surface->vertex_class,
			   (1. - split)*GTS_POINT (v1)->x +
			   split*GTS_POINT (v2)->x,
			   (1. - split)*GTS_POINT (v1)->y +
			   split*GTS_POINT (v2)->y,
			   (1. - split)*GTS_POINT (v1)->z +
			   split*GTS_POINT (v2)->z);
  }
  else
    return gts_segment_midvertex (GTS_SEGMENT (e), surface->vertex_class);
}

static gint split_encroached (GtsSurface * surface, 
			      GtsFifo * encroached,
			      gint steiner_max,
			      GtsEncroachFunc encroaches,
			      gpointer data)
{
  GtsSegment * s;

  while (steiner_max-- != 0 && (s = gts_fifo_pop (encroached))) {
    GtsVertex * v = split_edge (GTS_EDGE (s), surface);
    GtsFace * boundary = gts_edge_is_boundary (GTS_EDGE (s), surface);
    GtsFace * f = boundary;
#if 1
    GtsEdge * e1 = GTS_EDGE (gts_object_clone (GTS_OBJECT (s)));
    GtsEdge * e2 = GTS_EDGE (gts_object_clone (GTS_OBJECT (s)));

    GTS_SEGMENT (e1)->v1 = s->v1;
    s->v1->segments = g_slist_prepend (s->v1->segments, e1);
    GTS_SEGMENT (e1)->v2 = v;
    v->segments = g_slist_prepend (v->segments, e1);

    GTS_SEGMENT (e2)->v1 = v;
    v->segments = g_slist_prepend (v->segments, e2);
    GTS_SEGMENT (e2)->v2 = s->v2;
    s->v2->segments = g_slist_prepend (s->v2->segments, e2);
#else
    GtsEdge * e1 = gts_edge_new (GTS_EDGE_CLASS (GTS_OBJECT (s)->klass),
				 s->v1, v);
    GtsEdge * e2 = gts_edge_new (GTS_EDGE_CLASS (GTS_OBJECT (s)->klass),
				 v, s->v2);
#endif

    GTS_OBJECT (s)->klass = GTS_OBJECT_CLASS (surface->edge_class);

    if (f == NULL)
      g_assert ((f = gts_edge_has_parent_surface (GTS_EDGE (s), surface)));
    g_assert (gts_delaunay_add_vertex_to_face (surface, v, f) == NULL);

    if (boundary)
      gts_object_destroy (GTS_OBJECT (s));

    vertex_encroaches (v, surface, encroached, encroaches, data);

    if (gts_edge_is_encroached (e1, surface, encroaches, data)) {
      gts_fifo_push (encroached, e1);
      ALREADY_ENCROACHED (e1) = encroached;
    }
    if (gts_edge_is_encroached (e2, surface, encroaches, data)) {
      gts_fifo_push (encroached, e2);
      ALREADY_ENCROACHED (e2) = encroached;
    }
  }

  return steiner_max;
}

/**
 * gts_delaunay_conform:
 * @surface: a #GtsSurface describing a constrained Delaunay triangulation.
 * @steiner_max: maximum number of Steiner points.
 * @encroaches: a #GtsEncroachFunc.
 * @data: user-data to pass to @encroaches.
 *
 * Recursively split constraints of @surface which are encroached by
 * vertices of @surface (see Shewchuk 96 for details). The split
 * constraints are destroyed and replaced by a set of new constraints
 * of the same class. If gts_vertex_encroaches_edge() is used for
 * @encroaches, the resulting surface will be Delaunay conforming.
 *
 * If @steiner_max is positive or nul, the recursive splitting
 * procedure will stop when this maximum number of Steiner points is
 * reached. In that case the resulting surface will not necessarily be
 * Delaunay conforming.
 *
 * Returns: the number of remaining encroached edges. If @steiner_max
 * is set to a negative value and gts_vertex_encroaches_edge() is used
 * for @encroaches this should always be zero. 
 */
guint gts_delaunay_conform (GtsSurface * surface,
			    gint steiner_max,
			    GtsEncroachFunc encroaches,
			    gpointer data)
{
  GtsFifo * encroached;
  gpointer datas[4];
  guint encroached_number;

  g_return_val_if_fail (surface != NULL, 0);
  g_return_val_if_fail (surface != NULL, 0);
  g_return_val_if_fail (encroaches != NULL, 0);

  datas[0] = encroached = gts_fifo_new ();
  datas[1] = surface;
  datas[2] = encroaches;
  datas[3] = data;
  gts_surface_foreach_edge (surface, (GtsFunc) make_encroached_fifo, datas);

  split_encroached (surface, 
		    encroached, 
		    steiner_max,
		    encroaches, data);
  gts_fifo_foreach (encroached, (GtsFunc) gts_object_reset_reserved, NULL);
  encroached_number = gts_fifo_size (encroached);
  gts_fifo_destroy (encroached);
  return encroached_number;
}

#define EHEAP_PAIR(f) (GTS_OBJECT (f)->reserved)

static void heap_surface_add_face (GtsSurface * s, GtsFace * f)
{
  GtsEHeap * heap = GTS_OBJECT (s)->reserved;
  gdouble key = gts_eheap_key (heap, f);

  if (key != 0.)
    EHEAP_PAIR (f) = gts_eheap_insert_with_key (heap, f, key);
  
  if (GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass->parent_class)->add_face)
    (* GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass->parent_class)->add_face) 
      (s, f);
}

static void heap_surface_remove_face (GtsSurface * s, GtsFace * f)
{
  GtsEHeap * heap = GTS_OBJECT (s)->reserved;

  if (EHEAP_PAIR (f))
    gts_eheap_remove (heap, EHEAP_PAIR (f));

  if (GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass->parent_class)->remove_face)
    (* GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass->parent_class)->remove_face) 
      (s, f);
}

static void heap_surface_class_init (GtsSurfaceClass * klass)
{
  klass->add_face = heap_surface_add_face;
  klass->remove_face = heap_surface_remove_face;
}

static GtsObjectClass * heap_surface_class_new (GtsObjectClass * parent_class)
{
  GtsObjectClassInfo heap_surface_info;

  heap_surface_info = parent_class->info;
  heap_surface_info.class_init_func = (GtsObjectClassInitFunc)
    heap_surface_class_init;
  return gts_object_class_new (parent_class,
			       &heap_surface_info);
}

static void make_face_heap (GtsFace * f, GtsEHeap * heap)
{
  gdouble key = gts_eheap_key (heap, f);

  if (key != 0.)
    EHEAP_PAIR (f) = gts_eheap_insert_with_key (heap, f, key);
}

/**
 * gts_delaunay_refine:
 * @surface: a #GtsSurface describing a conforming Delaunay triangulation.
 * @steiner_max: maximum number of Steiner points.
 * @encroaches: a #GtsEncroachFunc.
 * @encroach_data: user-data to pass to @encroaches.
 * @cost: a #GtsKeyFunc used to sort the faces during refinement.
 * @cost_data: user-data to pass to @cost.
 *
 * An implementation of the refinement algorithm described in Ruppert
 * (1995) and Shewchuk (1996).
 * 
 * Returns: the number of unrefined faces of @surface left. Should be zero
 * if @steiner_max is set to a negative value.
 */
guint gts_delaunay_refine (GtsSurface * surface,
			   gint steiner_max,
			   GtsEncroachFunc encroaches,
			   gpointer encroach_data,
			   GtsKeyFunc cost,
			   gpointer cost_data)
{
  GtsObjectClass * heap_surface_class;
  GtsObjectClass * original_class;
  GtsEHeap * heap;
  GtsFifo * encroached;
  GtsFace * f;
  guint unrefined_number;

  g_return_val_if_fail (surface != NULL, 0);
  g_return_val_if_fail (encroaches != NULL, 0);
  g_return_val_if_fail (cost != NULL, 0);

  original_class = GTS_OBJECT (surface)->klass;
  heap_surface_class = heap_surface_class_new (original_class);
  GTS_OBJECT (surface)->klass = heap_surface_class;

  heap = gts_eheap_new (cost, cost_data);
  gts_surface_foreach_face (surface, (GtsFunc) make_face_heap, heap);
  encroached = gts_fifo_new ();
  
  GTS_OBJECT (surface)->reserved = heap;

  while (steiner_max-- != 0 && (f = gts_eheap_remove_top (heap, NULL))) {
    GtsVertex * c = 
      GTS_VERTEX (gts_triangle_circumcircle_center (GTS_TRIANGLE (f),
		  GTS_POINT_CLASS (surface->vertex_class)));
    EHEAP_PAIR (f) = NULL;
    g_assert (c != NULL);
    g_assert (gts_delaunay_add_vertex (surface, c, f) == NULL);

    vertex_encroaches (c, surface, encroached, encroaches, encroach_data);
    if (!gts_fifo_is_empty (encroached)) {
      gts_delaunay_remove_vertex (surface, c);
      steiner_max = split_encroached (surface, 
				      encroached, 
				      steiner_max, 
				      encroaches, 
				      encroach_data);
    }
  }

  unrefined_number = gts_eheap_size (heap);
  gts_eheap_foreach (heap, (GFunc) gts_object_reset_reserved, NULL);
  gts_eheap_destroy (heap);

  gts_fifo_foreach (encroached, (GtsFunc) gts_object_reset_reserved, NULL);
  gts_fifo_destroy (encroached);

  GTS_OBJECT (surface)->klass = original_class;
  GTS_OBJECT (surface)->reserved = NULL;
  g_free (heap_surface_class);

  return unrefined_number;
}
