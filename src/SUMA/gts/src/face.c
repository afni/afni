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

#include "gts.h"

gboolean gts_allow_floating_faces = FALSE;

static void face_destroy (GtsObject * object)
{
  GtsFace * face = GTS_FACE (object);
  GSList * i;

  i = face->surfaces;
  while (i) {
    GSList * next = i->next;
    gts_surface_remove_face (i->data, face);
    i = next;
  }
  g_assert (face->surfaces == NULL);

  (* GTS_OBJECT_CLASS (gts_face_class ())->parent_class->destroy) (object);
}

static void face_clone (GtsObject * clone, GtsObject * object)
{
  (* GTS_OBJECT_CLASS (gts_face_class ())->parent_class->clone) (clone, 
								 object);
  GTS_FACE (clone)->surfaces = NULL;
}

static void face_class_init (GtsFaceClass * klass)
{
  GTS_OBJECT_CLASS (klass)->clone = face_clone;
  GTS_OBJECT_CLASS (klass)->destroy = face_destroy;
}

static void face_init (GtsFace * face)
{
  face->surfaces = NULL;
}

/**
 * gts_face_class:
 *
 * Returns: the #GtsFaceClass.
 */
GtsFaceClass * gts_face_class (void)
{
  static GtsFaceClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo face_info = {
      "GtsFace",
      sizeof (GtsFace),
      sizeof (GtsFaceClass),
      (GtsObjectClassInitFunc) face_class_init,
      (GtsObjectInitFunc) face_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_triangle_class ()), 
				  &face_info);
  }

  return klass;
}

/**
 * gts_face_new:
 * @klass: a #GtsFaceClass.
 * @e1: a #GtsEdge.
 * @e2: a #GtsEdge.
 * @e3: a #GtsEdge.
 *
 * Returns: a new #GtsFace using @e1, @e2 and @e3 as edges.
 */
GtsFace * gts_face_new (GtsFaceClass * klass,
			GtsEdge * e1, GtsEdge * e2, GtsEdge * e3)
{
  GtsFace * f;

  f = GTS_FACE (gts_object_new (GTS_OBJECT_CLASS (klass)));
  gts_triangle_set (GTS_TRIANGLE (f), e1, e2, e3);

  return f;
}

/**
 * gts_face_has_parent_surface:
 * @f: a #GtsFace.
 * @s: a #GtsSurface.
 *
 * Returns: %TRUE if @f belongs to @s, %FALSE otherwise.
 */
gboolean gts_face_has_parent_surface (GtsFace * f, GtsSurface * s)
{
  GSList * i;

  g_return_val_if_fail (f != NULL, FALSE);

  i = f->surfaces;
  while (i) {
    if (i->data == s)
      return TRUE;
    i = i->next;
  }
  return FALSE;
}

/**
 * gts_faces_from_edges:
 * @edges: a list of #GtsEdge.
 * @s: a #GtsSurface or %NULL.
 *
 * Builds a list of unique faces which belong to @s and have
 * one of their edges in @edges.
 * 
 * Returns: the list of faces.
 */
GSList * gts_faces_from_edges (GSList * edges, GtsSurface * s)
{
  GHashTable * hash;
  GSList * faces = NULL, * i;

  hash = g_hash_table_new (NULL, NULL);
  i = edges;
  while (i) {
    GSList * j = GTS_EDGE (i->data)->triangles;
    while (j) {
      GtsTriangle * t = j->data;
      if (GTS_IS_FACE (t) &&
	  (!s || gts_face_has_parent_surface (GTS_FACE (t), s)) && 
	  g_hash_table_lookup (hash, t) == NULL) {
	faces = g_slist_prepend (faces, t);
	g_hash_table_insert (hash, t, i);
      }
      j = j->next;
    }
    i = i->next;
  }
  g_hash_table_destroy (hash);

  return faces;
}

/**
 * gts_face_neighbor_number:
 * @f: a #GtsFace.
 * @s: a #GtsSurface or %NULL.
 *
 * Returns: the number of faces neighbors of @f and belonging to @s.
 */
guint gts_face_neighbor_number (GtsFace * f, GtsSurface * s)
{
  GSList * i;
  guint nn = 0;
  GtsEdge * e[4], ** e1 = e;
  
  g_return_val_if_fail (f != NULL, 0);
  
  e[0] = GTS_TRIANGLE (f)->e1; 
  e[1] = GTS_TRIANGLE (f)->e2; 
  e[2] = GTS_TRIANGLE (f)->e3; 
  e[3] = NULL;
  while (*e1) {
    i = (*e1++)->triangles;
    while (i) {
      GtsTriangle * t = i->data;
      if (GTS_FACE (t) != f && 
	  GTS_IS_FACE (t) && 
	  (!s || gts_face_has_parent_surface (GTS_FACE (t), s)))
	nn++;
      i = i->next;
    }
  }

  return nn;
}

/**
 * gts_face_neighbors:
 * @f: a #GtsFace.
 * @s: a #GtsSurface or %NULL.
 *
 * Returns: a list of unique #GtsFace neighbors of @f and belonging to @s.
 */
GSList * gts_face_neighbors (GtsFace * f, GtsSurface * s)
{
  GSList * i, * list = NULL;
  GtsEdge * e[4], ** e1 = e;
  
  g_return_val_if_fail (f != NULL, NULL);

  e[0] = GTS_TRIANGLE (f)->e1; 
  e[1] = GTS_TRIANGLE (f)->e2; 
  e[2] = GTS_TRIANGLE (f)->e3; 
  e[3] = NULL;
  while (*e1) {
    i = (*e1++)->triangles;
    while (i) {
      GtsTriangle * t = i->data;
      if (GTS_FACE (t) != f && 
	  GTS_IS_FACE (t) && 
	  (!s || gts_face_has_parent_surface (GTS_FACE (t), s)))
	list = g_slist_prepend (list, t);
      i = i->next;
    }
  }

  return list;
}

/**
 * gts_face_foreach_neighbor:
 * @f: a #GtsFace.
 * @s: a #GtsSurface or %NULL.
 * @func: a #GtsFunc.
 * @data: user data to pass to @func.
 *
 * Calls @func for each neighbor of @f belonging to @s (if not %NULL).
 */
void gts_face_foreach_neighbor (GtsFace * f, 
				GtsSurface * s, 
				GtsFunc func,
				gpointer data)
{
  GSList * i;
  GtsEdge * e[4], ** e1 = e;
  
  g_return_if_fail (f != NULL);
  g_return_if_fail (func != NULL);

  e[0] = GTS_TRIANGLE (f)->e1;
  e[1] = GTS_TRIANGLE (f)->e2; 
  e[2] = GTS_TRIANGLE (f)->e3; 
  e[3] = NULL;
  while (*e1) {
    i = (*e1++)->triangles;
    while (i) {
      GtsTriangle * t = i->data;
      if (GTS_FACE (t) != f && 
	  GTS_IS_FACE (t) && 
	  (!s || gts_face_has_parent_surface (GTS_FACE (t), s)))
	(* func) (t, data);
      i = i->next;
    }
  }
}

static gboolean triangle_is_incompatible (GtsTriangle * t, GtsEdge * e, GtsSurface * s)
{
  GSList * i = e->triangles;

  while (i) {
    if (i->data != t &&
	GTS_IS_FACE (i->data) &&
	gts_face_has_parent_surface (i->data, s) &&
	!gts_triangles_are_compatible (t, i->data, e))
      return TRUE;
    i = i->next;
  }
  return FALSE;
}

/**
 * gts_face_is_compatible:
 * @f: a #GtsFace.
 * @s: a #GtsSurface.
 *
 * Returns: %TRUE if @f is compatible with all its neighbors belonging
 * to @s, %FALSE otherwise.
 */
gboolean gts_face_is_compatible (GtsFace * f, GtsSurface * s)
{
  g_return_val_if_fail (f != NULL, FALSE);
  g_return_val_if_fail (s != NULL, FALSE);

  return !(triangle_is_incompatible (GTS_TRIANGLE (f), GTS_TRIANGLE (f)->e1, s) ||
	   triangle_is_incompatible (GTS_TRIANGLE (f), GTS_TRIANGLE (f)->e2, s) ||
	   triangle_is_incompatible (GTS_TRIANGLE (f), GTS_TRIANGLE (f)->e3, s));
}
