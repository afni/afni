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

gboolean gts_allow_floating_edges = FALSE;

static void edge_destroy (GtsObject * object)
{
  GtsEdge * edge = GTS_EDGE (object);
  GSList * i;

  i = edge->triangles;
  while (i) {
    GSList * next = i->next;
    gts_object_destroy (i->data);
    i = next;
  }
  g_assert (edge->triangles == NULL);

  (* GTS_OBJECT_CLASS (gts_edge_class ())->parent_class->destroy) (object);
}

static void edge_clone (GtsObject * clone, GtsObject * object)
{
  (* GTS_OBJECT_CLASS (gts_edge_class ())->parent_class->clone) (clone,
								 object);
  GTS_SEGMENT (clone)->v1 = GTS_SEGMENT (clone)->v2 = NULL;
  GTS_EDGE (clone)->triangles = NULL;
}

static void edge_class_init (GtsObjectClass * klass)
{
  klass->clone = edge_clone;
  klass->destroy = edge_destroy;
}

static void edge_init (GtsEdge * edge)
{
  edge->triangles = NULL;
}

/**
 * gts_edge_class:
 *
 * Returns: the #GtsEdgeClass.
 */
GtsEdgeClass * gts_edge_class (void)
{
  static GtsEdgeClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo edge_info = {
      "GtsEdge",
      sizeof (GtsEdge),
      sizeof (GtsEdgeClass),
      (GtsObjectClassInitFunc) edge_class_init,
      (GtsObjectInitFunc) edge_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_segment_class ()), 
				  &edge_info);
  }

  return klass;
}

/**
 * gts_edge_new:
 * @klass: a #GtsEdgeClass.
 * @v1: a #GtsVertex.
 * @v2: a #GtsVertex.
 *
 * Returns: a new #GtsEdge linking @v1 and @v2.
 */
GtsEdge * gts_edge_new (GtsEdgeClass * klass,
			GtsVertex * v1, GtsVertex * v2)
{
  return GTS_EDGE (gts_segment_new (GTS_SEGMENT_CLASS (klass), v1, v2));
}

/**
 * gts_edge_replace:
 * @e: a #GtsEdge.
 * @with: a #GtsEdge.
 *
 * Replaces @e with @with. For each triangle which uses @e as an
 * edge, @e is replaced with @with. The @with->triangles list is
 * updated appropriately and the @e->triangles list is freed and set
 * to %NULL.
 */
void gts_edge_replace (GtsEdge * e, GtsEdge * with)
{
  GSList * i;

  g_return_if_fail (e != NULL && with != NULL && e != with);

  i = e->triangles;
  while (i) {
    GtsTriangle * t = i->data;
    if (t->e1 == e) t->e1 = with;
    if (t->e2 == e) t->e2 = with;
    if (t->e3 == e) t->e3 = with;
    if (!g_slist_find (with->triangles, t))
      with->triangles = g_slist_prepend (with->triangles, t);
    i = i->next;
  }
  g_slist_free (e->triangles);
  e->triangles = NULL;
}

/**
 * gts_edge_has_parent_surface:
 * @e: a #GtsEdge.
 * @surface: a #GtsSurface.
 * 
 * Returns: a #GtsFace of @surface having @e as an edge, %NULL otherwise.
 */
GtsFace * gts_edge_has_parent_surface (GtsEdge * e, GtsSurface * surface)
{
  GSList * i;

  g_return_val_if_fail (e != NULL, NULL);

  i = e->triangles;
  while (i) {
    if (GTS_IS_FACE (i->data) && 
	gts_face_has_parent_surface (i->data, surface))
      return i->data;
    i = i->next;
  }
  return NULL;
}

/**
 * gts_edge_has_any_parent_surface:
 * @e: a #GtsEdge.
 * 
 * Returns: %NULL if @e is not an edge of any triangle or if all the
 * faces having @e has an edge do not belong to any surface,
 * a #GtsFace belonging to a surface and having @e as an edge.
 */
GtsFace * gts_edge_has_any_parent_surface (GtsEdge * e)
{
  GSList * i;

  g_return_val_if_fail (e != NULL, NULL);

  i = e->triangles;
  while (i) {
    GtsTriangle * t = i->data;
    if (GTS_IS_FACE (t) && GTS_FACE (t)->surfaces != NULL)
      return GTS_FACE (t);
    i = i->next;
  }
  return NULL;
}

/**
 * gts_edge_is_boundary:
 * @e: a #GtsEdge.
 * @surface: a #GtsSurface or %NULL.
 * 
 * Returns: the unique #GtsFace (which belongs to @surface) and which
 * has @e as an edge (i.e. @e is a boundary edge (of @surface)) or %NULL 
 * if there is more than one or no faces (belonging to @surface) and
 * with @e as an edge.
 */
GtsFace * gts_edge_is_boundary (GtsEdge * e, GtsSurface * surface)
{
  GSList * i;
  GtsFace * f = NULL;
  
  g_return_val_if_fail (e != NULL, NULL);
  
  i = e->triangles;
  while (i) {
    if (GTS_IS_FACE (i->data)) {
      if (!surface || gts_face_has_parent_surface (i->data, surface)) {
	if (f != NULL)
	  return NULL;
	f = i->data;
      }
    }
    i = i->next;    
  }
  return f;
}

/**
 * gts_edges_from_vertices:
 * @vertices: a list of #GtsVertex.
 * @parent: a #GtsSurface.
 * 
 * Returns: a list of unique #GtsEdge which have one of their vertices in 
 * @vertices and are used by a face of @parent. 
 */
GSList * gts_edges_from_vertices (GSList * vertices, GtsSurface * parent)
{
  GHashTable * hash;
  GSList * edges = NULL, * i;

  g_return_val_if_fail (parent != NULL, NULL);
  
  hash = g_hash_table_new (NULL, NULL);
  i = vertices;
  while (i) {
    GSList * j = GTS_VERTEX (i->data)->segments;
    while (j) {
      GtsSegment * s = j->data;
      if (GTS_IS_EDGE (s) &&
	  gts_edge_has_parent_surface (GTS_EDGE (s), parent) && 
	  g_hash_table_lookup (hash, s) == NULL) {
	edges = g_slist_prepend (edges, s);
	g_hash_table_insert (hash, s, i);
      }
      j = j->next;
    }
    i = i->next;
  }
  g_hash_table_destroy (hash);
  return edges;
}

/**
 * gts_edge_face_number:
 * @e: a #GtsEdge.
 * @s: a #GtsSurface.
 *
 * Returns: the number of faces using @e and belonging to @s.
 */
guint gts_edge_face_number (GtsEdge * e, GtsSurface * s)
{
  GSList * i;
  guint nt = 0;

  g_return_val_if_fail (e != NULL, 0);
  g_return_val_if_fail (s != NULL, 0);

  i = e->triangles;
  while (i) {
    if (GTS_IS_FACE (i->data) && 
	gts_face_has_parent_surface (GTS_FACE (i->data), s))
      nt++;
    i = i->next;
  }
  return nt;
}

/**
 * gts_edge_is_duplicate:
 * @e: a #GtsEdge.
 *
 * Returns: the first #GtsEdge different from @e which shares the
 * same endpoints or %NULL if there is none.
 */
GtsEdge * gts_edge_is_duplicate (GtsEdge * e)
{
  GSList * i;
  GtsVertex * v2;

  g_return_val_if_fail (e != NULL, NULL);

  v2 = GTS_SEGMENT (e)->v2;
  i = GTS_SEGMENT (e)->v1->segments;
  if (GTS_SEGMENT (e)->v1 == v2) /* e is degenerate: special treatment */
    while (i) {
      GtsSegment * s = i->data;
      if (s != GTS_SEGMENT (e) &&
	  GTS_IS_EDGE (s) && 
	  s->v1 == v2 && s->v2 == v2)
	return GTS_EDGE (s);
      i = i->next;
    }
  else /* e is not degenerate */
    while (i) {
      GtsSegment * s = i->data;
      if (s != GTS_SEGMENT (e) &&
	  GTS_IS_EDGE (s) && 
	  (s->v1 == v2 || s->v2 == v2))
	return GTS_EDGE (s);
      i = i->next;
    }
  return NULL;
}

/**
 * gts_edges_merge:
 * @edges: a list of #GtsEdge.
 *
 * For each edge in @edges check if it is duplicated (as
 * returned by gts_edge_is_duplicate()). If it is replace it by its
 * duplicate, destroy it and remove it from the list.
 *
 * Returns: the updated @edges list.
 */
GList * gts_edges_merge (GList * edges)
{
  GList * i = edges;

  /* we want to control edge destruction */
  gts_allow_floating_edges = TRUE;
  while (i) {
    GtsEdge * e = i->data;
    GtsEdge * de = gts_edge_is_duplicate (e);
    if (de) {
      GList * next = i->next;
      edges = g_list_remove_link (edges, i);
      g_list_free_1 (i);
      i = next;
      gts_edge_replace (e, de);
      gts_object_destroy (GTS_OBJECT (e));
    }
    else
      i = i->next;
  }
  gts_allow_floating_edges = FALSE;;

  return edges;
}

static void triangle_vertices_edges (GtsTriangle * t, 
				     GtsEdge * e,
				     GtsVertex ** v,
				     GtsEdge ** ee1,
				     GtsEdge ** ee2)
{
  GtsEdge * e1 = t->e1, * e2 = t->e2, * e3 = t->e3;
  GtsVertex * v1 = GTS_SEGMENT (e)->v1;

  if (e1 == e)        e1 = e3;
  else if (e2 == e)   e2 = e3;
  else                g_assert (e3 == e);

  if (GTS_SEGMENT (e2)->v1 == v1 || GTS_SEGMENT (e2)->v2 == v1) {
    e3 = e1; e1 = e2; e2 = e3;
  }
  if (GTS_SEGMENT (e1)->v1 == v1)
    *v = GTS_SEGMENT (e1)->v2;
  else
    *v = GTS_SEGMENT (e1)->v1;
  *ee1 = e1;
  *ee2 = e2;
}

/**
 * gts_edge_belongs_to_tetrahedron:
 * @e: a #GtsEdge.
 *
 * Returns: %TRUE if @e is used by faces forming a tetrahedron, %FALSE
 * otherwise.
 */
gboolean gts_edge_belongs_to_tetrahedron (GtsEdge * e)
{
  GSList * i;
  GtsVertex * v1, * v2;

  g_return_val_if_fail (e != NULL, FALSE);

  v1 = GTS_SEGMENT (e)->v1;
  v2 = GTS_SEGMENT (e)->v2;
  i = e->triangles;
  while (i) {
    GtsEdge * e1, * e2;
    GtsVertex * vt1;
    GSList * j = i->next;
    triangle_vertices_edges (i->data, e, &vt1, &e1, &e2);
    while (j) {      
      GtsSegment * s5;
      GtsEdge * e3, * e4;
      GtsVertex * vt2;

      triangle_vertices_edges (j->data, e, &vt2, &e3, &e4);
      s5 = gts_vertices_are_connected (vt1, vt2);
      if (GTS_IS_EDGE (s5) &&
	  gts_triangle_use_edges (e1, e3, GTS_EDGE (s5)) &&
	  gts_triangle_use_edges (e2, e4, GTS_EDGE (s5)))
	return TRUE;
      j = j->next;
    }
    i = i->next;
  }

  return FALSE;
}

#define edge_use_vertex(e, v) (GTS_SEGMENT(e)->v1 == v ||\
			       GTS_SEGMENT(e)->v2 == v)

static GtsEdge * next_edge (GtsTriangle * t,
			    GtsEdge * e1,
			    GtsEdge * e)
{
  GtsVertex * v1 = GTS_SEGMENT (e)->v1;
  GtsVertex * v2 = GTS_SEGMENT (e)->v2;
  
  if (t->e1 != e1 && t->e1 != e && 
      (edge_use_vertex (t->e1, v1) || edge_use_vertex (t->e1, v2)))
    return t->e1;
  else if (t->e2 != e1 && t->e2 != e && 
	   (edge_use_vertex (t->e2, v1) || edge_use_vertex (t->e2, v2)))
    return t->e2;
  else if (t->e3 != e1 && t->e3 != e && 
	   (edge_use_vertex (t->e3, v1) || edge_use_vertex (t->e3, v2)))
    return t->e3;
  g_assert_not_reached ();
  return NULL;
}

static void triangle_next (GtsEdge * e1, GtsEdge * e)
{
  GSList * i;

  i = e1->triangles;
  while (i) {
    GtsTriangle * t = i->data;
    if (GTS_OBJECT (t)->reserved) {
      GTS_OBJECT (t)->reserved = NULL;
      triangle_next (next_edge (t, e1, e), e);
    }
    i = i->next;
  }
}

/** 
 * gts_edge_is_contact: 
 * @e: a #GtsEdge.  
 *
 * Returns: the number of sets of connected triangles sharing @e as a
 * contact edge.  
 */
guint gts_edge_is_contact (GtsEdge * e)
{
  GSList * i, * triangles;
  guint ncomponent = 0;

  g_return_val_if_fail (e != NULL, 0);

  triangles = gts_vertex_triangles (GTS_SEGMENT (e)->v1, NULL);
  i = triangles = gts_vertex_triangles (GTS_SEGMENT (e)->v2, triangles);
  while (i) {
    GTS_OBJECT (i->data)->reserved = i;
    i = i->next;
  }

  i = e->triangles;
  while (i) {
    GtsTriangle * t = i->data;
    if (GTS_OBJECT (t)->reserved) {
      GtsEdge * e1;
      GTS_OBJECT (t)->reserved = NULL;
      e1 = next_edge (t, NULL, e);
      triangle_next (e1, e);
      triangle_next (next_edge (t, e1, e), e);
      ncomponent++;
    }
    i = i->next;
  }
   
  g_slist_foreach (triangles, (GFunc) gts_object_reset_reserved, NULL);
  g_slist_free (triangles);

  return ncomponent;
}

/**
 * gts_edge_swap:
 * @e: a #GtsEdge.
 * @s: a #GtsSurface.
 *
 * Performs an "edge swap" on the two triangles sharing @e and
 * belonging to @s.
 */
void gts_edge_swap (GtsEdge * e, GtsSurface * s)
{
  GtsTriangle * t1 = NULL, * t2 = NULL, * t = NULL;
  GtsFace * f;
  GSList * i;
  GtsVertex * v1, * v2, * v3, * v4, * v5, * v6;
  GtsEdge * e1, * e2, * e3, * e4;
  GtsSegment * v3v6;

  g_return_if_fail (e != NULL);
  g_return_if_fail (s != NULL);

  i = e->triangles;
  while (i) {
    if (GTS_IS_FACE (i->data) && gts_face_has_parent_surface (i->data, s)) {
      if (!t1)
	t1 = i->data;
      else if (!t2)
	t2 = i->data;
      else
	g_return_if_fail (gts_edge_face_number (e, s) == 2);
    }
    i = i->next;
  }
  g_assert (t1 && t2);

  gts_triangle_vertices_edges (t1, e, &v1, &v2, &v3, &e, &e1, &e2);
  gts_triangle_vertices_edges (t2, e, &v4, &v5, &v6, &e, &e3, &e4);
  g_assert (v2 == v4 && v1 == v5);

  v3v6 = gts_vertices_are_connected (v3, v6);
  if (!GTS_IS_EDGE (v3v6))
    v3v6 = GTS_SEGMENT (gts_edge_new (s->edge_class, v3, v6));
  f = gts_face_new (s->face_class, e1, GTS_EDGE (v3v6), e4);
  if ((t == gts_triangle_is_duplicate (GTS_TRIANGLE (f))) &&
      GTS_IS_FACE (t)) {
    gts_object_destroy (GTS_OBJECT (f));
    f = GTS_FACE (t);
  }
  gts_surface_add_face (s, f);

  f = gts_face_new (s->face_class, GTS_EDGE (v3v6), e2, e3);
  if ((t == gts_triangle_is_duplicate (GTS_TRIANGLE (f))) &&
      GTS_IS_FACE (t)) {
    gts_object_destroy (GTS_OBJECT (f));
    f = GTS_FACE (t);
  }
  gts_surface_add_face (s, f);

  gts_surface_remove_face (s, GTS_FACE (t1));
  gts_surface_remove_face (s, GTS_FACE (t2));
}

/**
 * gts_edge_manifold_faces:
 * @e: a #GtsEdge.
 * @s: a #GtsSurface.
 * @f1: pointer for first face.
 * @f2: pointer for second face.
 *
 * If @e is a manifold edge of surface @s, fills @f1 and @f2 with the
 * faces belonging to @s and sharing @e.
 *
 * Returns: %TRUE if @e is a manifold edge, %FALSE otherwise.
 */
gboolean gts_edge_manifold_faces (GtsEdge * e, GtsSurface * s,
				  GtsFace ** f1, GtsFace ** f2)
{
  GSList * i;

  g_return_val_if_fail (e != NULL, FALSE);
  g_return_val_if_fail (s != NULL, FALSE);
  g_return_val_if_fail (f1 != NULL, FALSE);
  g_return_val_if_fail (f2 != NULL, FALSE);

  *f1 = *f2 = NULL;
  i = e->triangles;
  while (i) {
    if (GTS_IS_FACE (i->data) && gts_face_has_parent_surface (i->data, s)) {
      if (!(*f1)) *f1 = i->data;
      else if (!(*f2)) *f2 = i->data;
      else return FALSE;
    }
    i = i->next;
  }

  return (*f1 && *f2);
}
