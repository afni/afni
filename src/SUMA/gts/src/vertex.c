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

gboolean gts_allow_floating_vertices = FALSE;

static void vertex_destroy (GtsObject * object)
{
  GtsVertex * vertex = GTS_VERTEX (object);
  GSList * i;

  i = vertex->segments;
  while (i) {
    GTS_OBJECT_SET_FLAGS (i->data, GTS_DESTROYED);
    i = i->next;
  }
  i = vertex->segments;
  while (i) {
    GSList * next = i->next;
    gts_object_destroy (i->data);
    i = next;
  }
  g_assert (vertex->segments == NULL);

  (* GTS_OBJECT_CLASS (gts_vertex_class ())->parent_class->destroy) (object);
}

static void vertex_clone (GtsObject * clone, GtsObject * object)
{
  (* GTS_OBJECT_CLASS (gts_vertex_class ())->parent_class->clone) (clone, 
								   object);
  GTS_VERTEX (clone)->segments = NULL;
}

static void vertex_class_init (GtsVertexClass * klass)
{
  klass->intersection_attributes = NULL;
  GTS_OBJECT_CLASS (klass)->clone = vertex_clone;
  GTS_OBJECT_CLASS (klass)->destroy = vertex_destroy;
}

static void vertex_init (GtsVertex * vertex)
{
  vertex->segments = NULL;
}

/**
 * gts_vertex_class:
 *
 * Returns: the #GtsVertexClass.
 */
GtsVertexClass * gts_vertex_class (void)
{
  static GtsVertexClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo vertex_info = {
      "GtsVertex",
      sizeof (GtsVertex),
      sizeof (GtsVertexClass),
      (GtsObjectClassInitFunc) vertex_class_init,
      (GtsObjectInitFunc) vertex_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_point_class ()), 
				  &vertex_info);
  }

  return klass;
}

/**
 * gts_vertex_new:
 * @klass: a #GtsVertexClass.
 * @x: the x-coordinate of the vertex to create.
 * @y: the y-coordinate of the vertex to create.
 * @z: the y-coordinate of the vertex to create.
 *
 * Returns: a new #GtsVertex with @x, @y and @z as coordinates.
 */
GtsVertex * gts_vertex_new (GtsVertexClass * klass,
			    gdouble x, gdouble y, gdouble z)
{
  GtsVertex * v;

  v = GTS_VERTEX (gts_object_new (GTS_OBJECT_CLASS (klass)));
  gts_point_set (GTS_POINT (v), x, y, z);

  return v;
}

/**
 * gts_vertex_replace:
 * @v: a #GtsVertex.
 * @with: another #GtsVertex.
 *
 * Replaces vertex @v with vertex @with. @v and @with must be
 * different.  All the #GtsSegment which have @v has one of their
 * vertices are updated.  The segments list of vertex @v is freed and
 * @v->segments is set to %NULL.  
 */
void gts_vertex_replace (GtsVertex * v, GtsVertex * with)
{
  GSList * i;

  g_return_if_fail (v != NULL);
  g_return_if_fail (with != NULL);
  g_return_if_fail (v != with);

  i = v->segments;
  while (i) {
    GtsSegment * s = i->data;
    if (s->v1 != with && s->v2 != with)
      with->segments = g_slist_prepend (with->segments, s);
    if (s->v1 == v) s->v1 = with;
    if (s->v2 == v) s->v2 = with;
    i = i->next;
  }
  g_slist_free (v->segments);
  v->segments = NULL;
}

/**
 * gts_vertex_is_unattached:
 * @v: a #GtsVertex.
 *
 * Returns: %TRUE if @v is not the endpoint of any #GtsSegment, 
 * %FALSE otherwise.
 */
gboolean gts_vertex_is_unattached (GtsVertex * v)
{
  g_return_val_if_fail (v != NULL, FALSE);
  if (v->segments == NULL)
    return TRUE;
  return FALSE;
}

/**
 * gts_vertices_are_connected:
 * @v1: a #GtsVertex.
 * @v2: another #GtsVertex.
 *
 * Returns: if @v1 and @v2 are the vertices of the same #GtsSegment
 * this segment else %NULL.
 */
GtsSegment * gts_vertices_are_connected (GtsVertex * v1, GtsVertex * v2)
{
  GSList * i;
  
  g_return_val_if_fail (v1 != NULL, FALSE);
  g_return_val_if_fail (v2 != NULL, FALSE);
  
  i = v1->segments;
  while (i) {
    GtsSegment * s = i->data;

    if (s->v1 == v2 || s->v2 == v2)
      return s;
    i = i->next;
  }
  return NULL;
}

/**
 * gts_vertices_from_segments:
 * @segments: a list of #GtsSegment.
 *
 * Returns: a list of #GtsVertex, vertices of a #GtsSegment in @segments.
 * Each element in the list is unique (no duplicates).
 */
GSList * gts_vertices_from_segments (GSList * segments)
{
  GHashTable * hash;
  GSList * vertices = NULL, * i;
  
  hash = g_hash_table_new (NULL, NULL);
  i = segments;
  while (i) {
    GtsSegment * s = i->data;
    if (g_hash_table_lookup (hash, s->v1) == NULL) {
      vertices = g_slist_prepend (vertices, s->v1);
      g_hash_table_insert (hash, s->v1, s);
    }
    if (g_hash_table_lookup (hash, s->v2) == NULL) {
      vertices = g_slist_prepend (vertices, s->v2);
      g_hash_table_insert (hash, s->v2, s);
    }
    i = i->next;
  }
  g_hash_table_destroy (hash);
  return vertices;
}

/**
 * gts_vertex_triangles:
 * @v: a #GtsVertex.
 * @list: a list of #GtsTriangle.
 *
 * Adds all the #GtsTriangle which share @v as a vertex and do not
 * already belong to @list.
 *
 * Returns: the new list of unique #GtsTriangle which share @v as a
 * vertex.  
 */
GSList * gts_vertex_triangles (GtsVertex * v, 
			       GSList * list)
{
  GSList * i;

  g_return_val_if_fail (v != NULL, NULL);

  i = v->segments;
  while (i) {
    GtsSegment * s = i->data;
    if (GTS_IS_EDGE (s)) {
      GSList * j = GTS_EDGE (s)->triangles;
      while (j) {
	if (!g_slist_find (list, j->data))
	  list = g_slist_prepend (list, j->data);
	j = j->next;
      }
    }
    i = i->next;
  }
  return list;
}

/**
 * gts_vertex_faces:
 * @v: a #GtsVertex.
 * @surface: a #GtsSurface or %NULL.
 * @list: a list of #GtsFace.
 *
 * Adds all the #GtsFace belonging to @surface (if not %NULL) which share 
 * @v as a vertex and do not already belong to @list.
 *
 * Returns: the new list of unique #GtsFace belonging to @surface 
 * which share @v as a vertex.
 */
GSList * gts_vertex_faces (GtsVertex * v, 
			   GtsSurface * surface, 
			   GSList * list)
{
  GSList * i;

  g_return_val_if_fail (v != NULL, NULL);

  i = v->segments;
  while (i) {
    GtsSegment * s = i->data;
    if (GTS_IS_EDGE (s)) {
      GSList * j = GTS_EDGE (s)->triangles;
      while (j) {
	GtsTriangle * t = j->data;
	if (GTS_IS_FACE (t) 
	    && 
	    (!surface || gts_face_has_parent_surface (GTS_FACE (t), surface)) 
	    &&
	    !g_slist_find (list, t))
	  list = g_slist_prepend (list, t);
	j = j->next;
      }
    }
    i = i->next;
  }
  return list;
}

/**
 * gts_vertex_neighbors:
 * @v: a #GtsVertex.
 * @list: a list of #GtsVertex.
 * @surface: a #GtsSurface or %NULL.
 *
 * Adds to @list all the #GtsVertex connected to @v by a #GtsSegment and not
 * already in @list. If @surface is not %NULL only the vertices connected to
 * @v by an edge belonging to @surface are considered.
 *
 * Returns: the new list of unique #GtsVertex.
 */
GSList * gts_vertex_neighbors (GtsVertex * v, 
			       GSList * list,
			       GtsSurface * surface)
{
  GSList * i;

  g_return_val_if_fail (v != NULL, NULL);

  i = v->segments;
  while (i) {
    GtsSegment * s = i->data;
    GtsVertex * v1 = s->v1 == v ? s->v2 : s->v1;
    if (v1 != v && 
	(!surface || 
	 (GTS_IS_EDGE (s) && 
	  gts_edge_has_parent_surface (GTS_EDGE (s), surface))) &&
	!g_slist_find (list, v1))
      list = g_slist_prepend (list, v1);
    i = i->next;
  }
  return list;
}

/**
 * gts_vertex_is_boundary:
 * @v: a #GtsVertex.
 * @surface: a #GtsSurface or %NULL.
 * 
 * Returns: %TRUE if @v is used by a #GtsEdge boundary of @surface as
 * determined by gts_edge_is_boundary(), %FALSE otherwise.
 */
gboolean gts_vertex_is_boundary (GtsVertex * v, GtsSurface * surface)
{
  GSList * i;

  g_return_val_if_fail (v != NULL, FALSE);
  
  i = v->segments;
  while (i) {
    if (GTS_IS_EDGE (i->data) && 
	gts_edge_is_boundary (i->data, surface))
      return TRUE;
    i = i->next;
  }

  return FALSE;
}

/**
 * gts_vertices_merge:
 * @vertices: a list of #GtsVertex.
 * @epsilon: half the size of the bounding box to consider for each vertex.
 * @check: function called for each pair of vertices about to be merged
 * or %NULL.
 *
 * For each vertex v in @vertices look if there are any vertex of
 * @vertices contained in a box centered on v of size 2*@epsilon. If
 * there are and if @check is not %NULL and returns %TRUE, replace
 * them with v (using gts_vertex_replace()), destroy them and remove
 * them from list.  This is done efficiently using Kd-Trees.
 *
 * Returns: the updated list of vertices.  
 */
GList * gts_vertices_merge (GList * vertices, 
			    gdouble epsilon,
			    gboolean (* check) (GtsVertex *, GtsVertex *))
{
  GPtrArray * array;
  GList * i;
  GNode * kdtree;

  g_return_val_if_fail (vertices != NULL, 0);

  array = g_ptr_array_new ();
  i = vertices;
  while (i) {
    g_ptr_array_add (array, i->data);
    i = i->next;
  }
  kdtree = gts_kdtree_new (array, NULL);
  g_ptr_array_free (array, TRUE);
  
  i = vertices;
  while (i) {
    GtsVertex * v = i->data;
    if (!GTS_OBJECT (v)->reserved) { /* Do something only if v is active */
      GtsBBox * bbox;
      GSList * selected, * j;

      /* build bounding box */
      bbox = gts_bbox_new (gts_bbox_class (),
			   v, 
			   GTS_POINT (v)->x - epsilon,
			   GTS_POINT (v)->y - epsilon,
			   GTS_POINT (v)->z - epsilon,
			   GTS_POINT (v)->x + epsilon,
			   GTS_POINT (v)->y + epsilon,
			   GTS_POINT (v)->z + epsilon);

      /* select vertices which are inside bbox using kdtree */
      j = selected = gts_kdtree_range (kdtree, bbox, NULL);
      while (j) {
	GtsVertex * sv = j->data;
	if (sv != v && !GTS_OBJECT (sv)->reserved && (!check || (*check) (sv, v))) {
	  /* sv is not v and is active */
	  gts_vertex_replace (sv, v);
	  GTS_OBJECT (sv)->reserved = sv; /* mark sv as inactive */
	}
	j = j->next;
      }
      g_slist_free (selected);
      gts_object_destroy (GTS_OBJECT (bbox));
    }
    i = i->next;
  }

  gts_kdtree_destroy (kdtree);

  /* destroy inactive vertices and removes them from list */

  /* we want to control vertex destruction */
  gts_allow_floating_vertices = TRUE;

  i = vertices;
  while (i) {
    GtsVertex * v = i->data;
    GList * next = i->next;
    if (GTS_OBJECT (v)->reserved) { /* v is inactive */
      gts_object_destroy (GTS_OBJECT (v));
      vertices = g_list_remove_link (vertices, i);
      g_list_free_1 (i);
    }
    i = next;
  }
  gts_allow_floating_vertices = FALSE; 

  return vertices;
}

/* returns the list of edges belonging to @surface turning around @v */
static GSList * edge_fan_list (GtsVertex * v,
			       GtsSurface * surface,
			       GtsFace * f, 
			       GtsEdge * e,
			       GtsFace * first)
{
  GSList * i = e->triangles;
  GtsFace * neighbor = NULL;
  GtsEdge * next = NULL, * enext = NULL;

  while (i) {
    GtsFace * f1 = i->data;
    if (GTS_IS_FACE (f1) &&
	f1 != f &&
	gts_face_has_parent_surface (f1, surface)) {
      g_return_val_if_fail (neighbor == NULL, NULL); /* non-manifold edge */
      neighbor = f1;
    }
    i = i->next;
  }
  if (neighbor == NULL || neighbor == first) /* end of fan */
    return NULL;

  if (GTS_TRIANGLE (neighbor)->e1 == e) {
    next = GTS_TRIANGLE (neighbor)->e2;
    enext = GTS_TRIANGLE (neighbor)->e3;
  }
  else if (GTS_TRIANGLE (neighbor)->e2 == e) {
    next = GTS_TRIANGLE (neighbor)->e3;
    enext = GTS_TRIANGLE (neighbor)->e1;
  }
  else if (GTS_TRIANGLE (neighbor)->e3 == e) {
    next = GTS_TRIANGLE (neighbor)->e1;
    enext = GTS_TRIANGLE (neighbor)->e2;
  }
  else
    g_assert_not_reached ();

  /* checking for correct orientation */
  g_return_val_if_fail (GTS_SEGMENT (enext)->v1 == v ||
			GTS_SEGMENT (enext)->v2 == v, NULL);

  return g_slist_prepend (edge_fan_list (v, surface, neighbor, enext, first), 
			  next);
}

/**
 * gts_vertex_fan_oriented:
 * @v: a #GtsVertex.
 * @surface: a #GtsSurface.
 *
 * Returns: a list of #GtsEdge describing in counterclockwise order the 
 * boundary of the fan of summit @v, the faces of the fan belonging to 
 * @surface.
 */
GSList * gts_vertex_fan_oriented (GtsVertex * v, GtsSurface * surface)
{
  GtsFace * f = NULL;
  guint d = 2;
  GSList * i;
  GtsVertex * v1, * v2, * v3;
  GtsEdge * e1, * e2, * e3;

  g_return_val_if_fail (v != NULL, NULL);
  g_return_val_if_fail (surface != NULL, NULL);

  i = v->segments;
  while (i) {
    GtsEdge * e = i->data;
    if (GTS_IS_EDGE (e)) {
      GSList * j = e->triangles;
      GtsFace * f1 = NULL;
      guint degree = 0;
      while (j) {
	if (GTS_IS_FACE (j->data) &&
	    gts_face_has_parent_surface (j->data, surface)) {
	  f1 = j->data;
	  degree++;
	}
	j = j->next;
      }
      if (f1 != NULL) {
	g_return_val_if_fail (degree <= 2, NULL); /* non-manifold edge */
	if (degree == 1) {
	  gts_triangle_vertices_edges (GTS_TRIANGLE (f1), NULL,
				       &v1, &v2, &v3, &e1, &e2, &e3);
	  if (v == v2) {
	    e2 = e3;
	    e3 = e1;
	  }
	  else if (v == v3) {
	    e3 = e2;
	    e2 = e1;
	  }
	  if (e3 != e) {
	    d = 1;
	    f = f1;
	  }
	}
	else if (degree <= d)
	  f = f1;
      }
    }
    i = i->next;
  }

  if (f == NULL)
    return NULL;

  gts_triangle_vertices_edges (GTS_TRIANGLE (f), NULL,
			       &v1, &v2, &v3, &e1, &e2, &e3);
  if (v == v2) {
    e2 = e3;
    e3 = e1;
  }
  else if (v == v3) {
    e3 = e2;
    e2 = e1;
  }

  return g_slist_prepend (edge_fan_list (v, surface, f, e3, f), e2);
}

#define edge_use_vertex(e, v) (GTS_SEGMENT(e)->v1 == v ||\
			       GTS_SEGMENT(e)->v2 == v)

static GtsEdge * replace_vertex (GtsTriangle * t, 
				 GtsEdge * e1,
				 GtsVertex * v, 
				 GtsVertex * with)
{
  GtsEdge * e = NULL;

  if (t->e1 != e1 && edge_use_vertex (t->e1, v))
    e = t->e1;
  else if (t->e2 != e1 && edge_use_vertex (t->e2, v))
    e = t->e2;
  else if (t->e3 != e1 && edge_use_vertex (t->e3, v))
    e = t->e3;
  else
    return NULL;

  if (with != v) {
    GtsSegment * s = GTS_SEGMENT (e);
    if (s->v1 == v) s->v1 = with;
    if (s->v2 == v) s->v2 = with;
    with->segments = g_slist_prepend (with->segments, s);
    v->segments = g_slist_remove (v->segments, s);
  }

  return e;
}

static void triangle_next (GtsEdge * e, GtsVertex * v, GtsVertex * with)
{
  GSList * i;

  if (e == NULL)
    return;
    
  i = e->triangles;
  while (i) {
    GtsTriangle * t = i->data;
    if (GTS_OBJECT (t)->reserved) {
      GTS_OBJECT (t)->reserved = NULL;
      triangle_next (replace_vertex (t, e, v, with), v, with);
    }
    i = i->next;
  }
}

/** 
 * gts_vertex_is_contact: 
 * @v: a #GtsVertex.  
 * @sever: if %TRUE and if @v is a contact vertex between two or more
 * sets of connected triangles replaces it with as many vertices,
 * clones of @v.
 *
 * Returns: the number of sets of connected triangles sharing @v as a
 * contact vertex.  
 */
guint gts_vertex_is_contact (GtsVertex * v, gboolean sever)
{
  GSList * triangles, * i;
  GtsVertex * with = v;
  guint ncomponent = 0;

  g_return_val_if_fail (v != NULL, 0);

  triangles = gts_vertex_triangles (v, NULL);
  i = triangles;
  while (i) {
    GTS_OBJECT (i->data)->reserved = i;
    i = i->next;
  }

  i = triangles;
  while (i) {
    GtsTriangle * t = i->data;
    if (GTS_OBJECT (t)->reserved) {
      GtsEdge * e;
      if (ncomponent && sever)
	with = GTS_VERTEX (gts_object_clone (GTS_OBJECT (v)));
      GTS_OBJECT (t)->reserved = NULL;
      e = replace_vertex (t, NULL, v, with);
      triangle_next (e, v, with);
      triangle_next (replace_vertex (t, e, v, with), v, with);
      ncomponent++;
    }
    i = i->next;
  }
  g_slist_free (triangles);

  return ncomponent;
}

/* GtsVertexNormal: Object */

static void vertex_normal_attributes (GtsVertex * v,
				      GtsObject * e,
				      GtsObject * t)
{
  g_return_if_fail (GTS_IS_EDGE (e));
  g_return_if_fail (GTS_IS_TRIANGLE (t));

  if (GTS_IS_VERTEX_NORMAL (GTS_SEGMENT (e)->v1) &&
      GTS_IS_VERTEX_NORMAL (GTS_SEGMENT (e)->v2)) {
    GtsPoint * p1 = GTS_POINT (GTS_SEGMENT (e)->v1);
    GtsPoint * p2 = GTS_POINT (GTS_SEGMENT (e)->v2);
    GtsPoint * p = GTS_POINT (v);
    gdouble a, b, lambda;
    guint i;

    a = p2->x - p1->x; b = p->x - p1->x;
    if (fabs (p2->y - p1->y) > fabs (a)) {
      a = p2->y - p1->y; b = p->y - p1->y;      
    }
    if (fabs (p2->z - p1->z) > fabs (a)) {
      a = p2->z - p1->z; b = p->z - p1->z;      
    }
    lambda = a != 0. ? b/a : 0.;
    for (i = 0; i < 3; i++)
      GTS_VERTEX_NORMAL (v)->n[i] = 
	(1. - lambda)*GTS_VERTEX_NORMAL (GTS_SEGMENT (e)->v1)->n[i] +
	lambda*GTS_VERTEX_NORMAL (GTS_SEGMENT (e)->v2)->n[i];
  }
  else {
    GtsVertex * v1, * v2, * v3;

    gts_triangle_vertices (GTS_TRIANGLE (t), &v1, &v2, &v3);
    if (GTS_IS_VERTEX_NORMAL (v1) && 
	GTS_IS_VERTEX_NORMAL (v2) &&
	GTS_IS_VERTEX_NORMAL (v3)) {
      GtsVector a1, a2, a3, det;
      guint i, j = 0;
      gdouble l1, l2;

      gts_vector_init (a1, GTS_POINT (v1), GTS_POINT (v));
      gts_vector_init (a2, GTS_POINT (v1), GTS_POINT (v2));
      gts_vector_init (a3, GTS_POINT (v1), GTS_POINT (v3));
      gts_vector_cross (det, a2, a3);
      if (fabs (det[1]) > fabs (det[0])) j = 1;
      if (fabs (det[2]) > fabs (det[j])) j = 2;
      if (det[j] == 0.) {
	g_warning ("vertex_normal_attributes: det[%d] == 0.", j);
	return;
      }
      switch (j) {
      case 0: 
	l1 = (a1[1]*a3[2] - a1[2]*a3[1])/det[0]; 
	l2 = (a1[2]*a2[1] - a1[1]*a2[2])/det[0]; 
	break;
      case 1:
	l1 = (a1[2]*a3[0] - a1[0]*a3[2])/det[1];
	l2 = (a1[0]*a2[2] - a1[2]*a2[0])/det[1];
	break;
      case 2:
	l1 = (a1[0]*a3[1] - a1[1]*a3[0])/det[2];
	l2 = (a1[1]*a2[0] - a1[0]*a2[1])/det[2];
	break;
      default:
	l1 = l2 = 0.;
      }
      for (i = 0; i < 3; i++)
	GTS_VERTEX_NORMAL (v)->n[i] = 
	  GTS_VERTEX_NORMAL (v1)->n[i]*(1. - l1 - l2) +
	  GTS_VERTEX_NORMAL (v2)->n[i]*l1 +
	  GTS_VERTEX_NORMAL (v3)->n[i]*l2;
    }
  }
}

static void gts_vertex_normal_class_init (GtsVertexClass * klass)
{
  klass->intersection_attributes = vertex_normal_attributes;
}

GtsVertexClass * gts_vertex_normal_class (void)
{
  static GtsVertexClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo gts_vertex_normal_info = {
      "GtsVertexNormal",
      sizeof (GtsVertexNormal),
      sizeof (GtsVertexClass),
      (GtsObjectClassInitFunc) gts_vertex_normal_class_init,
      (GtsObjectInitFunc) NULL,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_vertex_class ()),
				  &gts_vertex_normal_info);
  }

  return klass;
}

/* GtsColorVertex: Object */

GtsVertexClass * gts_color_vertex_class (void)
{
  static GtsVertexClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo gts_color_vertex_info = {
      "GtsColorVertex",
      sizeof (GtsColorVertex),
      sizeof (GtsVertexClass),
      (GtsObjectClassInitFunc) NULL,
      (GtsObjectInitFunc) NULL,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_vertex_class ()),
				  &gts_color_vertex_info);
  }

  return klass;
}

