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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif


#include <math.h>
#include "gts.h"

#ifdef USE_SURFACE_BTREE

static gint find_closest (GtsTriangle * t, gpointer value, gpointer * data)
{
  guint * ns = data[2];
  guint * n = data[3];

  if (*n >= *ns)
    return TRUE;
  else {
    gdouble * dmin = data[0];
    gpointer * closest = data[1];
    GtsPoint * p = data[4];

    if (gts_triangle_orientation (t) > 0.) {
      GtsPoint * p1 = GTS_POINT (GTS_SEGMENT (t->e1)->v1);
      gdouble d = (p->x - p1->x)*(p->x - p1->x) + (p->y - p1->y)*(p->y - p1->y);
      
      if (d < *dmin) {
	*dmin = d;
	*closest = t;
      }
      (*n)++;
    }
  }
  return FALSE;
}

/* select the face closest to @p among n^1/3 randomly picked faces
 *  of @surface */
static GtsFace * closest_face (GtsSurface * s, GtsPoint * p)
{
  guint n = 0, nt, ns;
  gdouble dmin = G_MAXDOUBLE;
  GtsFace * closest = NULL;
  gpointer data[5];

  nt = gts_surface_face_number (s);
  if (!nt)
    return NULL;
  ns = exp (log ((gdouble) nt)/3.);

  data[0] = &dmin;
  data[1] = &closest;
  data[2] = &ns;
  data[3] = &n;
  data[4] = p;
  g_tree_traverse (s->faces, (GTraverseFunc) find_closest, G_IN_ORDER, data);

  return closest;
}

#else /* not USE_SURFACE_BTREE */

#  if GLIB_CHECK_VERSION(2,4,0)
/* finally, with g_hash_table_find we are able to stop iteration over the hash 
   table in the middle */

typedef struct _SFindClosest SFindClosest; 

struct _SFindClosest {
  gdouble dmin; 
  GtsFace *closest;
  GtsPoint * p;
  gint stop;
};

static gboolean find_closest (gpointer key, gpointer value, gpointer user_data)
{
  SFindClosest * data = (SFindClosest *) user_data;
  GtsFace * f = GTS_FACE (value);
  
  if (gts_triangle_orientation (GTS_TRIANGLE (f)) > 0.) {
    GtsPoint * p1 = GTS_POINT (GTS_SEGMENT (GTS_TRIANGLE (f)->e1)->v1);
    gdouble d = ((data->p->x - p1->x)*(data->p->x - p1->x) + 
		 (data->p->y - p1->y)*(data->p->y - p1->y));

    if (d < data->dmin) {
      data->dmin = d;
      data->closest = f;
    }
  }
  data->stop--;
  return !(data->stop > 0);
}

static GtsFace * closest_face (GtsSurface * s, GtsPoint * p)
{
  SFindClosest fc;

  fc.dmin = G_MAXDOUBLE;
  fc.closest = NULL;
  fc.p = p;
  fc.stop = (gint) exp (log ((gdouble) g_hash_table_size (s->faces))/3.);
  g_hash_table_find (s->faces, find_closest, &fc);
  
  return fc.closest;
}

#  else /* VERSION < 2.4.0 */

/* Due to an unkown reason g_hash_table_foreach does not allow to stop 
 * the loop, hence the redefinition. I hope they don't change
 * the GHashTable, GHashNode structures ... */
typedef struct _GHashNode      GHashNode;

struct _GHashNode
{
  gpointer key;
  gpointer value;
  GHashNode *next;
};

struct _GHashTable
{
  gint size;
  gint nnodes;
  guint frozen;
  GHashNode **nodes;
  GHashFunc hash_func;
  GCompareFunc key_compare_func;
};

/* select the face closest to @p among n^1/3 randomly picked faces
 * of @surface */
static GtsFace * closest_face (GtsSurface * s, GtsPoint * p)
{
  guint i, n, nt, ns;
  gdouble dmin = G_MAXDOUBLE;
  GtsFace * closest = NULL;
  GHashNode * node;
  GHashTable * hash_table = s->faces;

  nt = g_hash_table_size (hash_table);
  if (!nt)
    return NULL;

  ns = exp(log((gdouble) nt)/3.);
  for (i = 0, n = 0; i < hash_table->size && n < ns; i++)
    for (node = hash_table->nodes[i]; node && n < ns; node = node->next) {
      GtsFace * f = node->key;

      if (gts_triangle_orientation (GTS_TRIANGLE (f)) > 0.) {
	GtsPoint * p1 = GTS_POINT (GTS_SEGMENT (GTS_TRIANGLE (f)->e1)->v1);
	gdouble d = (p->x - p1->x)*(p->x - p1->x) + (p->y - p1->y)*(p->y - p1->y);
		 
	if (d < dmin) {
	  dmin = d;
	  closest = f;
	}
	n++;
      }
    }
  return closest;
}
#  endif /* VERSION < 2.4.0 */
#endif /* not USE_SURFACE_BTREE */

/* returns the face belonging to @surface and neighbor of @f via @e */
static GtsFace * neighbor (GtsFace * f,
			   GtsEdge * e,
			   GtsSurface * surface)
{
  GSList * i = e->triangles;
  GtsTriangle * t = GTS_TRIANGLE (f);

  while (i) {
    GtsTriangle * t1 = i->data;
    if (t1 != t &&
	GTS_IS_FACE (t1) &&
	gts_face_has_parent_surface (GTS_FACE (t1), surface))
      return GTS_FACE (t1);
    i = i->next;
  }
  return NULL;
}

/* given a triangle @t and a segment s (@o -> @p). 
   @o must be in @t. Returns the
   edge of @t which is intersected by s or %NULL if @p is also
   contained in @t (on_summit is set to %FALSE) or if s intersects @t 
   exactly on one of its summit (on_summit is set to %TRUE). */
static GtsEdge * triangle_next_edge (GtsTriangle * t,
				     GtsPoint * o, GtsPoint * p,
				     gboolean * on_summit)
{
  GtsVertex * v1, * v2, * v3;
  GtsEdge * e1, * e2, * e3;
  gdouble orient = 0.0;
  
  gts_triangle_vertices_edges (t, NULL,
			       &v1, &v2, &v3, 
			       &e1, &e2, &e3);

  *on_summit = FALSE;
  orient = gts_point_orientation (o, GTS_POINT (v1), p);
  if (orient > 0.0) {
    orient = gts_point_orientation (o, GTS_POINT (v2), p);
    if (orient > 0.0) {
      if (gts_point_orientation (GTS_POINT (v2), GTS_POINT (v3), p) >= 0.0)
	return NULL;
      return e2;
    }
    if (orient < 0.0) {
      if (gts_point_orientation (GTS_POINT (v1), GTS_POINT (v2), p) >= 0.0)
	return NULL;
      return e1;
    }
    if (gts_point_orientation (GTS_POINT (v1), GTS_POINT (v2), p) < 0.0)
      *on_summit = TRUE;
    return NULL;
  }

  if (orient < 0.0) {
    orient = gts_point_orientation (o, GTS_POINT (v3), p);
    if (orient > 0.0) {
      if (gts_point_orientation (GTS_POINT (v3), GTS_POINT (v1), p) >= 0.0)
	return NULL;
      return e3;
    }
    if (orient < 0.0) {
      if (gts_point_orientation (GTS_POINT (v2), GTS_POINT (v3), p) >= 0.0)
	return NULL;
      return e2;
    }
    if (gts_point_orientation (GTS_POINT (v3), GTS_POINT (v1), p) < 0.0)
      *on_summit = TRUE;
    return NULL;
  }
  
  if (gts_point_orientation (GTS_POINT (v2), GTS_POINT (v3), p) < 0.0)
    return e2;
  if (gts_point_orientation (GTS_POINT (v1), GTS_POINT (v2), p) < 0.0)
    *on_summit = TRUE;
  return NULL;
}

static void triangle_barycenter (GtsTriangle * t, GtsPoint * b)
{
  GtsPoint * p = GTS_POINT (gts_triangle_vertex (t));
  b->x = (p->x + 
	  GTS_POINT (GTS_SEGMENT(t->e1)->v1)->x +
	  GTS_POINT (GTS_SEGMENT(t->e1)->v2)->x)/3.;
  b->y = (p->y + 
	  GTS_POINT (GTS_SEGMENT(t->e1)->v1)->y +
	  GTS_POINT (GTS_SEGMENT(t->e1)->v2)->y)/3.;
}

static GtsFace * point_locate (GtsPoint * o,
			       GtsPoint * p,
			       GtsFace * f,
			       GtsSurface * surface)
{
  GtsEdge * prev;
  gboolean on_summit;
  GtsVertex * v1, * v2, * v3;
  GtsEdge * e2, * e3;    
  
  prev = triangle_next_edge (GTS_TRIANGLE (f), o, p, &on_summit);

  if (!prev) {
    GtsFace * f1;

    if (!on_summit)
      return f; /* p is inside f */

    /* s intersects f exactly on a summit: restarts from a neighbor of f */
    if ((f1 = neighbor (f, GTS_TRIANGLE (f)->e1, surface)) ||
	(f1 = neighbor (f, GTS_TRIANGLE (f)->e2, surface)) ||
	(f1 = neighbor (f, GTS_TRIANGLE (f)->e3, surface))) {
      triangle_barycenter (GTS_TRIANGLE (f1), o);
      return point_locate (o, p, f1, surface);
    }
    return NULL;
  }
  
  f = neighbor (f, prev, surface);
  if (f)
    gts_triangle_vertices_edges (GTS_TRIANGLE (f), prev, 
				 &v1, &v2, &v3, &prev, &e2, &e3);
  while (f) {
    gdouble orient = gts_point_orientation (o, GTS_POINT (v3), p);

    if (orient < 0.0) {
      if (gts_point_orientation (GTS_POINT (v2), GTS_POINT (v3), p) >= 0.0)
	return f; /* p is inside f */
      f = neighbor (f, e2, surface);
      prev = e2;
      v1 = v3;      
    }
    else if (orient > 0.0) {
      if (gts_point_orientation (GTS_POINT (v3), GTS_POINT (v1), p) >= 0.0)
	return f; /* p is inside f */
      f = neighbor (f, e3, surface);
      prev = e3;
      v2 = v3;
    }
    else {
      GtsFace * f1;

      if (gts_point_orientation (GTS_POINT (v2), GTS_POINT (v3), p) >= 0.0)
	return f; /* p is inside f */

      /* s intersects f exactly on v3: restarts from a neighbor of f */
      if ((f1 = neighbor (f, e2, surface)) ||
	  (f1 = neighbor (f, e3, surface))) {
	triangle_barycenter (GTS_TRIANGLE (f1), o);
	return point_locate (o, p, f1, surface);
      }
      return NULL;
    }
    /* update e2, e3, v3 for the new triangle */
    if (f) {
      if (prev == GTS_TRIANGLE (f)->e1) {
	e2 = GTS_TRIANGLE (f)->e2; e3 = GTS_TRIANGLE (f)->e3;
      }
      else if (prev == GTS_TRIANGLE (f)->e2) {
	e2 = GTS_TRIANGLE (f)->e3; e3 = GTS_TRIANGLE (f)->e1;
      }
      else {
	e2 = GTS_TRIANGLE (f)->e1; e3 = GTS_TRIANGLE (f)->e2;
      }
      if (GTS_SEGMENT (e2)->v1 == v1 || GTS_SEGMENT (e2)->v1 == v2)
	v3 = GTS_SEGMENT (e2)->v2;
      else
	v3 = GTS_SEGMENT (e2)->v1;
    }
  }
  return NULL;
}

/**
 * gts_point_locate:
 * @p: a #GtsPoint.
 * @surface: a #GtsSurface.
 * @guess: %NULL or a face of @surface close to @p.
 *
 * Locates the face of the planar projection of @surface containing
 * @p. The planar projection of @surface must define a connected set
 * of triangles without holes and bounded by a convex boundary. The
 * algorithm is randomized and performs in O(n^1/3) expected time
 * where n is the number of triangles of @surface.
 *
 * If a good @guess is given the point location can be significantly faster.
 *
 * Returns: a #GtsFace of @surface containing @p or %NULL if @p is not
 * contained within the boundary of @surface.  
 */
GtsFace * gts_point_locate (GtsPoint * p, 
			    GtsSurface * surface,
			    GtsFace * guess)
{
  GtsFace * fr;
  GtsPoint * o;

  g_return_val_if_fail (p != NULL, NULL);
  g_return_val_if_fail (surface != NULL, NULL);
  g_return_val_if_fail (guess == NULL || 
			gts_face_has_parent_surface (guess, surface), NULL);

  if (guess == NULL)
    guess = closest_face (surface, p);
  else
    g_return_val_if_fail (gts_triangle_orientation (GTS_TRIANGLE (guess)) > 0., NULL);

  if (guess == NULL)
    return NULL;

  o = GTS_POINT (gts_object_new (GTS_OBJECT_CLASS (gts_point_class ())));
  triangle_barycenter (GTS_TRIANGLE (guess), o);
  fr = point_locate (o, p, guess, surface);
  gts_object_destroy (GTS_OBJECT (o));

  return fr;
}

struct _GtsConstraint {
  GtsEdge edge;
};

struct _GtsConstraintClass {
  GtsEdgeClass parent_class;
};

/**
 * gts_constraint_class:
 *
 * Returns: the #GtsConstraintClass.
 */
GtsConstraintClass * gts_constraint_class (void)
{
  static GtsConstraintClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo constraint_info = {
      "GtsConstraint",
      sizeof (GtsConstraint),
      sizeof (GtsConstraintClass),
      (GtsObjectClassInitFunc) NULL,
      (GtsObjectInitFunc) NULL,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_edge_class ()), 
				  &constraint_info);
  }

  return klass;
}

static void split_list (GtsListFace * f, GtsListFace * f1, GtsListFace * f2, 
			GtsPoint * p1, GtsPoint * p2,
			GSList ** last1, GSList ** last2)
{
  GSList * i = f->points, * l1 = *last1, * l2 = *last2;

  while (i) {
    GtsPoint * p = i->data;
    
    if (gts_point_orientation (p1, p2, p) >= 0.) {
      if (l1) l1->next = i; else f1->points = i;
      l1 = i;
    }
    else {
      if (l2) l2->next = i; else f2->points = i;
      l2 = i;
    }
    i = i->next;
  }
  f->points = NULL;
  *last1 = l1;
  *last2 = l2;
}

/* cf. figure misc/swap.fig */
static void swap_if_in_circle (GtsFace * f1,
			       GtsVertex * v1, 
			       GtsVertex * v2, 
			       GtsVertex * v3,
			       GtsEdge * e1, 
			       GtsEdge * e2, 
			       GtsEdge * e3,
			       GtsSurface * surface)
{
  GtsFace * f2;
  GtsEdge * e4, *e5;
  GtsVertex * v4;

  if (GTS_IS_CONSTRAINT (e1)) /* @e1 is a constraint can not swap */
    return;

  f2 = neighbor (f1, e1, surface);
  if (f2 == NULL) /* @e1 is a boundary of @surface */
    return;

  if (GTS_TRIANGLE (f2)->e1 == e1) {
    e4 = GTS_TRIANGLE (f2)->e2; e5 = GTS_TRIANGLE (f2)->e3;
  }
  else if (GTS_TRIANGLE (f2)->e2 == e1) {
    e4 = GTS_TRIANGLE (f2)->e3; e5 = GTS_TRIANGLE (f2)->e1;
  }
  else {
    e4 = GTS_TRIANGLE (f2)->e1; e5 = GTS_TRIANGLE (f2)->e2;
  }
  if (GTS_SEGMENT (e4)->v1 == GTS_SEGMENT (e1)->v1 || 
      GTS_SEGMENT (e4)->v1 == GTS_SEGMENT (e1)->v2)
    v4 = GTS_SEGMENT (e4)->v2;
  else
    v4 = GTS_SEGMENT (e4)->v1;

  if (gts_point_in_circle (GTS_POINT (v4), GTS_POINT (v1), 
			   GTS_POINT (v2), GTS_POINT (v3)) > 0.0) {
    GtsEdge * en;
    GtsSegment * sn = gts_vertices_are_connected (v3, v4);
    GtsFace * f3, * f4;

    if (!GTS_IS_EDGE (sn))
      en = gts_edge_new (surface->edge_class, v3, v4);
    else
      en = GTS_EDGE (sn);

    f3 = gts_face_new (surface->face_class, en, e5, e2);
    gts_object_attributes (GTS_OBJECT (f3), GTS_OBJECT (f1));
    f4 = gts_face_new (surface->face_class, en, e3, e4);
    gts_object_attributes (GTS_OBJECT (f4), GTS_OBJECT (f2));
    
    if (GTS_IS_LIST_FACE (f3)) {
      GSList * last3 = NULL, * last4 = NULL;

      if (GTS_IS_LIST_FACE (f1))
	split_list (GTS_LIST_FACE (f1), GTS_LIST_FACE (f3), GTS_LIST_FACE (f4),
		    GTS_POINT (v3), GTS_POINT (v4), &last3, &last4);
      if (GTS_IS_LIST_FACE (f2))
	split_list (GTS_LIST_FACE (f2), GTS_LIST_FACE (f3), GTS_LIST_FACE (f4),
		    GTS_POINT (v3), GTS_POINT (v4), &last3, &last4);
      if (last3) last3->next = NULL;
      if (last4) last4->next = NULL;
    }

    gts_surface_remove_face (surface, f1);
    gts_surface_remove_face (surface, f2);
    gts_surface_add_face (surface, f3);
    gts_surface_add_face (surface, f4);

    swap_if_in_circle (f3, v4, v2, v3, e5, e2, en, surface);
    swap_if_in_circle (f4, v1, v4, v3, e4, en, e3, surface);
  }
}

/**
 * gts_delaunay_add_vertex_to_face:
 * @surface: a #GtsSurface.
 * @v: a #GtsVertex.
 * @f: a #GtsFace belonging to @surface.
 *
 * Adds vertex @v to the face @f of the Delaunay triangulation defined
 * by @surface.
 *
 * Returns: %NULL is @v has been successfully added to @surface or was
 * already contained in @surface or a #GtsVertex having the same x and
 * y coordinates as @v.  
 */
GtsVertex * gts_delaunay_add_vertex_to_face (GtsSurface * surface, 
					     GtsVertex * v,
					     GtsFace * f)
{
  GtsEdge * e1, * e2, * e3;
  GtsSegment * s4, * s5, * s6;
  GtsEdge * e4, * e5, * e6;
  GtsVertex * v1, * v2, * v3;
  GtsFace * nf[3];

  g_return_val_if_fail (surface != NULL, v);
  g_return_val_if_fail (v != NULL, v);
  g_return_val_if_fail (f != NULL, v);

  gts_triangle_vertices_edges (GTS_TRIANGLE (f), NULL, 
			       &v1, &v2, &v3, &e1, &e2, &e3);
  if (v == v1 || v == v2 || v == v3) /* v already in @surface */
    return NULL;
  if (GTS_POINT (v)->x == GTS_POINT (v1)->x &&
      GTS_POINT (v)->y == GTS_POINT (v1)->y)
    return v1;
  if (GTS_POINT (v)->x == GTS_POINT (v2)->x &&
      GTS_POINT (v)->y == GTS_POINT (v2)->y)
    return v2;
  if (GTS_POINT (v)->x == GTS_POINT (v3)->x &&
      GTS_POINT (v)->y == GTS_POINT (v3)->y)
    return v3;

  s4 = gts_vertices_are_connected (v, v1);
  if (!GTS_IS_EDGE (s4))
    e4 = gts_edge_new (surface->edge_class, v, v1);
  else
    e4 = GTS_EDGE (s4);
  s5 = gts_vertices_are_connected (v, v2);
  if (!GTS_IS_EDGE (s5))
    e5 = gts_edge_new (surface->edge_class, v, v2);
  else
    e5 = GTS_EDGE (s5);
  s6 = gts_vertices_are_connected (v, v3);
  if (!GTS_IS_EDGE (s6))
    e6 = gts_edge_new (surface->edge_class, v, v3);
  else
    e6 = GTS_EDGE (s6);

  /* cf. figure misc/swap.fig */
  nf[0] = gts_face_new (surface->face_class, e4, e1, e5);
  gts_object_attributes (GTS_OBJECT (nf[0]), GTS_OBJECT (f));
  nf[1] = gts_face_new (surface->face_class, e5, e2, e6);
  gts_object_attributes (GTS_OBJECT (nf[1]), GTS_OBJECT (f));
  nf[2] = gts_face_new (surface->face_class, e6, e3, e4);
  gts_object_attributes (GTS_OBJECT (nf[2]), GTS_OBJECT (f));

  if (GTS_IS_LIST_FACE (f) && GTS_IS_LIST_FACE (nf[0])) {
    GSList * i = GTS_LIST_FACE (f)->points, * last[3] = { NULL, NULL, NULL };

    while (i) {
      GtsPoint * p = i->data;
      GSList * next = i->next;
      guint j;
      
      if (p != GTS_POINT (v)) {
	if (gts_point_orientation (GTS_POINT (v), GTS_POINT (v1), p) >= 0.) {
	  gdouble o = gts_point_orientation (GTS_POINT (v), GTS_POINT (v2), p);

	  if (o != 0.)
	    j = o > 0. ? 1 : 0;
	  else
	    j = gts_point_orientation (GTS_POINT (v), GTS_POINT (v3), p) 
	      > 0. ? 0 : 1;
	}
	else if (gts_point_orientation (GTS_POINT (v), GTS_POINT (v3), p) > 0.)
	  j = 2;
	else
	  j = 1;
	if (last[j])
	  last[j]->next = i; 
	else 
	  GTS_LIST_FACE (nf[j])->points = i;
	last[j] = i;
      }
      else
	g_slist_free_1 (i);
      i = next;
    }
    GTS_LIST_FACE (f)->points = NULL;
    if (last[0]) last[0]->next = NULL;
    if (last[1]) last[1]->next = NULL;
    if (last[2]) last[2]->next = NULL;
  }

  gts_surface_remove_face (surface, f);
  gts_surface_add_face (surface, nf[0]);
  gts_surface_add_face (surface, nf[1]);
  gts_surface_add_face (surface, nf[2]);

  swap_if_in_circle (nf[0], v1, v2, v, e1, e5, e4, surface);
  swap_if_in_circle (nf[1], v2, v3, v, e2, e6, e5, surface);
  swap_if_in_circle (nf[2], v3, v1, v, e3, e4, e6, surface);

  return NULL;
}

/** 
 * gts_delaunay_add_vertex: 
 * @surface: a #GtsSurface.  
 * @v: a #GtsVertex.  
 * @guess: %NULL or a #GtsFace belonging to @surface to be used as an initial
 * guess for point location.
 *
 * Adds vertex @v to the Delaunay triangulation defined by
 * @surface. If @v is not contained in the convex hull bounding
 * @surface, @v is not added to the triangulation.
 *
 * Returns: %NULL is @v has been successfully added to @surface or was
 * already contained in @surface, @v if @v is not contained in the
 * convex hull bounding surface or a #GtsVertex having the same x and
 * y coordinates as @v.  
 */
GtsVertex * gts_delaunay_add_vertex (GtsSurface * surface, 
				     GtsVertex * v,
				     GtsFace * guess)
{
  GtsFace * f;

  g_return_val_if_fail (surface != NULL, v);
  g_return_val_if_fail (v != NULL, v);

  if (!(f = gts_point_locate (GTS_POINT (v), surface, guess)))
    return v;
  return gts_delaunay_add_vertex_to_face (surface, v, f);
}

static gboolean polygon_in_circle (GSList * poly,
				   GtsPoint * p1, 
				   GtsPoint * p2,
				   GtsPoint * p3)
{
  GtsVertex * v1 = NULL, * v2 = NULL;

  while (poly) {
    GtsSegment * s = poly->data;
    GtsVertex * v;
    v = s->v1;
    if (v != v1 && v != v2 &&
	v != GTS_VERTEX (p1) &&
	v != GTS_VERTEX (p2) &&
	v != GTS_VERTEX (p3) &&
	gts_point_in_circle (GTS_POINT (v), p1, p2, p3) > 0.)
      return TRUE;
    v = s->v2;
    if (v != v1 && v != v2 &&
	v != GTS_VERTEX (p1) &&
	v != GTS_VERTEX (p2) &&
	v != GTS_VERTEX (p3) &&
	gts_point_in_circle (GTS_POINT (v), p1, p2, p3) > 0.)
      return TRUE;
    v1 = s->v1;
    v2 = s->v2;
    poly = poly->next;
  }
  return FALSE;
}

static void triangulate_polygon (GSList * poly, 
				 GtsSurface * surface,
				 GtsFace * ref)
{
  GSList * i, * poly1, * poly2;
  GtsVertex * v1, * v2, * v3 = NULL;
  gboolean found = FALSE;
  GtsSegment * s, * s1, * s2;
  GtsEdge * e1, * e2;
  GtsFace * f;

  if (poly == NULL || poly->next == NULL) {
    g_slist_free (poly);
    return;
  }

  s = poly->data;
  s1 = poly->next->data;
  if (s->v1 == s1->v1 || s->v1 == s1->v2) {
    v1 = s->v2;
    v2 = s->v1;
  }
  else {
    g_assert (s->v2 == s1->v1 || s->v2 == s1->v2);
    v1 = s->v1;
    v2 = s->v2;
  }

  i = poly->next;
  v3 = v2;
  while (i && !found) {
    s1 = i->data;
    if (s1->v1 == v3)
      v3 = s1->v2;
    else {
      g_assert (s1->v2 == v3);
      v3 = s1->v1;
    }
    if (v3 != v1 &&
	gts_point_orientation (GTS_POINT (v1), 
			       GTS_POINT (v2), 
			       GTS_POINT (v3)) >= 0. &&
	!polygon_in_circle (poly, 
			    GTS_POINT (v1), 
			    GTS_POINT (v2), 
			    GTS_POINT (v3)))
      found = TRUE;
    else 
      i = i->next;
  }

  if (!found) {
    g_slist_free (poly);
    return;
  }

  s1 = gts_vertices_are_connected (v2, v3);
  if (!GTS_IS_EDGE (s1))
    e1 = gts_edge_new (surface->edge_class, v2, v3);
  else
    e1 = GTS_EDGE (s1);
  s2 = gts_vertices_are_connected (v3, v1);
  if (!GTS_IS_EDGE (s2))
    e2 = gts_edge_new (surface->edge_class, v3, v1);
  else
    e2 = GTS_EDGE (s2);
  f = gts_face_new (surface->face_class, GTS_EDGE (s), e1, e2);
  gts_object_attributes (GTS_OBJECT (f), GTS_OBJECT (ref));
  gts_surface_add_face (surface, f);

  poly1 = poly->next;
  g_slist_free_1 (poly);
  if (i->next && e2 != i->next->data)
    poly2 = g_slist_prepend (i->next, e2);
  else
    poly2 = i->next;
  if (e1 != i->data)
    i->next = g_slist_prepend (NULL, e1);
  else
    i->next = NULL;

 triangulate_polygon (poly1, surface, ref);
 triangulate_polygon (poly2, surface, ref);
}

/**
 * gts_delaunay_remove_vertex:
 * @surface: a #GtsSurface.
 * @v: a #GtsVertex.
 *
 * Removes @v from the Delaunay triangulation defined by @surface and
 * restores the Delaunay property. Vertex @v must not be used by any
 * constrained edge otherwise the triangulation is not guaranteed to
 * be Delaunay.  
 */
void gts_delaunay_remove_vertex (GtsSurface * surface, GtsVertex * v)
{
  GSList * triangles, * i;
  GtsFace * ref = NULL;

  g_return_if_fail (surface != NULL);
  g_return_if_fail (v != NULL);

  i = triangles = gts_vertex_triangles (v, NULL);
  while (i && !ref) {
    if (GTS_IS_FACE (i->data) &&
	gts_face_has_parent_surface (i->data, surface))
      ref = i->data;
    i = i->next;
  }
  if (!ref) {
    g_slist_free (triangles);
    g_return_if_fail (ref);
  }
  triangulate_polygon (gts_vertex_fan_oriented (v, surface), surface, ref);
  i = triangles;
  while (i) {
    if (GTS_IS_FACE (i->data) &&
	gts_face_has_parent_surface (i->data, surface))
      gts_surface_remove_face (surface, i->data);
    i = i->next;
  }
  g_slist_free (triangles);
}

#define NEXT_CUT(edge, edge1, list) { next = neighbor (f, edge, surface);\
                                      remove_triangles (e, surface);\
                                      if (!constraint && !e->triangles)\
				        gts_object_destroy (GTS_OBJECT (e));\
                                      g_assert (next);\
				      *list = g_slist_prepend (*list, edge1);\
                                      return g_slist_concat (constraint,\
                                        remove_intersected_edge (s, edge,\
					       next, surface, left, right));\
                                    }

static void remove_triangles (GtsEdge * e, GtsSurface * s)
{
  GSList * i = e->triangles;

  while (i) {
    GSList * next = i->next;

    if (GTS_IS_FACE (i->data) && gts_face_has_parent_surface (i->data, s))
      gts_surface_remove_face (s, i->data);
    i = next;
  }
}

static GSList * 
remove_intersected_edge (GtsSegment * s,
			 GtsEdge * e,
			 GtsFace * f,
			 GtsSurface * surface,
			 GSList ** left, GSList ** right)
{
  GtsVertex * v1, * v2, * v3;
  GtsEdge * e1, * e2;
  gdouble o1, o2;
  GtsFace * next;
  GSList * constraint = NULL;

  if (GTS_IS_CONSTRAINT (e))
    constraint = g_slist_prepend (NULL, e);

  gts_triangle_vertices_edges (GTS_TRIANGLE (f), e, 
			       &v1, &v2, &v3, &e, &e1, &e2);
  
  o1 = gts_point_orientation (GTS_POINT (v2), GTS_POINT (v3), 
			      GTS_POINT (s->v2));
  o2 = gts_point_orientation (GTS_POINT (v3), GTS_POINT (v1), 
			      GTS_POINT (s->v2));

  if (o1 == 0.) {
    g_assert (o2 == 0.);
    remove_triangles (e, surface);
    if (!constraint && !e->triangles)
      gts_object_destroy (GTS_OBJECT (e));
    *left = g_slist_prepend (*left, e2);
    *right = g_slist_prepend (*right, e1);
  }
  else if (o1 > 0.) {
    g_assert (o2 <= 0.);
    NEXT_CUT (e2, e1, right)
  }
  else if (o2 >= 0.)
    NEXT_CUT (e1, e2, left)
  else {
    gdouble o3 = gts_point_orientation (GTS_POINT (s->v1), GTS_POINT (s->v2),
					GTS_POINT (v3));
    if (o3 > 0.)
      NEXT_CUT (e1, e2, left)
    else
      NEXT_CUT (e2, e1, right)
  }
  return constraint;
}

static GSList * 
remove_intersected_vertex (GtsSegment * s,
			   GtsVertex * v,
			   GtsSurface * surface,
			   GSList ** left,
			   GSList ** right,
			   GtsFace ** ref)
{
  GSList * triangles = gts_vertex_triangles (v, NULL);
  GSList * i;

  i = triangles;
  while (i) {
    GtsTriangle * t = i->data;
    if (GTS_IS_FACE (t) && 
	gts_face_has_parent_surface (GTS_FACE (t), surface)) {
      GtsVertex * v1, * v2, * v3;
      gdouble o1, o2;

      gts_triangle_vertices (t, &v1, &v2, &v3);
      if (v == v2) {
	v2 = v3;
	v3 = v1;
      }
      else if (v == v3) {
	v3 = v2;
	v2 = v1;	
      }
      else
	g_assert (v == v1);

      if ((o1 = gts_point_orientation (GTS_POINT (v), GTS_POINT (v2),
				       GTS_POINT (s->v2))) >= 0. &&
	  (o2 = gts_point_orientation (GTS_POINT (v3), GTS_POINT (v),
				       GTS_POINT (s->v2))) >= 0.) {
	gdouble o3 = gts_point_orientation (GTS_POINT (v2), GTS_POINT (v3),
					    GTS_POINT (s->v2));
	GtsEdge * e = gts_triangle_edge_opposite (t, v);
	GtsEdge * e1, * e2;
	GtsFace * next = neighbor (GTS_FACE (t), e, surface);

	*ref = GTS_FACE (t);
	gts_triangle_vertices_edges (t, e, &v2, &v3, &v, &e, &e2, &e1);

	g_slist_free (triangles);

	if (o3 >= 0.) /* @s->v2 is inside (or on the edge) of t */
	  return NULL;

	gts_allow_floating_faces = TRUE;
	gts_surface_remove_face (surface, GTS_FACE (t));
	gts_allow_floating_faces = FALSE;

	*left = g_slist_prepend (*left, e2);
	*right = g_slist_prepend (*right, e1);

	g_assert (next);
	return remove_intersected_edge (s, e, next, surface, left, right);
      }
    }
    i = i->next;
  }

  g_assert_not_reached ();
  return NULL;
}

/**
 * gts_delaunay_add_constraint:
 * @surface: a #GtsSurface.
 * @c: a #GtsConstraint.
 *
 * Add constraint @c to the constrained Delaunay triangulation defined by
 * @surface.
 *
 * Returns: a list of #GtsConstraint conflicting (i.e. intersecting) with @c 
 * which were removed from @surface (%NULL if there was none).
 */
GSList * gts_delaunay_add_constraint (GtsSurface * surface,
				      GtsConstraint * c)
{
  GSList * constraints;
  GtsVertex * v1, * v2;
  GSList * left = NULL, * right = NULL;
  GtsFace * ref = NULL;

  g_return_val_if_fail (surface != NULL, NULL);
  g_return_val_if_fail (c != NULL, NULL);
  g_return_val_if_fail (GTS_IS_CONSTRAINT (c), NULL);
  
  v1 = GTS_SEGMENT (c)->v1;
  v2 = GTS_SEGMENT (c)->v2;
  
  gts_allow_floating_edges = TRUE;
  constraints = remove_intersected_vertex (GTS_SEGMENT (c), v1, surface,
					   &left, &right, &ref);
  gts_allow_floating_edges = FALSE;
#if 1
  triangulate_polygon (g_slist_prepend (g_slist_reverse (right), c), 
		       surface, ref);
  triangulate_polygon (g_slist_prepend (left, c), 
		       surface, ref);
#else
  right = g_slist_prepend (g_slist_reverse (right), c);
  left = g_slist_prepend (left, c);
  {
    FILE * fp0 = fopen ("hole", "wt");
    FILE * fp1 = fopen ("right", "wt");
    FILE * fp2 = fopen ("left", "wt");
    GSList * i = left;

    gts_surface_write (surface, fp0);
    fclose (fp0);
 
    fprintf (fp2, "LIST {\n");
    while (i) {
      GtsSegment * s = i->data;
      fprintf (fp2, 
	       "# %p: %p->%p\n"
	       "VECT 1 2 0 2 0 %g %g 0 %g %g 0\n",
	       s, s->v1, s->v2,
	       GTS_POINT (s->v1)->x, GTS_POINT (s->v1)->y,
	       GTS_POINT (s->v2)->x, GTS_POINT (s->v2)->y);
      i = i->next;
    }
    fprintf (fp2, "}\n");
    fprintf (fp1, "LIST {\n");
    i = right;
    while (i) {
      GtsSegment * s = i->data;
      fprintf (fp1, 
	       "# %p: %p->%p\n"
	       "VECT 1 2 0 2 0 %g %g 0 %g %g 0\n",
	       s, s->v1, s->v2,
	       GTS_POINT (s->v1)->x, GTS_POINT (s->v1)->y,
	       GTS_POINT (s->v2)->x, GTS_POINT (s->v2)->y);
      i = i->next;
    }
    fprintf (fp1, "}\n");
    fclose (fp1);
    fclose (fp2);
  }
  triangulate_polygon (right, surface);
  triangulate_polygon (left, surface);
#endif
  if (ref && !ref->surfaces) {
    gts_allow_floating_edges = TRUE;
    gts_object_destroy (GTS_OBJECT (ref));
    gts_allow_floating_edges = FALSE;
  }
  return constraints;
}

static void delaunay_check (GtsTriangle * t, gpointer * data)
{
  GtsSurface * surface = data[0];
  GtsFace ** face = data[1];

  if (*face == NULL) {
    GSList * i, * list;
    GtsVertex * v1, * v2, * v3;

    gts_triangle_vertices (t, &v1, &v2, &v3);
    list = gts_vertex_neighbors (v1, NULL, surface);
    list = gts_vertex_neighbors (v2, list, surface);
    list = gts_vertex_neighbors (v3, list, surface);
    i = list;
    while (i && *face == NULL) {
      GtsVertex * v = i->data;
      if (v != v1 && v != v2 && v != v3 &&
	  gts_point_in_circle (GTS_POINT (v), 
			       GTS_POINT (v1),
			       GTS_POINT (v2),  
			       GTS_POINT (v3)) > 0.)
	*face = GTS_FACE (t);
      i = i->next;
    }
    g_slist_free (list);
  }
}

/**
 * gts_delaunay_check:
 * @surface: a #GtsSurface.
 *
 * Returns: %NULL if the planar projection of @surface is a Delaunay 
 * triangulation (unconstrained), a #GtsFace violating the Delaunay
 * property otherwise.
 */
GtsFace * gts_delaunay_check (GtsSurface * surface)
{
  GtsFace * face = NULL;
  gpointer data[2];

  g_return_val_if_fail (surface != NULL, FALSE);

  data[0] = surface;
  data[1] = &face;
  gts_surface_foreach_face (surface, (GtsFunc) delaunay_check, data);

  return face;
}

/**
 * gts_delaunay_remove_hull:
 * @surface: a #GtsSurface.
 *
 * Removes all the edges of the boundary of @surface which are not
 * constraints.  
 */
void gts_delaunay_remove_hull (GtsSurface * surface)
{
  GSList * boundary;

  g_return_if_fail (surface != NULL);

  boundary = gts_surface_boundary (surface);
  gts_allow_floating_edges = TRUE;
  while (boundary) {
    GSList * i = boundary;
    GtsEdge * e = i->data;

    boundary = i->next;
    g_slist_free_1 (i);
    if (!GTS_IS_CONSTRAINT (e)) {
      GtsTriangle * t = GTS_TRIANGLE (gts_edge_is_boundary (e, surface));

      if (t != NULL) {
	if (t->e1 != e && !GTS_IS_CONSTRAINT (t->e1) &&
	    !gts_edge_is_boundary (t->e1, surface))
	  boundary = g_slist_prepend (boundary, t->e1);
	if (t->e2 != e && !GTS_IS_CONSTRAINT (t->e2) &&
	    !gts_edge_is_boundary (t->e2, surface))
	  boundary = g_slist_prepend (boundary, t->e2);
	if (t->e3 != e && !GTS_IS_CONSTRAINT (t->e3) &&
	    !gts_edge_is_boundary (t->e3, surface))
	  boundary = g_slist_prepend (boundary, t->e3);
	gts_surface_remove_face (surface, GTS_FACE (t));
      }
      if (!e->triangles)
	gts_object_destroy (GTS_OBJECT (e));
    }
  }
  gts_allow_floating_edges = FALSE;
}

/* GtsListFace: Object */

static void gts_list_face_destroy (GtsObject * object)
{
  g_slist_free (GTS_LIST_FACE (object)->points);

  (* GTS_OBJECT_CLASS (gts_list_face_class ())->parent_class->destroy) 
    (object);
}

static void gts_list_face_class_init (GtsFaceClass * klass)
{
  GTS_OBJECT_CLASS (klass)->destroy = gts_list_face_destroy;
}

GtsFaceClass * gts_list_face_class (void)
{
  static GtsFaceClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo gts_list_face_info = {
      "GtsListFace",
      sizeof (GtsListFace),
      sizeof (GtsFaceClass),
      (GtsObjectClassInitFunc) gts_list_face_class_init,
      (GtsObjectInitFunc) NULL,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_face_class ()),
				  &gts_list_face_info);
  }

  return klass;
}
