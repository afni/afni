/* GTS - Library for the manipulation of triangulated surfaces
 * Copyright (C) 1999--2002 Stéphane Popinet
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

/*#define DEBUG*/
/*#define DEBUG_BOOLEAN*/
/*#define CHECK_ORIENTED*/

#ifdef DEBUG
#  include "gts-private.h"
#endif /* DEBUG */

static void surface_inter_destroy (GtsObject * object)
{
  GtsSurfaceInter * si = GTS_SURFACE_INTER (object);

  gts_object_destroy (GTS_OBJECT (si->s1));
  gts_object_destroy (GTS_OBJECT (si->s2));
  g_slist_free (si->edges);

  (* GTS_OBJECT_CLASS (gts_surface_inter_class ())->parent_class->destroy)
    (object);
}

static void surface_inter_class_init (GtsObjectClass * klass)
{
  klass->destroy = surface_inter_destroy;
}

static void surface_inter_init (GtsSurfaceInter * si)
{
  si->s1 = si->s2 = NULL;
  si->edges = NULL;
}

/**
 * gts_surface_inter_class:
 *
 * Returns: the #GtsSurfaceInterClass.
 */
GtsSurfaceInterClass * gts_surface_inter_class (void)
{
  static GtsSurfaceInterClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo surface_inter_info = {
      "GtsSurfaceInter",
      sizeof (GtsSurfaceInter),
      sizeof (GtsSurfaceInterClass),
      (GtsObjectClassInitFunc) surface_inter_class_init,
      (GtsObjectInitFunc) surface_inter_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (), &surface_inter_info);
  }

  return klass;
}

/* EdgeInter: Header */

typedef struct _EdgeInter         EdgeInter;

struct _EdgeInter {
  GtsEdge parent;

  GtsTriangle * t1, * t2;
};

#define EDGE_INTER(obj)            GTS_OBJECT_CAST (obj,\
					         EdgeInter,\
					         edge_inter_class ())
#define IS_EDGE_INTER(obj)         (gts_object_is_from_class (obj,\
						 edge_inter_class ()))

static GtsEdgeClass * edge_inter_class  (void);
static EdgeInter * edge_inter_new    (GtsVertex * v1, GtsVertex * v2,
				      GtsTriangle * t1, GtsTriangle * t2);

/* EdgeInter: Object */

static GtsEdgeClass * edge_inter_class (void)
{
  static GtsEdgeClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo edge_inter_info = {
      "EdgeInter",
      sizeof (EdgeInter),
      sizeof (GtsEdgeClass),
      (GtsObjectClassInitFunc) NULL,
      (GtsObjectInitFunc) NULL,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_constraint_class ()),
				  &edge_inter_info);
  }

  return klass;
}

static EdgeInter * edge_inter_new (GtsVertex * v1, GtsVertex * v2,
				   GtsTriangle * t1, GtsTriangle * t2)
{
  EdgeInter * object;

  object = EDGE_INTER (gts_edge_new (GTS_EDGE_CLASS (edge_inter_class ()), 
				     v1, v2));
  object->t1 = t1;
  object->t2 = t2;

  return object;
}

#ifdef DEBUG
static void write_surface_graph (GtsSurface * s, FILE * fp)
{
  GSList * l = NULL;
  GtsGraph * g;
  static void add_to_list (gpointer data, GSList ** l) {
    *l = g_slist_prepend (*l, data);
  }

  gts_surface_foreach_vertex (s, (GtsFunc) gts_object_reset_reserved, NULL);
  gts_surface_foreach_edge (s, (GtsFunc) gts_object_reset_reserved, NULL);
  gts_surface_foreach_edge (s, (GtsFunc) add_to_list, &l);
  g = gts_segments_graph_new (gts_graph_class (), l);
  gts_graph_write_dot (g, fp);
  gts_object_destroy (GTS_OBJECT (g));
  g_slist_free (l);
}
#endif /* DEBUG */

static GtsPoint * segment_triangle_intersection (GtsSegment * s,
						 GtsTriangle * t,
						 GtsPointClass * klass)
{
  GtsPoint * A, * B, * C, * D, * E;
  gint ABCE, ABCD, ADCE, ABDE, BCDE;
  GtsEdge * AB, * BC, * CA;
  gdouble a, b, c;

  g_return_val_if_fail (s != NULL, NULL);
  g_return_val_if_fail (t != NULL, NULL);
  g_return_val_if_fail (klass != NULL, NULL);

  gts_triangle_vertices_edges (t, NULL, 
			       (GtsVertex **) &A, 
			       (GtsVertex **) &B, 
			       (GtsVertex **) &C, 
			       &AB, &BC, &CA);
  D = GTS_POINT (s->v1);
  E = GTS_POINT (s->v2);

  ABCE = gts_point_orientation_3d_sos (A, B, C, E);
  ABCD = gts_point_orientation_3d_sos (A, B, C, D);
  if (ABCE < 0 || ABCD > 0) {
    GtsPoint * tmpp;
    gint tmp;

    tmpp = E; E = D; D = tmpp;
    tmp = ABCE; ABCE = ABCD; ABCD = tmp;
  }
  if (ABCE < 0 || ABCD > 0)
    return NULL;
  ADCE = gts_point_orientation_3d_sos (A, D, C, E);
  if (ADCE < 0)
    return NULL;
  ABDE = gts_point_orientation_3d_sos (A, B, D, E);
  if (ABDE < 0)
    return NULL;
  BCDE = gts_point_orientation_3d_sos (B, C, D, E);
  if (BCDE < 0)
    return NULL;
  a = gts_point_orientation_3d (A, B, C, E);
  b = gts_point_orientation_3d (A, B, C, D);
  if (a != b) {
    c = a/(a - b);
    return gts_point_new (klass,
			  E->x + c*(D->x - E->x),
			  E->y + c*(D->y - E->y),
			  E->z + c*(D->z - E->z));
  }
  /* D and E are contained within ABC */
#ifdef DEBUG
  fprintf (stderr, 
	   "segment: %p:%s triangle: %p:%s intersection\n"
	   "D and E contained in ABC\n",
	   s, GTS_NEDGE (s)->name, t, GTS_NFACE (t)->name);
#endif /* DEBUG */  
  g_assert (a == 0.); 
  return gts_point_new (klass,
			(E->x + D->x)/2.,
			(E->y + D->y)/2.,
			(E->z + D->z)/2.);
}

static gint triangle_triangle_orientation (GtsPoint * p1, 
					   GtsPoint * p2, GtsPoint * p3,
					   GtsPoint * p4, GtsPoint * p5,
					   GtsPoint * p6)
{
  gint o4 = 0, o5 = 0, o6 = 0;

  if (p4 != p1 && p4 != p2 && p4 != p3)
    o4 = gts_point_orientation_3d_sos (p1, p2, p3, p4);
  if (p5 != p1 && p5 != p2 && p5 != p3)
    o5 = gts_point_orientation_3d_sos (p1, p2, p3, p5);
  if (o4*o5 < 0)
    return 0;
  if (p6 != p1 && p6 != p2 && p6 != p3)
    o6 = gts_point_orientation_3d_sos (p1, p2, p3, p6);
  if (o4*o6 < 0 || o5*o6 < 0)
    return 0;
  if (o4) return o4;
  if (o5) return o5;
  g_assert (o6);
  return o6;
}

static gint triangle_point_orientation (GtsTriangle * t1, 
					GtsTriangle * t2,
					gint o1,
					GtsPoint * p)
{
  GtsPoint * p1 = GTS_POINT (GTS_SEGMENT (t1->e1)->v1);
  GtsPoint * p2 = GTS_POINT (GTS_SEGMENT (t1->e1)->v2);
  GtsPoint * p3 = GTS_POINT (gts_triangle_vertex (t1));
  GtsPoint * p4 = GTS_POINT (GTS_SEGMENT (t2->e1)->v1);
  GtsPoint * p5 = GTS_POINT (GTS_SEGMENT (t2->e1)->v2);
  GtsPoint * p6 = GTS_POINT (gts_triangle_vertex (t2));
  gint o = triangle_triangle_orientation (p1, p2, p3, p4, p5, p6);

  if (o != 0)
    return o;
  o = triangle_triangle_orientation (p4, p5, p6, p1, p2, p3);
  if (o != 0) {
    gint o2 = gts_point_orientation_3d_sos (p4, p5, p6, p);

    return - o*o1*o2;
  }
  return 0;
}

static void add_edge_inter (GtsEdge * e,
			    GtsTriangle * t,
			    GtsVertex * v)
{
  GtsVertex * ev1 = GTS_SEGMENT (e)->v1, * ev2 = GTS_SEGMENT (e)->v2;
  GList * i = GTS_OBJECT (e)->reserved;

  GTS_OBJECT (v)->reserved = t;
  if (i == NULL) {
    GTS_OBJECT (e)->reserved = g_list_prepend (NULL, v);
#ifdef DEBUG
    fprintf (stderr, "add_edge_inter: inserting %p (%p,%p)\n", v, e, t);
#endif /* DEBUG */
  }
  else {
    GtsPoint * p1 = GTS_POINT (GTS_SEGMENT (t->e1)->v1);
    GtsPoint * p2 = GTS_POINT (GTS_SEGMENT (t->e1)->v2);
    GtsPoint * p3 = GTS_POINT (gts_triangle_vertex (t));
    gint o1, oref = gts_point_orientation_3d_sos (p1, p2, p3, GTS_POINT (ev1));
    
    o1 = oref;
    while (i) {
      gint o2 = triangle_point_orientation (t, GTS_OBJECT (i->data)->reserved,
					    oref, GTS_POINT (ev1));

      if (o2 == 0) {
#ifdef DEBUG
	g_warning ("add_edge_inter: safe sign evaluation failed\n");
#endif /* DEBUG */
	o2 = gts_point_orientation_3d_sos (p1, p2, p3, i->data);
      }

      if (o1*o2 < 0)
	break;
      ev1 = i->data;
      o1 = o2;
      i = i->next;
    }
    if (i != NULL) {
      GList * n = g_list_prepend (NULL, v);

      ev2 = i->data;
      n->next = i;
      n->prev = i->prev;
      i->prev = n;
      if (n->prev == NULL)
	GTS_OBJECT (e)->reserved = n;
      else
	n->prev->next = n;
    }
    else {
      g_assert (o1*gts_point_orientation_3d_sos (p1, p2, p3, GTS_POINT (ev2))
		< 0);
      GTS_OBJECT (e)->reserved = g_list_append (GTS_OBJECT (e)->reserved, v);
    }
#ifdef DEBUG
    fprintf (stderr, 
	     "add_edge_inter: inserting %p (%p,%p) between %p and %p\n", 
	     v, e, t, ev1, ev2);
    i = GTS_OBJECT (e)->reserved;
    while (i) {
      fprintf (stderr, " %p", i->data);
      i = i->next;
    }
    fprintf (stderr, "\n");
#endif /* DEBUG */
  }
}

static GtsVertex * intersects (GtsEdge * e,
			       GtsTriangle * t,
			       GtsSurface * s)
{
  GList * i = GTS_OBJECT (e)->reserved;
  GtsVertex * v;

  while (i) {
    if (GTS_OBJECT (i->data)->reserved == t)
      return i->data;
    i = i->next;
  }

  v = GTS_VERTEX (segment_triangle_intersection (GTS_SEGMENT (e), t, 
				    GTS_POINT_CLASS (s->vertex_class)));
  if (v != NULL) {
#ifdef DEBUG
    if (GTS_IS_NVERTEX (v) && GTS_IS_NEDGE (e) && GTS_IS_NFACE (t) &&
	GTS_NVERTEX (v)->name[0] == '\0')
      g_snprintf (GTS_NVERTEX (v)->name, GTS_NAME_LENGTH, "%s|%s",
		  GTS_NEDGE (e)->name, GTS_NFACE (t)->name);
#endif /* DEBUG */
    if (s->vertex_class->intersection_attributes)
      (*s->vertex_class->intersection_attributes)
	(v, GTS_OBJECT (e), GTS_OBJECT (t));
    add_edge_inter (e, t, v);
  }
  return v;
}

/* see figure misc/orientation.fig */
static gint intersection_orientation (GtsTriangle * t1, 
				      GtsEdge * e,
				      GtsTriangle * t2)
{
  GtsVertex * v1, * v2, * v3;
  GtsEdge * e2, * e3;
  GtsVertex * v4, * v5, * v6;

  gts_triangle_vertices_edges (t1, e, &v1, &v2, &v3, &e, &e2, &e3);
  gts_triangle_vertices (t2, &v4, &v5, &v6);

  return gts_point_orientation_3d_sos (GTS_POINT (v4), 
				       GTS_POINT (v5), 
				       GTS_POINT (v6),
				       GTS_POINT (v2));
}

#define UPDATE_ORIENTATION if (o > 0) { vi2 = v; e2 = e; } else { vi2 = vi1;\
                                                                  e2 = e1;\
                                                                  vi1 = v;\
                                                                  e1 = e; }

static void intersect_edges (GtsBBox * bb1, GtsBBox * bb2,
			     GtsSurfaceInter * si)
{
  GtsSurface * s1 = GTS_OBJECT (si->s1)->reserved;
  GtsTriangle * t1 = GTS_TRIANGLE (bb1->bounded);
  GtsTriangle * t2 = GTS_TRIANGLE (bb2->bounded);
  GtsVertex * v, * vi1 = NULL, * vi2 = NULL;
  GtsEdge * e1 = NULL, * e2 = NULL, * e;

  vi1 = intersects (t2->e1, t1, s1);
  e1 = t2->e1;
  v = intersects (t2->e2, t1, s1);
  e = t2->e2;
  if (!vi1) {
    vi1 = v;
    e1 = e;
  }
  else if (v) {
    gint o = intersection_orientation (t2, t2->e2, t1);
    UPDATE_ORIENTATION;
  }
  if (!vi2) {
    v = intersects (t2->e3, t1, s1);
    e = t2->e3;
    if (!vi1) {
      vi1 = v;
      e1 = e;
    }
    else if (v) {
      gint o = intersection_orientation (t2, t2->e3, t1);
      UPDATE_ORIENTATION;
    }
  }
  if (!vi2) {
    v = intersects (t1->e1, t2, s1);
    e = t1->e1;
    if (!vi1) {
      vi1 = v;
      e1 = e;
    }
    else if (v) {
      gint o = - intersection_orientation (t1, t1->e1, t2);
      UPDATE_ORIENTATION;
    }
  }
  if (!vi2) {
    v = intersects (t1->e2, t2, s1);
    e = t1->e2;
    if (!vi1) {
      vi1 = v;
      e1 = e;
    }
    else if (v) {
      gint o = - intersection_orientation (t1, t1->e2, t2);
      UPDATE_ORIENTATION;
    }
  }
  if (!vi2) {
    v = intersects (t1->e3, t2, s1);
    e = t1->e3;
    if (!vi1) {
      vi1 = v;
      e1 = e;
    }
    else if (v) {
      gint o = - intersection_orientation (t1, t1->e3, t2);
      UPDATE_ORIENTATION;
    }
  }

  g_assert ((!vi1 && !vi2) || (vi1 && vi2));
  if (vi1) {
    GtsEdge * e = GTS_EDGE (edge_inter_new (vi1, vi2, t1, t2));

#ifdef DEBUG
    fprintf (stderr, "creating constraint %p: %p->%p: %p/%p\n", 
	     e, vi1, vi2, t1, t2);
#endif /* DEBUG */
    gts_surface_add_face (si->s1, GTS_FACE (t1));
    gts_surface_add_face (si->s2, GTS_FACE (t2));
    si->edges = g_slist_prepend (si->edges, e);
    GTS_OBJECT (t1)->reserved = g_slist_prepend (GTS_OBJECT (t1)->reserved, e);
    GTS_OBJECT (t2)->reserved = g_slist_prepend (GTS_OBJECT (t2)->reserved, e);
  }
}

static GtsSurfaceInter * surface_inter_new (GtsSurfaceInterClass * klass,
					    GtsSurface * s1,
					    GtsSurface * s2,
					    GNode * faces_tree1,
					    GNode * faces_tree2)
{
  GtsSurfaceInter * si;

  si = GTS_SURFACE_INTER (gts_object_new (GTS_OBJECT_CLASS (klass)));
  si->s1 = gts_surface_new (gts_surface_class (),
			    s1->face_class,
			    s1->edge_class,
			    s1->vertex_class);
  GTS_OBJECT (si->s1)->reserved = s1;
  si->s2 = gts_surface_new (gts_surface_class (),
			    s2->face_class,
			    s2->edge_class,
			    s2->vertex_class);
  GTS_OBJECT (si->s2)->reserved = s2;
  gts_bb_tree_traverse_overlapping (faces_tree1, faces_tree2,
				    (GtsBBTreeTraverseFunc) intersect_edges, 
				    si);

  return si;
}

static void free_slist (GtsObject * o)
{
  g_slist_free (o->reserved);
  o->reserved = NULL;
}

static void free_glist (GtsObject * o)
{
  g_list_foreach (o->reserved, (GFunc) gts_object_reset_reserved, NULL);
  g_list_free (o->reserved);
  o->reserved = NULL;
}

/**
 * gts_surface_intersection:
 * @s1: a #GtsSurface.
 * @s2: a #GtsSurface.
 * @faces_tree1: a bounding box tree (see gts_bb_tree_new()) for
 * the faces of @s1.
 * @faces_tree2: a bounding box tree for the faces of @s2.
 *
 * Returns: a list of #GtsEdge defining the curve intersection of the
 * two surfaces.
 */
GSList * gts_surface_intersection (GtsSurface * s1,
				   GtsSurface * s2,
				   GNode * faces_tree1,
				   GNode * faces_tree2)
{
  GtsSurfaceInter * si;
  GSList * inter;

  g_return_val_if_fail (s1 != NULL, NULL);
  g_return_val_if_fail (s2 != NULL, NULL);
  g_return_val_if_fail (faces_tree1 != NULL, NULL);
  g_return_val_if_fail (faces_tree2 != NULL, NULL);

  si = surface_inter_new (gts_surface_inter_class (),
			  s1, s2, faces_tree1, faces_tree2);

  gts_surface_foreach_face (si->s1, (GtsFunc) free_slist, NULL);
  gts_surface_foreach_face (si->s2, (GtsFunc) free_slist, NULL);
  gts_surface_foreach_edge (si->s1, (GtsFunc) free_glist, NULL);
  gts_surface_foreach_edge (si->s2, (GtsFunc) free_glist, NULL);
  inter = si->edges;
  si->edges = NULL;
  gts_object_destroy (GTS_OBJECT (si));

  return inter;  
}

typedef enum {
  INTERIOR = 1 << (GTS_USER_FLAG),
  RELEVANT = 1 << (GTS_USER_FLAG + 1)
} CurveFlag;

#define IS_SET(s, f) ((GTS_OBJECT_FLAGS (s) & (f)) != 0)
#define SET(s, f)   (GTS_OBJECT_FLAGS (s) |= (f))
#define UNSET(s, f) (GTS_OBJECT_FLAGS (s) &= ~(f))
#define NEXT(s)  (GTS_OBJECT (s)->reserved)

#ifdef DEBUG
static void print_segment (GtsSegment * s)
{
  fprintf (stderr, "%p: %s->%s ", s,
	   GTS_NVERTEX (s->v1)->name,
	   GTS_NVERTEX (s->v2)->name);
  if (NEXT (s)) {
    GtsSegment * next = NEXT (s);

    fprintf (stderr, "next %p: %s->%s\n", next,
	     GTS_NVERTEX (next->v1)->name,
	     GTS_NVERTEX (next->v2)->name);
  }
  else
    fprintf (stderr, "next NULL\n");
}

static void write_nodes (GSList * i, GHashTable * hash, guint * nn,
			 FILE * fp)
{
  while (i) {
    GtsSegment * s = i->data;

    if (!g_hash_table_lookup (hash, s->v1)) {
      fprintf (fp, "  %u [ label = \"%p\" ];\n", *nn, s->v1);
      g_hash_table_insert (hash, s->v1, GUINT_TO_POINTER ((*nn)++));
    }
    if (!g_hash_table_lookup (hash, s->v2)) {
      fprintf (fp, "  %u [ label = \"%p\" ];\n", *nn, s->v2);
      g_hash_table_insert (hash, s->v2, GUINT_TO_POINTER ((*nn)++));
    }
    i = i->next;
  }
}

static void write_edges (GSList * i, GHashTable * hash, 
			 GtsSurface * surface,
			 FILE * fp)
{
  while (i) {
    GtsSegment * s = i->data;

    fprintf (fp, "  %u -> %u [ label = \"%p:%d\" ];\n",
	     GPOINTER_TO_UINT (g_hash_table_lookup (hash, s->v1)),
	     GPOINTER_TO_UINT (g_hash_table_lookup (hash, s->v2)),
	     s,
	     gts_edge_face_number (GTS_EDGE (s), surface));
    i = i->next;
  }
}

static void write_graph (GSList * boundary, GSList * interior,
			 GtsSurface * surface,
			 FILE * fp)
{
  GHashTable * hash = g_hash_table_new (NULL, NULL);
  guint nn = 1;
  
  fprintf (fp, "digraph oriented_curve {\n");
  write_nodes (boundary, hash, &nn, fp);
  write_nodes (interior, hash, &nn, fp);
  write_edges (boundary, hash, surface, fp);
  fprintf (fp, "  edge [ color = red ];\n");
  write_edges (interior, hash, surface, fp);
  fprintf (fp, "}\n");
  g_hash_table_destroy (hash);
}

static void write_graph1 (GtsSegment * start, GSList * i,
			  GtsSurface * surface,
			  FILE * fp)
{
  GSList * boundary = NULL, * interior = NULL;
  GtsSegment * s = start;

  do {
    boundary = g_slist_prepend (boundary, s);
    s = NEXT (s);
  } while (s != start);
  while (i) {
    if (IS_SET (i->data, INTERIOR))
      interior = g_slist_prepend (interior, i->data);
    i = i->next;
  }
  write_graph (boundary, interior, surface, fp);
  g_slist_free (boundary);
  g_slist_free (interior);
}

static void print_loop (GtsSegment * start, FILE * fp)
{
  GtsSegment * s = start;

  do {
    fprintf (fp, "  %p: %p:%s -> %p:%s\n",
	     s, 
	     s->v1, GTS_NVERTEX (s->v1)->name, 
	     s->v2, GTS_NVERTEX (s->v2)->name);
    s = NEXT (s);
  } while (s != start && s != NULL);
}

static void draw_vector (GtsPoint * p1, GtsPoint * p2, FILE * fp)
{
  gdouble x = p2->x - p1->x;
  gdouble y = p2->y - p1->y;
  gdouble z = p2->z - p1->z;

  fprintf (fp, "VECT 1 3 0 3 0 %g %g %g %g %g %g %g %g %g\n",
	   p1->x + x - (x - y/2.)/5.,
	   p1->y + y - (x/2. + y)/5.,
	   p1->z + z - (x/2. + z)/5.,
	   p1->x + x,
	   p1->y + y,
	   p1->z + z,
	   p1->x + x - (x + y/2.)/5.,
	   p1->y + y + (x/2. - y)/5.,
	   p1->z + z + (x/2. - z)/5.);
  fprintf (fp, "VECT 1 2 0 2 0 %g %g %g %g %g %g\n",
	   p1->x, p1->y, p1->z,
	   p1->x + x,
	   p1->y + y,
	   p1->z + z);
}

static void draw_vector1 (GtsPoint * p1, GtsPoint * p2, GtsPoint * o,
			  FILE * fp)
{
  gdouble x1 = o->x + 0.9*(p1->x - o->x);
  gdouble y1 = o->y + 0.9*(p1->y - o->y);
  gdouble z1 = o->z + 0.9*(p1->z - o->z);
  gdouble x2 = o->x + 0.9*(p2->x - o->x);
  gdouble y2 = o->y + 0.9*(p2->y - o->y);
  gdouble z2 = o->z + 0.9*(p2->z - o->z);
  gdouble x = x2 - x1;
  gdouble y = y2 - y1;
  gdouble z = z2 - z1;

  fprintf (fp, "VECT 1 3 0 3 0 %g %g %g %g %g %g %g %g %g\n",
	   x1 + x - (x - y/2.)/5.,
	   y1 + y - (x/2. + y)/5.,
	   z1 + z - (x/2. + z)/5.,
	   x1 + x,
	   y1 + y,
	   z1 + z,
	   x1 + x - (x + y/2.)/5.,
	   y1 + y + (x/2. - y)/5.,
	   z1 + z + (x/2. - z)/5.);
  fprintf (fp, "VECT 1 2 0 2 0 %g %g %g %g %g %g\n",
	   x1, y1, z1,
	   x1 + x,
	   y1 + y,
	   z1 + z);
}

static void write_segments (GSList * boundary, GSList * interior,
			    FILE * fp)
{
  GSList * i = boundary;

  fprintf (fp, "LIST {\n");
  while (i) {
    GSList * inext = i->next;
    GtsSegment * s = i->data;
    GtsSegment * next = inext ? inext->data : boundary->data;
    GtsVertex * v1, * v2;

    if (s->v1 != next->v1 && s->v1 != next->v2) {
      v1 = s->v1;
      v2 = s->v2;
    }
    else {
      v1 = s->v2;
      v2 = s->v1;
    }
    draw_vector (GTS_POINT (v1), GTS_POINT (v2), fp);
    i = inext;
  }
  i = interior;
  while (i) {
    GtsSegment * s = i->data;

    draw_vector (GTS_POINT (s->v1), GTS_POINT (s->v2), fp);
    i = i->next;
  }
  fprintf (fp, "}\n");
}

static void write_loops (GSList * i, FILE * fp)
{
  guint nl = 0;

  while (i) {
    GtsSegment * start = i->data, * s;
    GtsPoint os;
    guint n = 0;

    fprintf (fp, "(geometry \"loop%d\" = LIST {\n", nl++);    

    os.x = os.y = os.z = 0.;
    s = start;
    do {
      GtsSegment * next = NEXT (s);
      GtsPoint * p;
      
      if (s->v1 != next->v1 && s->v1 != next->v2)
	p = GTS_POINT (s->v1);
       else
	 p = GTS_POINT (s->v2);
      os.x += p->x; os.y += p->y; os.z += p->z; n++;
      s = next;
     } while (s != start);
    os.x /= n; os.y /= n; os.z /= n;
    
    s = start;
    do {
      GtsSegment * next = NEXT (s);
      
      if (s->v1 != next->v1 && s->v1 != next->v2)
	draw_vector1 (GTS_POINT (s->v1), GTS_POINT (s->v2), &os, fp);
      else
	 draw_vector1 (GTS_POINT (s->v2), GTS_POINT (s->v1), &os, fp);
      s = next;
    } while (s != start);
    
    fprintf (fp, "})\n");

    i = i->next;
  }
}

#define NAME(v) (GTS_IS_NVERTEX (v) ? GTS_NVERTEX (v)->name : "")
#endif /* DEBUG */

static GtsSegment * prev_flag (GtsSegment * s, CurveFlag flag)
{
  GSList * i = s->v1->segments;

  while (i) {
    if (i->data != s && IS_SET (i->data, flag))
      return i->data;
    i = i->next;
  }
  return NULL;
}

static GtsSegment * next_flag (GtsSegment * s, CurveFlag flag)
{
  GSList * i = s->v2->segments;

  while (i) {
    if (i->data != s && IS_SET (i->data, flag))
      return i->data;
    i = i->next;
  }
  return NULL;
}

static GtsSegment * next_interior (GtsVertex * v)
{
  GSList * i = v->segments;

  while (i) {
    GtsSegment * s = i->data;

    if (s->v1 == v && IS_SET (s, INTERIOR))
      return s;
    i = i->next;
  }
  return NULL;
}

static GtsSegment * prev_interior (GtsVertex * v)
{
  GSList * i = v->segments;

  while (i) {
    GtsSegment * s = i->data;

    if (s->v2 == v && IS_SET (s, INTERIOR))
      return s;
    i = i->next;
  }
  return NULL;
}

static GtsSegment * reverse (GtsSegment * start,
			     gboolean interior,
			     gboolean * isloop)
{
  GtsSegment * s = start, * prev = NULL, * rprev = NULL;
  GtsSegment * rstart = NULL, * rstart1 = NULL;

  do {
    GtsSegment * rs;

    g_assert (IS_EDGE_INTER (s));
    rs = GTS_SEGMENT (edge_inter_new (s->v2, s->v1,
				      EDGE_INTER (s)->t1, EDGE_INTER (s)->t2));

    if (rstart == NULL)
      rstart = rs;
    else if (rstart1 == NULL)
      rstart1 = rs;
    if (interior)
      SET (rs, INTERIOR);
    NEXT (rs) = rprev;
    rprev = rs;
    prev = s;
    s = NEXT (s);
  } while (s != NULL && s != start);
  if (s == start) {
    NEXT (rstart) = rprev;
    *isloop = TRUE;
  }
  else {
    NEXT (rstart) = start;
    NEXT (prev) = rprev;
    *isloop = FALSE;
  }    
  return rstart1;
}

static GSList * interior_loops (GSList * interior)
{
  GSList * i = interior;
  GSList * loops = NULL;

  i = interior;
  while (i) {
    GtsSegment * s = i->data;

    if (IS_SET (s, RELEVANT)) {
      GtsSegment * start = s, * end;

      do {
	GtsSegment * next = next_flag (s, INTERIOR);

	UNSET (s, RELEVANT);
	end = s; 
	s = NEXT (s) = next;
      } while (s != NULL && s != start);

      if (s == start)
	loops = g_slist_prepend (loops, start);
      else {
	GtsSegment * next, * prev;
	gboolean isloop;

	s = prev_flag (start, INTERIOR);
	while (s) {
	  UNSET (s, RELEVANT);
	  NEXT (s) = start;
	  start = s;
	  s = prev_flag (s, INTERIOR);
	}
	next = next_flag (end, RELEVANT);
	prev = prev_flag (start, RELEVANT);
	if (prev != NULL)
	  SET (start->v1, INTERIOR);
	if (next != NULL)
	  SET (end->v2, INTERIOR);
	if (next == NULL && prev == NULL)
	  loops = g_slist_prepend (loops, start);
	else
	  reverse (start, TRUE, &isloop);
      }
    }
    i = i->next;
  }
  return loops;
}

#define ORIENTATION(p1,p2,p3,o) (gts_point_orientation_3d (p1, p2, o, p3))
#define ORIENTATION_SOS(p1,p2,p3,o) (gts_point_orientation_3d_sos (p1, p2, o, p3))

#define ORIENTED_VERTICES(s,next,w1,w2) {\
  if ((s)->v1 == (next)->v1 || (s)->v1 == (next)->v2) {\
    w1 = (s)->v2;\
    w2 = (s)->v1;\
  }\
  else {\
    w1 = (s)->v1;\
    w2 = (s)->v2;\
  }\
}

#if 0
static GtsSegment * segment_intersects (GtsPoint * p1, GtsPoint * p2,
					GSList * i,
					GtsPoint * o)
{
  while (i) {
    GtsSegment * s = i->data;
    GtsPoint * p3 = GTS_POINT (s->v1);
    GtsPoint * p4 = GTS_POINT (s->v2);

    if (p3 != p1 && p3 != p2 && p4 != p1 && p4 != p2) {
      gdouble o1 = ORIENTATION (p3, p4, p1, o);
      gdouble o2 = ORIENTATION (p3, p4, p2, o);

      if ((o1 < 0. && o2 > 0.) || (o1 > 0. && o2 < 0.)) {
	o1 = ORIENTATION (p1, p2, p3, o);
	o2 = ORIENTATION (p1, p2, p4, o);

	if ((o1 <= 0. && o2 >= 0.) || (o1 >= 0. && o2 <= 0.))
	  return s;
      }
    }
    i = i->next;
  }
  return NULL;
}
#else
static GtsSegment * segment_intersects (GtsPoint * p1, GtsPoint * p2,
					GSList * i,
					GtsPoint * o)
{
  while (i) {
    GtsSegment * s = i->data;
    GtsPoint * p3 = GTS_POINT (s->v1);
    GtsPoint * p4 = GTS_POINT (s->v2);

    if (p3 != p1 && p3 != p2 && p4 != p1 && p4 != p2) {
      gint o1 = ORIENTATION_SOS (p3, p4, p1, o);
      gint o2 = ORIENTATION_SOS (p3, p4, p2, o);

      if (o1*o2 < 0) {
	o1 = ORIENTATION_SOS (p1, p2, p3, o);
	o2 = ORIENTATION_SOS (p1, p2, p4, o);

	if (o1*o2 < 0)
	  return s;
      }
    }
    i = i->next;
  }
  return NULL;
}
#endif

static gboolean is_inside_wedge (GtsSegment * s1, GtsSegment * s2,
				 GtsPoint * p, GtsPoint * o)
{
  GtsVertex * v1, * v2, * v3;

  ORIENTED_VERTICES (s1, s2, v1, v2);
  v3 = s2->v1 != v2 ? s2->v1 : s2->v2;

  if (ORIENTATION (GTS_POINT (v1), GTS_POINT (v2), 
		   GTS_POINT (v3), o) >= 0.) {
    if (ORIENTATION (GTS_POINT (v1), GTS_POINT (v2), p, o) <= 0. ||
	ORIENTATION (GTS_POINT (v2), GTS_POINT (v3), p, o) <= 0.)
      return FALSE;
  }
  else if (ORIENTATION (GTS_POINT (v1), GTS_POINT (v2), p, o) <= 0. &&
	   ORIENTATION (GTS_POINT (v2), GTS_POINT (v3), p, o) <= 0.)
    return FALSE;
  return TRUE;
}

static GtsSegment * connection (GtsPoint * p, 
				GSList * interior,
				GSList * bloops,
				GtsPoint * o)
{
  while (bloops) {
    GtsSegment * start = bloops->data, * s = start;

    do {
      GtsSegment * next = NEXT (s);
      GtsVertex * v2 = s->v1 == next->v1 || s->v1 == next->v2 ? s->v1 : s->v2;

      if (is_inside_wedge (s, next, p, o) &&
	  !segment_intersects (p, GTS_POINT (v2), interior, o))
	return s;
      s = next;
    } while (s != start);
    bloops = bloops->next;
  }
  return NULL;
}

static gdouble loop_orientation (GtsSegment * start,
				 GtsPoint * p, GtsPoint * o)
{
  GtsSegment * s = start;
  gdouble or = 0.;

  do {
    GtsSegment * next = NEXT (s);
    GtsVertex * v1, * v2;

    ORIENTED_VERTICES (s, next, v1, v2);
    or += ORIENTATION (p, GTS_POINT (v1), GTS_POINT (v2), o);
    s = next;
  } while (s != start);

#ifdef DEBUG
  fprintf (stderr, "loop orientation: %g\n", or);
#endif /* DEBUG */

  return or;
}

static void connect_interior_loop (GtsSegment * start,
				   GSList ** interior,
				   GSList ** bloops,
				   GtsSurface * surface,
				   GtsPoint * o)
{
  GtsSegment * s = start, * c = NULL, * next, * s1, * rs1, * rs;
  GtsVertex * v, * cv;
  gboolean isloop;

  do {
    if (!(c = connection (GTS_POINT (s->v2), *interior, *bloops, o)))
      s = NEXT (s);
  } while (s != start && !c);
  g_assert (c);
  next = NEXT (c);
  v = c->v1 == next->v1 || c->v1 == next->v2 ? c->v1 : c->v2;
  cv = s->v2;
#ifdef DEBUG
  fprintf (stderr, "connecting %p:%s with %p:%s\n", 
	   cv, NAME (cv), v, NAME (v));
  fprintf (stderr, "  c: %p: %p:%s %p:%s\n", c, 
	   c->v1, NAME (c->v1),
	   c->v2, NAME (c->v2));
  fprintf (stderr, "  next: %p: %p:%s %p:%s\n", next,
	   next->v1, NAME (next->v1),
	   next->v2, NAME (next->v2));
#endif /* DEBUG */
  rs = reverse (s, FALSE, &isloop);
  if (isloop) {
    if (loop_orientation (rs, GTS_POINT (v), o) < 0.) {
      GtsSegment * tmp = s;
      s = rs;
      rs = tmp;
    }
    *bloops = g_slist_prepend (*bloops, rs);
  }
  s1 = GTS_SEGMENT (gts_edge_new (surface->edge_class, v, cv));
  rs1 = GTS_SEGMENT (gts_edge_new (surface->edge_class, cv, v));
  NEXT (c) = s1;
  NEXT (rs1) = next;
  *interior = g_slist_prepend (*interior, s1);
  NEXT (s1) = NEXT (s);
  NEXT (s) = rs1;
}

static GSList * boundary_loops (GSList * boundary)
{
  GSList * i = boundary;  
  GtsSegment * start = i->data;
  GSList * loops = NULL;

  while (i) {
    GtsSegment * s = i->data;
    GSList * inext = i->next;
    GtsSegment * next = inext ? inext->data : start;
    GtsVertex * v = s->v1 == next->v1 || s->v1 == next->v2 ? s->v1 : s->v2;

    if (IS_SET (v, INTERIOR)) {
      GtsSegment * intprev = prev_interior (v);

      NEXT (intprev) = next;
      NEXT (s) = next_interior (v);
      UNSET (v, INTERIOR);
    }
    else
      NEXT (s) = next;
    i = inext;
  }

  i = boundary;
  while (i) {
    start = i->data;
    
    if (IS_SET (start, RELEVANT)) {
      GtsSegment * s = start;

      do {
	UNSET (s, RELEVANT);
	UNSET (s, INTERIOR);
	s = NEXT (s);
      } while (s != start);
      loops = g_slist_prepend (loops, start);
    }
    i = i->next;
  }

  return loops;
}

typedef struct _Ear    Ear;

struct _Ear {
  GtsVertex * v1, * v2, * v3;
  GtsSegment * s1, * s2, * s3;
};

static gboolean point_in_wedge (GtsPoint * p1, GtsPoint * p2, GtsPoint * p3,
				GtsPoint * p, gboolean closed, GtsPoint * o)
{
  gdouble o1;

  if (p == p2 || p == p3)
    return FALSE;
  o1 = ORIENTATION (p1, p2, p, o);
  if ((closed && o1 < 0.) || (!closed && o1 <= 0.)) return FALSE;
  o1 = ORIENTATION (p3, p1, p, o);
  if ((closed && o1 < 0.) || (!closed && o1 <= 0.)) return FALSE;
  return TRUE;
}

#if 0
static gboolean segment_intersects1 (GtsPoint * p1, GtsPoint * p2, 
				     GtsPoint * p3, GtsPoint * p4,
				     gboolean closed, GtsPoint * o)
{
  gdouble o1 = ORIENTATION (p3, p4, p1, o);
  gdouble o2 = ORIENTATION (p3, p4, p2, o);
  gdouble o3, o4;

  if ((closed && ((o1 > 0. && o2 > 0.) || (o1 < 0. && o2 < 0.))) ||
      (!closed && ((o1 >= 0. && o2 >= 0.) || (o1 <= 0. && o2 <= 0.))))
    return FALSE;
  o3 = ORIENTATION (p1, p2, p3, o);
  o4 = ORIENTATION (p1, p2, p4, o);
  if ((o3 > 0. && o4 > 0.) || (o3 < 0. && o4 < 0.))
    return FALSE;
  if (closed) return TRUE;
  if ((o3 == 0. && o4 > 0.) || (o4 == 0. && o3 > 0.))
    return TRUE;
  return FALSE;
}
#else
static gboolean segment_intersects1 (GtsPoint * p1, GtsPoint * p2, 
				     GtsPoint * p3, GtsPoint * p4,
				     gboolean closed, GtsPoint * o)
{
  gint o1, o2;

  o1 = ORIENTATION_SOS (p3, p4, p1, o);
  o2 = ORIENTATION_SOS (p3, p4, p2, o);
  if (o1*o2 > 0)
    return FALSE;
  o1 = ORIENTATION_SOS (p1, p2, p3, o);
  o2 = ORIENTATION_SOS (p1, p2, p4, o);
  if (o1*o2 > 0)
    return FALSE;
  return TRUE;
}
#endif

static GtsSegment * triangle_intersects_segments (GtsPoint * p1,
						  GtsPoint * p2,
						  GtsPoint * p3,
						  gboolean closed,
						  GtsSegment * start,
						  GtsPoint * o)
{
  GtsSegment * s = start;

  do {
    GtsPoint * p4 = GTS_POINT (s->v1);
    GtsPoint * p5 = GTS_POINT (s->v2);

    if (p4 == p1) {
      if (point_in_wedge (p1, p2, p3, p5, closed, o))
	return s;
    }
    else if (p4 == p2) {
      if (point_in_wedge (p2, p3, p1, p5, closed, o))
	return s;
    }
    else if (p4 == p3) {
      if (point_in_wedge (p3, p1, p2, p5, closed, o))
	return s;
    }
    else if (p5 == p1) {
      if (point_in_wedge (p1, p2, p3, p4, closed, o))
	return s;
    }
    else if (p5 == p2) {
      if (point_in_wedge (p2, p3, p1, p4, closed, o))
	return s;
    }
    else if (p5 == p3) {
      if (point_in_wedge (p3, p1, p2, p4, closed, o))
	return s;
    }
    else if (segment_intersects1 (p1, p2, p4, p5, closed, o) ||
	     segment_intersects1 (p2, p3, p4, p5, closed, o) ||
	     segment_intersects1 (p3, p1, p4, p5, closed, o))
      return s;
    s = NEXT (s);
  } while (s != start);
  return NULL;
}

static gboolean new_ear (GtsSegment * s, 
			 Ear * e, 
			 GtsSegment * start,
			 guint sloppy,
			 GtsPoint * o)
{
  gdouble or;

  e->s1 = s;
  e->s2 = NEXT (s);

  g_return_val_if_fail (e->s2, FALSE);
  g_return_val_if_fail (e->s2 != e->s1, FALSE);

  ORIENTED_VERTICES (e->s1, e->s2, e->v1, e->v2);
  e->v3 = e->s2->v1 != e->v2 ? e->s2->v1 : e->s2->v2;
  if (e->v3 == e->v1)
    return FALSE;
  e->s3 = NEXT (e->s2);
  if (gts_segment_connect (e->s3, e->v1, e->v3)) {
    if (NEXT (e->s3) != e->s1)
      return FALSE;
  }
  else if (gts_vertices_are_connected (e->v1, e->v3))
    return FALSE;
  else
    e->s3 = NULL;
  or = ORIENTATION (GTS_POINT (e->v1), GTS_POINT (e->v2), GTS_POINT (e->v3),o);
  switch (sloppy) {
  case 0: 
    if (or <= 0. ||
	triangle_intersects_segments (GTS_POINT (e->v1), GTS_POINT (e->v2),
				      GTS_POINT (e->v3), TRUE, start, o))
      return FALSE;
    break;
  case 1:
    if (or < 0. || 
	(or > 0. && 
	 triangle_intersects_segments (GTS_POINT (e->v1), GTS_POINT (e->v2),
				       GTS_POINT (e->v3), FALSE, start, o)))
      return FALSE;
    break;
  case 2:
    if ((or > 0. && 
	 triangle_intersects_segments (GTS_POINT (e->v1), GTS_POINT (e->v2),
				       GTS_POINT (e->v3), FALSE, start, o)) ||
	(or < 0. && 
	 triangle_intersects_segments (GTS_POINT (e->v2), GTS_POINT (e->v1),
				       GTS_POINT (e->v3), FALSE, start, o)))
      return FALSE;
    break;
  case 3:
    if (or < 0.)
      return FALSE;
    break;
  }
#ifdef DEBUG
  if (or <= 0.)
    fprintf (stderr, "or: %g\n", or);
#endif /* DEBUG */
  g_assert (or > -1e-6);
  return TRUE;
}

static void triangulate_loop (GtsSegment * start,
			      GtsSurface * surface,
			      GtsPoint * o)
{
  GtsSegment * prev = start, * s;
  guint sloppy = 0;
#ifdef DEBUG
  guint nt = 0;
#endif /* DEBUG */

  s = NEXT (start);
  while (NEXT (s) != s) {
    GtsSegment * next = NEXT (s);
    Ear e;

#ifdef DEBUG
    fprintf (stderr, "prev: %p s: %p next: %p\n", prev, s, next);
#endif /* DEBUG */
  
    if (!new_ear (s, &e, start, sloppy, o)) {
      if (s == start) {
	sloppy++;
#ifdef DEBUG
	fprintf (stderr, "sloppy: %u\n", sloppy);
#endif /* DEBUG */
      }
      prev = s;
      s = next;
    }
    else {
      GtsFace * f;

      if (!GTS_IS_EDGE (e.s3))
	e.s3 = GTS_SEGMENT (gts_edge_new (surface->edge_class, e.v1, e.v3));
      f = gts_face_new (surface->face_class, 
			GTS_EDGE (e.s1), GTS_EDGE (e.s2), GTS_EDGE (e.s3));
      gts_surface_add_face (surface, f);
      UNSET (e.s1, RELEVANT);
      UNSET (e.s1, INTERIOR);
      UNSET (e.s2, RELEVANT);
      UNSET (e.s2, INTERIOR);
      NEXT (prev) = e.s3;
      NEXT (e.s3) = NEXT (e.s2);
      NEXT (e.s1) = NEXT (e.s2) = NULL;
      start = prev;
      s = NEXT (prev);
      sloppy = 0;
#ifdef DEBUG
      {
	gchar name[80];
	FILE * fp;
	
	fprintf (stderr, " t.%u: (%p:%s,%p:%s,%p:%s)\n",
		 nt, 
		 e.v1, NAME (e.v1),
		 e.v2, NAME (e.v2),
		 e.v3, NAME (e.v3));
	sprintf (name, "/tmp/t.%u", nt++);
	fp = fopen (name, "wt");
	//	gts_surface_write (surface, fp);
	gts_write_triangle (GTS_TRIANGLE (f), NULL, fp);
	//	  write_graph1 (start, interior, surface, fp);
	fclose (fp);
	print_loop (start, stderr);
      }
#endif /* DEBUG */
    }
  }
  UNSET (s, RELEVANT);
  UNSET (s, INTERIOR);
  NEXT (s) = NULL;
}

static void check_object (GtsObject * o)
{
  g_assert (o->reserved == NULL);
  g_assert (o->flags == 0);  
}

static void check_boundary (GtsEdge * e, GtsSurface * s)
{
  check_object (GTS_OBJECT (e));
  check_object (GTS_OBJECT (GTS_SEGMENT (e)->v1));
  check_object (GTS_OBJECT (GTS_SEGMENT (e)->v2));
  g_assert (gts_edge_face_number (e, s) == 1);
}

static void check_interior (GtsEdge * e, GtsSurface * s)
{
  guint n;
  check_object (GTS_OBJECT (e));
  check_object (GTS_OBJECT (GTS_SEGMENT (e)->v1));
  check_object (GTS_OBJECT (GTS_SEGMENT (e)->v2));

  n = gts_edge_face_number (e, s);
#ifdef DEBUG
  if (n != 2)
    gts_surface_print_stats (s, stderr);
#endif /* DEBUG */
  g_assert (n == 2);
}

static void check_boundary_interior_triangulation (GSList * boundary,
						   GSList * interior,
						   GtsSurface * surface)
{
  g_slist_foreach (boundary, (GFunc) check_boundary, surface);
  g_slist_foreach (interior, (GFunc) check_interior, surface);
}

static void merge_duplicate (GtsEdge * e)
{
  GtsEdge * dup = gts_edge_is_duplicate (e);

  g_assert (dup);
  gts_edge_replace (dup, e);
  gts_object_destroy (GTS_OBJECT (dup));
}

static void triangulate_boundary_interior (GSList * boundary, 
					   GSList * interior,
					   GtsSurface * s,
					   GtsPoint * o)
{
  GSList * iloops, * bloops, * i;

  i = boundary;
  while (i) {
    SET (i->data, RELEVANT);
    i = i->next;
  }
  i = interior;
  while (i) {
    SET (i->data, RELEVANT);
    SET (i->data, INTERIOR);
    i = i->next;
  }

  iloops = interior_loops (interior);
  bloops = boundary_loops (boundary);

  i = iloops;
  while (i) {
#ifdef DEBUG
    fprintf (stderr, "--- interior loop ---\n");
    print_loop (i->data, stderr);
#endif /* DEBUG */
    connect_interior_loop (i->data, &interior, &bloops, s, o);
    i = i->next;
  }
  
#ifdef DEBUG
 {
   FILE * fp = fopen ("/tmp/bloops", "w");
   write_loops (bloops, fp);
   fclose (fp);
 }
#endif /* DEBUG */

  i = bloops;
  while (i) {
#ifdef DEBUG
    fprintf (stderr, "--- boundary loop ---\n");
    print_loop (i->data, stderr);
#endif /* DEBUG */
    triangulate_loop (i->data, s, o);
    i = i->next;
  }
  
  g_slist_foreach (interior, (GFunc) merge_duplicate, NULL);
  g_slist_free (iloops);
  g_slist_free (bloops);

#ifdef CHECK_ORIENTED
  check_boundary_interior_triangulation (boundary, interior, s);
#endif /* CHECK_ORIENTED */
}

static void create_edges (GtsSegment * s, GtsSurface * surface)
{
  if (GTS_OBJECT (s)->reserved) {
    GList * i = GTS_OBJECT (s)->reserved;
    GtsVertex * v1 = i->data;

    GTS_OBJECT (s)->reserved = g_list_prepend (i, 
		      gts_edge_new (surface->edge_class, s->v1, v1));
    while (i) {
      GList * next = i->next;
      GtsVertex * v2 = next ? next->data : s->v2;

      GTS_OBJECT (i->data)->reserved = NULL;
      i->data = gts_edge_new (surface->edge_class, v1, v2);
      v1 = v2;
      i = next;
    }
  }
}

static void add_boundary (GtsSegment * s, GtsSegment * next, 
			  GSList ** boundary)
{
  if (GTS_OBJECT (s)->reserved == NULL)
    *boundary = g_slist_prepend (*boundary, s);
  else {
    if (s->v2 == next->v2 || s->v2 == next->v1) {
      GList * i = g_list_last (GTS_OBJECT (s)->reserved);

      while (i) {
	*boundary = g_slist_prepend (*boundary, i->data);
	i = i->prev;
      }
    }
    else {
      GList * i = GTS_OBJECT (s)->reserved;

      while (i) {
	*boundary = g_slist_prepend (*boundary, i->data);
	i = i->next;
      }
    }
  }
}

static void triangulate_face (GtsTriangle * t, GtsSurface * surface)
{
  GSList * interior = GTS_OBJECT (t)->reserved;
  GSList * boundary = NULL;
  GtsSurface * s = gts_surface_new (gts_surface_class (),
				    surface->face_class,
				    surface->edge_class,
				    surface->vertex_class);
  gdouble x, y, z;
  GtsPoint * p = GTS_POINT (GTS_SEGMENT (t->e1)->v1);
  GtsPoint * o;

  GTS_OBJECT (t)->reserved = NULL;  
  gts_triangle_normal (t, &x, &y, &z);
  g_assert (x != 0. || y != 0. || z != 0.);
  o = gts_point_new (gts_point_class (), p->x + x, p->y + y, p->z + z);
  add_boundary (GTS_SEGMENT (t->e3), GTS_SEGMENT (t->e1), &boundary);
  add_boundary (GTS_SEGMENT (t->e2), GTS_SEGMENT (t->e3), &boundary);
  add_boundary (GTS_SEGMENT (t->e1), GTS_SEGMENT (t->e2), &boundary);
#ifdef DEBUG
  {
    static guint nt = 0;
    char name[80];
    FILE * fp;

    fprintf (stderr, "%u: triangulating %p\n", nt, t);
if (nt == 28)
  fprintf (stderr, "tintin!!!!\n");
    sprintf (name, "/tmp/oc.%u", nt++);
    fp = fopen (name, "wt");
    //    write_graph (boundary, interior, s, fp);
    write_segments (boundary, interior, fp);
    fclose (fp);
  }
#endif /* DEBUG */
  triangulate_boundary_interior (boundary, interior, s, o);
  g_slist_free (interior);
  g_slist_free (boundary);
  if (GTS_OBJECT (t)->klass->attributes)
    gts_surface_foreach_face (s, (GtsFunc) gts_object_attributes, t);
  gts_surface_merge (surface, s);
  gts_object_destroy (GTS_OBJECT (s));
  gts_object_destroy (GTS_OBJECT (o));
}

static void free_edge_list (GtsObject * o)
{
  g_list_free (o->reserved);
  o->reserved = NULL;
}

/**
 * gts_surface_inter_new:
 * @klass: a #GtsSurfaceInterClass.
 * @s1: a #GtsSurface.
 * @s2: a #GtsSurface.
 * @faces_tree1: a bounding box tree (see gts_bb_tree_new()) for
 * the faces of @s1.
 * @faces_tree2: a bounding box tree for the faces of @s2.
 * @is_open1: whether @s1 is an "open" surface.
 * @is_open2: whether @s2 is an "open" surface.
 *
 * When triangulating the cut faces, the new faces inherit the
 * attributes of these original faces through their attributes()
 * method.
 *
 * Returns: a new #GtsSurfaceInter describing the intersection of @s1
 * and @s2.  
 */
GtsSurfaceInter * gts_surface_inter_new (GtsSurfaceInterClass * klass,
					 GtsSurface * s1,
					 GtsSurface * s2,
					 GNode * faces_tree1,
					 GNode * faces_tree2,
					 gboolean is_open1,
					 gboolean is_open2)
{
  GtsSurfaceInter * si;
  GtsSurface * s;

  g_return_val_if_fail (klass != NULL, NULL);
  g_return_val_if_fail (s1 != NULL, NULL);
  g_return_val_if_fail (s2 != NULL, NULL);
  g_return_val_if_fail (faces_tree1 != NULL, NULL);
  g_return_val_if_fail (faces_tree2 != NULL, NULL);

  si = surface_inter_new (klass, s1, s2, faces_tree1, faces_tree2);

  gts_surface_foreach_edge (si->s1, (GtsFunc) create_edges, si->s1);
  gts_surface_foreach_edge (si->s2, (GtsFunc) create_edges, si->s2);

#ifdef DEBUG
  fprintf (stderr, "====== triangulating s1 ======\n");
#endif /* DEBUG */
  s = gts_surface_new (gts_surface_class (),
		       s1->face_class,
		       s1->edge_class,
		       s1->vertex_class);
  gts_surface_foreach_face (si->s1, (GtsFunc) triangulate_face, s);
  gts_surface_foreach_edge (si->s1, (GtsFunc) free_edge_list, NULL);
  gts_object_destroy (GTS_OBJECT (si->s1));
  si->s1 = s;
  GTS_OBJECT (si->s1)->reserved = s1;
  
#ifdef DEBUG
  fprintf (stderr, "====== triangulating s2 ======\n");
#endif /* DEBUG */
  s = gts_surface_new (gts_surface_class (),
		       s2->face_class,
		       s2->edge_class,
		       s2->vertex_class);
  gts_surface_foreach_face (si->s2, (GtsFunc) triangulate_face, s);
  gts_surface_foreach_edge (si->s2, (GtsFunc) free_edge_list, NULL);
  gts_object_destroy (GTS_OBJECT (si->s2));
  si->s2 = s;
  GTS_OBJECT (si->s2)->reserved = s2;

  return si;
}

static void check_surface_edge (GtsEdge * e, gpointer * data)
{
  gboolean * ok = data[0];
  GtsSurface * s = data[1];
  GtsSurface * bs = GTS_OBJECT (s)->reserved;
  guint nf = gts_edge_face_number (e, s);

  if (nf < 1 || nf > 2) {
    *ok = FALSE;
    g_return_if_fail (nf >= 1 && nf <= 2);
  }
  if (nf == 1 && gts_edge_face_number (e, bs) == 0) {
    *ok = FALSE;
    g_return_if_fail (gts_edge_face_number (e, bs) > 0);
  }
}

static void mark_edge (GtsObject * o, gpointer data)
{
  o->reserved = data;
}

static gint triangle_orientation (GtsTriangle * t, GtsEdge * e)
{
  GtsSegment * s = GTS_SEGMENT (t->e1 == e ? t->e2 
				: 
				t->e2 == e ? t->e3 
				: 
				t->e1);
  GtsVertex * v2 = GTS_SEGMENT (e)->v2;

  if (s->v1 == v2 || s->v2 == v2)
    return 1;
  return -1;
}

static gboolean check_orientation (GtsEdge * e, GtsSurface * s)
{
  GtsTriangle * t1 = NULL, * t2 = NULL;
  GSList * i = e->triangles;
  gint o1 = 0, o2 = 0;

  while (i) {
    if (GTS_IS_FACE (i->data) && 
	gts_face_has_parent_surface (i->data, s)) {
      if (t1 == NULL) {
	t1 = i->data;
	o1 = triangle_orientation (t1, e);
      }
      else if (t2 == NULL) {
	t2 = i->data;
	o2 = triangle_orientation (t2, e);
	g_return_val_if_fail (o1*o2 < 0, FALSE);
      }
      else
	g_assert_not_reached ();
    }
    i = i->next;
  }
  g_return_val_if_fail (t1 && t2, FALSE);
  return TRUE;
}

static void check_edge (GtsSegment * s, gpointer * data)
{
  gboolean * ok = data[0];
  GtsSurfaceInter * si = data[1];
  gboolean * closed = data[2];
  GSList * j;
  guint nn = 0;
  
  j = s->v1->segments;
  while (j && *ok) {
    GtsSegment * s1 = j->data;
    
    if (s1 != s && GTS_OBJECT (s1)->reserved == si) {
      if (s1->v2 != s->v1)
	*ok = FALSE;
      nn++;
    }
    j = j->next;
  }
  j = s->v2->segments;
  while (j && *ok) {
    GtsSegment * s1 = j->data;
    
    if (s1 != s && GTS_OBJECT (s1)->reserved == si) {
      if (s1->v1 != s->v2)
	*ok = FALSE;
      nn++;
    }
    j = j->next;
  }
  if (nn != 2)
    *closed = FALSE;

  if (!check_orientation (GTS_EDGE (s), si->s1))
    *ok = FALSE;
  if (!check_orientation (GTS_EDGE (s), si->s2))
    *ok = FALSE;
}

/**
 * gts_surface_inter_check:
 * @si: a #GtsSurfaceInter.
 * @closed: is set to %TRUE if @si->edges is a closed curve, %FALSE
 * otherwise.
 *
 * Returns: %TRUE if the curve described by @si is an orientable
 * manifold, %FALSE otherwise.  
 */
gboolean gts_surface_inter_check (GtsSurfaceInter * si,
				  gboolean * closed)
{
  gboolean ok = TRUE;
  gpointer data[3];

  g_return_val_if_fail (si != NULL, FALSE);
  g_return_val_if_fail (closed != NULL, FALSE);

  *closed = si->edges ? TRUE : FALSE;

  /* mark edges as used by si */
  g_slist_foreach (si->edges, (GFunc) mark_edge, si);

  data[0] = &ok;
  data[1] = si;
  data[2] = closed;
  g_slist_foreach (si->edges, (GFunc) check_edge, data);
  g_slist_foreach (si->edges, (GFunc) gts_object_reset_reserved, NULL);

  /* check connectivity of the faces of @si */
  if (*closed) {
    gpointer data[2];

    data[0] = &ok;
    data[1] = si->s1;
    gts_surface_foreach_edge (si->s1, (GtsFunc) check_surface_edge, data);
    data[1] = si->s2;
    gts_surface_foreach_edge (si->s2, (GtsFunc) check_surface_edge, data);
  }

  return ok;
}

/* Given @e and @f returns a #GtsFace compatible with @f and belonging to
   @s1 or @s2 */
static GtsFace * next_compatible_face (GtsEdge * e, 
				       GtsFace * f, 
				       GtsSurface * s1,
				       GtsSurface * s2)
{
  GSList * i = e->triangles;
  GtsFace * f2 = NULL, * f3 = NULL;

  while (i) {
    GtsFace * f1 = i->data;

    if (f1 != f && GTS_IS_FACE (f1)) {
      if (gts_face_has_parent_surface (f1, s1))
	return f1;
      if (gts_face_has_parent_surface (f1, s2)) {
	if (f2 == NULL) f2 = f1;
	else if (f3 == NULL) f3 = f1;
	else g_assert_not_reached (); /* s2 is a non-manifold surface */
      }
    }
    i = i->next;
  }
  if (f3 == NULL) {
    if (gts_edge_is_boundary (e, s2))
      return NULL;
    return f2; 
  }
  g_assert (gts_face_has_parent_surface (f, s1));
  if (gts_triangles_are_compatible (GTS_TRIANGLE (f), GTS_TRIANGLE (f2), e))
    return f2;
  return f3;
}

static void walk_faces (GtsEdge * e, GtsFace * f, 
			GtsSurface * s1,
			GtsSurface * s2,
			GtsSurface * s)
{
  GtsFifo * faces = gts_fifo_new ();
  GtsFifo * edges = gts_fifo_new ();

  gts_fifo_push (faces, f);
  gts_fifo_push (edges, e);
  while ((f = gts_fifo_pop (faces)) && (e = gts_fifo_pop (edges))) {
    if (!GTS_OBJECT (f)->reserved) {
      GtsTriangle * t = GTS_TRIANGLE (f);
      GtsFace * f1;

      gts_surface_add_face (s, f);
      GTS_OBJECT (f)->reserved = s;
      if (t->e1 != e && !GTS_OBJECT (t->e1)->reserved &&
	  (f1 = next_compatible_face (t->e1, f, s1, s2))) {
	gts_fifo_push (faces, f1);
	gts_fifo_push (edges, t->e1);
      }	
      if (t->e2 != e && !GTS_OBJECT (t->e2)->reserved &&
	  (f1 = next_compatible_face (t->e2, f, s1, s2))) {
	gts_fifo_push (faces, f1);
	gts_fifo_push (edges, t->e2);
      }	
      if (t->e3 != e && !GTS_OBJECT (t->e3)->reserved &&
	  (f1 = next_compatible_face (t->e3, f, s1, s2))) {
	gts_fifo_push (faces, f1);
	gts_fifo_push (edges, t->e3);
      }	
    }
  }
  gts_fifo_destroy (faces);
  gts_fifo_destroy (edges);
}

/**
 * gts_surface_inter_boolean:
 * @si: a #GtsSurfaceInter.
 * @surface: a #GtsSurface.
 * @op: a #GtsBooleanOperation.
 *
 * Adds to @surface the part of the surface described by @si and @op.
 */
void gts_surface_inter_boolean (GtsSurfaceInter * si,
				GtsSurface * surface,
				GtsBooleanOperation op)
{
  GtsSurface * s = NULL;
  gint orient = 1;
  GSList * i;

  g_return_if_fail (si != NULL);
  g_return_if_fail (surface != NULL);

  switch (op) {
  case GTS_1_OUT_2: s = si->s1; orient = 1; break;
  case GTS_1_IN_2: s = si->s1; orient = -1; break;
  case GTS_2_OUT_1: s = si->s2; orient = -1; break;
  case GTS_2_IN_1: s = si->s2; orient = 1; break;
  default: g_assert_not_reached ();
  }

  /* mark edges as belonging to intersection */
  g_slist_foreach (si->edges, (GFunc) mark_edge, si);

  i = si->edges;
  while (i) {
    GtsEdge * e = i->data;
    GSList * j = e->triangles;
    
    while (j) {
      if (gts_face_has_parent_surface (j->data, s) &&
	  orient*triangle_orientation (j->data, e) > 0) {
#ifdef DEBUG_BOOLEAN
	GtsFace * boundary = gts_edge_is_boundary (e, surface);

	g_assert (!boundary || boundary == j->data);
#endif /* DEBUG_BOOLEAN */
	walk_faces (e, j->data, s, GTS_OBJECT (s)->reserved, surface);
	break;
      }
      j = j->next;
    }
    i = i->next;
  }
  g_slist_foreach (si->edges, (GFunc) gts_object_reset_reserved, NULL);
  gts_surface_foreach_face (surface, 
			    (GtsFunc) gts_object_reset_reserved, NULL);
}

static void self_intersecting (GtsBBox * bb1, GtsBBox * bb2, 
			       gpointer * d)
{
  GtsTriangle * t1 = bb1->bounded;
  GtsTriangle * t2 = bb2->bounded;

  if (t1 != t2) {
    GtsSegment * s1 = GTS_SEGMENT (t1->e1);
    GtsSegment * s2 = GTS_SEGMENT (t1->e2);
    GtsSegment * s3 = GTS_SEGMENT (t1->e3);
    GtsSegment * s4 = GTS_SEGMENT (t2->e1);
    GtsSegment * s5 = GTS_SEGMENT (t2->e2);
    GtsSegment * s6 = GTS_SEGMENT (t2->e3);
    GtsPoint * pi;

    if ((!gts_segments_touch (s4, s1) && 
	 !gts_segments_touch (s4, s2) &&
	 !gts_segments_touch (s4, s3) &&
	 (pi = segment_triangle_intersection (s4, t1, gts_point_class ()))
	 != NULL) ||
	(!gts_segments_touch (s5, s1) && 
	 !gts_segments_touch (s5, s2) &&
	 !gts_segments_touch (s5, s3) &&
	 (pi = segment_triangle_intersection (s5, t1, gts_point_class ())) 
	 != NULL) ||
	(!gts_segments_touch (s6, s1) && 
	 !gts_segments_touch (s6, s2) &&
	 !gts_segments_touch (s6, s3) &&
	 (pi = segment_triangle_intersection (s6, t1, gts_point_class ())) 
	 != NULL)) {
      GtsBBTreeTraverseFunc func = d[0];
      gpointer data = d[1];
      gboolean * self_inter = d[2];

      gts_object_destroy (GTS_OBJECT (pi));
      *self_inter = TRUE;
      (* func) (bb1, bb2, data);
    }
  }
}

/**
 * gts_surface_foreach_intersecting_face:
 * @s: a #GtsSurface.
 * @func: a #GtsBBTreeTraverseFunc.
 * @data: user data to pass to @func.
 *
 * Calls @func for each intersecting pair of faces of @s.
 *
 * Returns: %TRUE if @func was called at least once, %FALSE otherwise.
 */
gboolean gts_surface_foreach_intersecting_face (GtsSurface * s,
						GtsBBTreeTraverseFunc func,
						gpointer data)
{
  GNode * tree;
  gpointer d[3];
  gboolean self_inter = FALSE;

  g_return_val_if_fail (s != NULL, FALSE);
  g_return_val_if_fail (func != NULL, FALSE);

  tree = gts_bb_tree_surface (s);
  d[0] = func;
  d[1] = data;
  d[2] = &self_inter;
  gts_bb_tree_traverse_overlapping (tree, tree, 
				    (GtsBBTreeTraverseFunc) self_intersecting,
				    d);
  gts_bb_tree_destroy (tree, TRUE);

  return self_inter;
}

static void add_intersecting (GtsBBox * bb1, GtsBBox * bb2, 
			      GtsSurface * intersected)
{
  gts_surface_add_face (intersected, bb1->bounded);
  gts_surface_add_face (intersected, bb2->bounded);
}

/**
 * gts_surface_is_self_intersecting:
 * @s: a #GtsSurface.
 *
 * Returns: a new #GtsSurface containing the faces of @s which are
 * self-intersecting or %NULL if no faces of @s are self-intersecting.
 */
GtsSurface * gts_surface_is_self_intersecting (GtsSurface * s)
{
  GtsSurface * intersected;

  g_return_val_if_fail (s != NULL, NULL);

  intersected = gts_surface_new (GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass),
				 s->face_class,
				 s->edge_class,
				 s->vertex_class);
  if (!gts_surface_foreach_intersecting_face (s,
		      (GtsBBTreeTraverseFunc) add_intersecting, intersected)) {
    gts_object_destroy (GTS_OBJECT (intersected));
    intersected = NULL;
  }
  return intersected;
}
