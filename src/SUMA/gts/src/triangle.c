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

static void triangle_destroy (GtsObject * object)
{
  GtsTriangle * triangle = GTS_TRIANGLE (object);
  GtsEdge * e1 = triangle->e1;
  GtsEdge * e2 = triangle->e2;
  GtsEdge * e3 = triangle->e3;

  e1->triangles = g_slist_remove (e1->triangles, triangle);
  if (!GTS_OBJECT_DESTROYED (e1) &&
      !gts_allow_floating_edges && e1->triangles == NULL)
    gts_object_destroy (GTS_OBJECT (e1));
  
  e2->triangles = g_slist_remove (e2->triangles, triangle);
  if (!GTS_OBJECT_DESTROYED (e2) &&
      !gts_allow_floating_edges && e2->triangles == NULL)
    gts_object_destroy (GTS_OBJECT (e2));
  
  e3->triangles = g_slist_remove (e3->triangles, triangle);
  if (!GTS_OBJECT_DESTROYED (e3) &&
      !gts_allow_floating_edges && e3->triangles == NULL)
    gts_object_destroy (GTS_OBJECT (e3));

  (* GTS_OBJECT_CLASS (gts_triangle_class ())->parent_class->destroy) (object);
}

static void triangle_class_init (GtsObjectClass * klass)
{
  klass->destroy = triangle_destroy;
}

static void triangle_init (GtsTriangle * triangle)
{
  triangle->e1 = triangle->e2 = triangle->e3 = NULL;
}

/**
 * gts_triangle_class:
 *
 * Returns: the #GtsTriangleClass.
 */
GtsTriangleClass * gts_triangle_class (void)
{
  static GtsTriangleClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo triangle_info = {
      "GtsTriangle",
      sizeof (GtsTriangle),
      sizeof (GtsTriangleClass),
      (GtsObjectClassInitFunc) triangle_class_init,
      (GtsObjectInitFunc) triangle_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (), 
				  &triangle_info);
  }

  return klass;
}

/**
 * gts_triangle_set:
 * @triangle: a #GtsTriangle.
 * @e1: a #GtsEdge.
 * @e2: another #GtsEdge touching @e1.
 * @e3: another #GtsEdge touching both @e1 and @e2.
 *
 * Sets the edge of @triangle to @e1, @e2 and @e3 while checking that they
 * define a valid triangle.
 */
void gts_triangle_set (GtsTriangle * triangle, 
		       GtsEdge * e1, 
		       GtsEdge * e2,
		       GtsEdge * e3)
{
  g_return_if_fail (e1 != NULL);
  g_return_if_fail (e2 != NULL);
  g_return_if_fail (e3 != NULL);
  g_return_if_fail (e1 != e2 && e1 != e3 && e2 != e3);

  triangle->e1 = e1;
  triangle->e2 = e2;
  triangle->e3 = e3;

  if (GTS_SEGMENT (e1)->v1 == GTS_SEGMENT (e2)->v1)
    g_return_if_fail (gts_segment_connect (GTS_SEGMENT (e3), 
					   GTS_SEGMENT (e1)->v2, 
					   GTS_SEGMENT (e2)->v2));
  else if (GTS_SEGMENT (e1)->v2 == GTS_SEGMENT (e2)->v1)
    g_return_if_fail (gts_segment_connect (GTS_SEGMENT (e3), 
					   GTS_SEGMENT (e1)->v1, 
					   GTS_SEGMENT (e2)->v2));
  else if (GTS_SEGMENT (e1)->v2 == GTS_SEGMENT (e2)->v2)
    g_return_if_fail (gts_segment_connect (GTS_SEGMENT (e3), 
					   GTS_SEGMENT (e1)->v1, 
					   GTS_SEGMENT (e2)->v1));
  else if (GTS_SEGMENT (e1)->v1 == GTS_SEGMENT (e2)->v2)
    g_return_if_fail (gts_segment_connect (GTS_SEGMENT (e3), 
					   GTS_SEGMENT (e1)->v2, 
					   GTS_SEGMENT (e2)->v1));
  else
    g_assert_not_reached ();

  e1->triangles = g_slist_prepend (e1->triangles, triangle);
  e2->triangles = g_slist_prepend (e2->triangles, triangle);
  e3->triangles = g_slist_prepend (e3->triangles, triangle);
}

/**
 * gts_triangle_new:
 * @klass: a #GtsTriangleClass.
 * @e1: a #GtsEdge.
 * @e2: another #GtsEdge touching @e1.
 * @e3: another #GtsEdge touching both @e1 and @e2.
 *
 * Returns: a new #GtsTriangle having @e1, @e2 and @e3 as edges.
 */
GtsTriangle * gts_triangle_new (GtsTriangleClass * klass,
				GtsEdge * e1,
				GtsEdge * e2,
				GtsEdge * e3)
{
  GtsTriangle * t;

  t = GTS_TRIANGLE (gts_object_new (GTS_OBJECT_CLASS (klass)));
  gts_triangle_set (t, e1, e2, e3);
  
  return t;
}

/**
 * gts_triangle_vertex_opposite:
 * @t: a #GtsTriangle.
 * @e: a #GtsEdge used by @t.
 *
 * This function fails if @e is not an edge of @t.
 * 
 * Returns: a #GtsVertex, vertex of @t which does not belong to @e.
 */
GtsVertex * gts_triangle_vertex_opposite (GtsTriangle * t, GtsEdge * e)
{
  g_return_val_if_fail (t != NULL, NULL);
  g_return_val_if_fail (e != NULL, NULL);

  if (t->e1 == e) {
    GtsVertex * v = GTS_SEGMENT (t->e2)->v1;
    if (v != GTS_SEGMENT (e)->v1 && v != GTS_SEGMENT (e)->v2)
      return v;
    return GTS_SEGMENT (t->e2)->v2;
  }
  if (t->e2 == e) {
    GtsVertex * v = GTS_SEGMENT (t->e1)->v1;
    if (v != GTS_SEGMENT (e)->v1 && v != GTS_SEGMENT (e)->v2)
      return v;
    return GTS_SEGMENT (t->e1)->v2;
  }
  if (t->e3 == e) {
    GtsVertex * v = GTS_SEGMENT (t->e2)->v1;
    if (v != GTS_SEGMENT (e)->v1 && v != GTS_SEGMENT (e)->v2)
      return v;
    return GTS_SEGMENT (t->e2)->v2;
  }
  g_assert_not_reached ();
  return NULL;
}

/**
 * gts_triangle_edge_opposite:
 * @t: a #GtsTriangle.
 * @v: a #GtsVertex of @t.
 *
 * Returns: the edge of @t opposite @v or %NULL if @v is not a vertice of @t.
 */
GtsEdge * gts_triangle_edge_opposite (GtsTriangle * t, GtsVertex * v)
{
  GtsSegment * s1, * s2, * s3;

  g_return_val_if_fail (t != NULL, NULL);
  g_return_val_if_fail (v != NULL, NULL);

  s1 = GTS_SEGMENT (t->e1);
  s2 = GTS_SEGMENT (t->e2);

  if (s1->v1 != v && s1->v2 != v) {
    if (s2->v1 != v && s2->v2 != v)
      return NULL;
    return t->e1;
  }
  if (s2->v1 != v && s2->v2 != v)
    return t->e2;
  s3 = GTS_SEGMENT (t->e3);
  g_assert (s3->v1 != v && s3->v2 != v);
  return t->e3;
}

/**
 * gts_triangles_angle:
 * @t1: a #GtsTriangle.
 * @t2: a #GtsTriangle.
 *
 * Returns: the value (in radians) of the angle between @t1 and @t2.
 */
gdouble gts_triangles_angle (GtsTriangle * t1,
			     GtsTriangle * t2)
{
  gdouble nx1, ny1, nz1, nx2, ny2, nz2;
  gdouble pvx, pvy, pvz;
  gdouble theta;

  g_return_val_if_fail (t1 != NULL && t2 != NULL, 0.0);

  gts_triangle_normal (t1, &nx1, &ny1, &nz1);
  gts_triangle_normal (t2, &nx2, &ny2, &nz2);

  pvx = ny1*nz2 - nz1*ny2;
  pvy = nz1*nx2 - nx1*nz2;
  pvz = nx1*ny2 - ny1*nx2;

  theta = atan2 (sqrt (pvx*pvx + pvy*pvy + pvz*pvz), 
		 nx1*nx2 + ny1*ny2 + nz1*nz2) - M_PI;
  return theta < - M_PI ? theta + 2.*M_PI : theta;
}

/**
 * gts_triangles_are_compatible:
 * @t1: a #GtsTriangle.
 * @t2: a #GtsTriangle.
 * @e: a #GtsEdge used by both @t1 and @t2.
 *
 * Checks if @t1 and @t2 have compatible orientations i.e. if @t1 and
 * @t2 can be part of the same surface without conflict in the surface
 * normal orientation.
 *
 * Returns: %TRUE if @t1 and @t2 are compatible, %FALSE otherwise.
 */
gboolean gts_triangles_are_compatible (GtsTriangle * t1, 
				       GtsTriangle * t2,
				       GtsEdge * e)
{
  GtsEdge * e1 = NULL, * e2 = NULL;

  g_return_val_if_fail (t1 != NULL, FALSE);
  g_return_val_if_fail (t2 != NULL, FALSE);
  g_return_val_if_fail (e != NULL, FALSE);

  if (t1->e1 == e) e1 = t1->e2;
  else if (t1->e2 == e) e1 = t1->e3;
  else if (t1->e3 == e) e1 = t1->e1;
  else
    g_assert_not_reached ();
  if (t2->e1 == e) e2 = t2->e2;
  else if (t2->e2 == e) e2 = t2->e3;
  else if (t2->e3 == e) e2 = t2->e1;
  else
    g_assert_not_reached ();
  if (GTS_SEGMENT (e1)->v1 == GTS_SEGMENT (e2)->v1 || 
      GTS_SEGMENT (e1)->v1 == GTS_SEGMENT (e2)->v2 || 
      GTS_SEGMENT (e1)->v2 == GTS_SEGMENT (e2)->v1 || 
      GTS_SEGMENT (e1)->v2 == GTS_SEGMENT (e2)->v2)
    return FALSE;
  return TRUE;
}

/**
 * gts_triangle_area:
 * @t: a #GtsTriangle.
 *
 * Returns: the area of the triangle @t.
 */
gdouble gts_triangle_area (GtsTriangle * t)
{
  gdouble x, y, z;
  
  g_return_val_if_fail (t != NULL, 0.0);
  
  gts_triangle_normal (t, &x, &y, &z);
  
  return sqrt (x*x + y*y + z*z)/2.;
}

/**
 * gts_triangle_perimeter:
 * @t: a #GtsTriangle.
 *
 * Returns: the perimeter of the triangle @t.
 */
gdouble gts_triangle_perimeter (GtsTriangle * t)
{
  GtsVertex * v;

  g_return_val_if_fail (t != NULL, 0.0);

  v = gts_triangle_vertex (t);
  return 
    gts_point_distance (GTS_POINT (GTS_SEGMENT (t->e1)->v1), 
			GTS_POINT (GTS_SEGMENT (t->e1)->v2)) +
    gts_point_distance (GTS_POINT (GTS_SEGMENT (t->e1)->v1), 
			GTS_POINT (v)) +
    gts_point_distance (GTS_POINT (GTS_SEGMENT (t->e1)->v2), 
			GTS_POINT (v));
}

/* perimeter of the equilateral triangle of area unity */
#define GOLDEN_PERIMETER 4.5590141139 

/**
 * gts_triangle_quality:
 * @t: a #GtsTriangle.
 *
 * The quality of a triangle is defined as the ratio of its surface to 
 * its perimeter relative to this same ratio for an equilateral
 * triangle with the same area. The quality is then one for an
 * equilateral triangle and tends to zero for a very stretched triangle.
 *
 * Returns: the quality of the triangle @t.
 */
gdouble gts_triangle_quality (GtsTriangle * t)
{
  gdouble perimeter;

  g_return_val_if_fail (t != NULL, 0.0);

  perimeter = gts_triangle_perimeter (t);
  return perimeter > 0.0 ?
    GOLDEN_PERIMETER*sqrt (gts_triangle_area (t))/perimeter :
    0.0;
}

/**
 * gts_triangle_normal:
 * @t: a #GtsTriangle.
 * @x: the x coordinate of the normal.
 * @y: the y coordinate of the normal.
 * @z: the z coordinate of the normal.
 *
 * Computes the coordinates of the oriented normal of @t as the
 * cross-product of two edges, using the left-hand rule. The normal is
 * not normalized.  If this triangle is part of a closed and oriented
 * surface, the normal points to the outside of the surface.  
 */
void gts_triangle_normal (GtsTriangle * t, 
			  gdouble * x, 
			  gdouble * y, 
			  gdouble * z)
{
  GtsVertex * v1, * v2 = NULL, * v3 = NULL;
  GtsPoint * p1, * p2, * p3;
  gdouble x1, y1, z1, x2, y2, z2;

  g_return_if_fail (t != NULL);

  v1 = GTS_SEGMENT (t->e1)->v1;
  if (GTS_SEGMENT (t->e1)->v1 == GTS_SEGMENT (t->e2)->v1) {
    v2 = GTS_SEGMENT (t->e2)->v2;
    v3 = GTS_SEGMENT (t->e1)->v2;
  }
  else if (GTS_SEGMENT (t->e1)->v2 == GTS_SEGMENT (t->e2)->v2) {
    v2 = GTS_SEGMENT (t->e1)->v2;
    v3 = GTS_SEGMENT (t->e2)->v1;
  }
  else if (GTS_SEGMENT (t->e1)->v1 == GTS_SEGMENT (t->e2)->v2) {
    v2 = GTS_SEGMENT (t->e2)->v1;
    v3 = GTS_SEGMENT (t->e1)->v2;
  }
  else if (GTS_SEGMENT (t->e1)->v2 == GTS_SEGMENT (t->e2)->v1) {
    v2 = GTS_SEGMENT (t->e1)->v2;
    v3 = GTS_SEGMENT (t->e2)->v2;
  }
  else {
    fprintf (stderr, "t: %p t->e1: %p t->e2: %p t->e3: %p t->e1->v1: %p t->e1->v2: %p t->e2->v1: %p t->e2->v2: %p t->e3->v1: %p t->e3->v2: %p\n",
	 t, t->e1, t->e2, 
	 t->e3, GTS_SEGMENT (t->e1)->v1, GTS_SEGMENT (t->e1)->v2, 
	 GTS_SEGMENT (t->e2)->v1, GTS_SEGMENT (t->e2)->v2, 
	 GTS_SEGMENT (t->e3)->v1, GTS_SEGMENT (t->e3)->v2);
    g_assert_not_reached ();
  }

  p1 = GTS_POINT (v1);
  p2 = GTS_POINT (v2);
  p3 = GTS_POINT (v3);

  x1 = p2->x - p1->x;
  y1 = p2->y - p1->y;
  z1 = p2->z - p1->z;

  x2 = p3->x - p1->x;
  y2 = p3->y - p1->y;
  z2 = p3->z - p1->z;

  *x = y1*z2 - z1*y2;
  *y = z1*x2 - x1*z2;
  *z = x1*y2 - y1*x2;
}

/**
 * gts_triangle_orientation:
 * @t: a #GtsTriangle.
 * 
 * Checks for the orientation of the plane (x,y) projection of a
 * triangle. See gts_point_orientation() for details. This function
 * is geometrically robust.
 *
 * Returns: a number depending on the orientation of the vertices of @t.
 */
gdouble gts_triangle_orientation (GtsTriangle * t)
{
  GtsVertex * v1, * v2 = NULL, * v3 = NULL;

  g_return_val_if_fail (t != NULL, 0.0);

  v1 = GTS_SEGMENT (t->e1)->v1;
  if (GTS_SEGMENT (t->e1)->v1 == GTS_SEGMENT (t->e2)->v1) {
    v2 = GTS_SEGMENT (t->e2)->v2;
    v3 = GTS_SEGMENT (t->e1)->v2;
  }
  else if (GTS_SEGMENT (t->e1)->v2 == GTS_SEGMENT (t->e2)->v2) {
    v2 = GTS_SEGMENT (t->e1)->v2;
    v3 = GTS_SEGMENT (t->e2)->v1;
  }
  else if (GTS_SEGMENT (t->e1)->v1 == GTS_SEGMENT (t->e2)->v2) {
    v2 = GTS_SEGMENT (t->e2)->v1;
    v3 = GTS_SEGMENT (t->e1)->v2;
  }
  else if (GTS_SEGMENT (t->e1)->v2 == GTS_SEGMENT (t->e2)->v1) {
    v2 = GTS_SEGMENT (t->e1)->v2;
    v3 = GTS_SEGMENT (t->e2)->v2;
  }
  else
    g_assert_not_reached ();
  return gts_point_orientation (GTS_POINT (v1), 
				GTS_POINT (v2), 
				GTS_POINT (v3));
}

/**
 * gts_triangle_revert:
 * @t: a #GtsTriangle.
 * 
 * Changes the orientation of triangle @t, turning it inside out.
 */
void gts_triangle_revert (GtsTriangle * t)
{
  GtsEdge * e;

  g_return_if_fail (t != NULL);

  e = t->e1;
  t->e1 = t->e2;
  t->e2 = e;
}

/**
 * gts_triangles_from_edges:
 * @edges: a list of #GtsEdge.
 *
 * Builds a list of unique triangles which have one of their edges in @edges.
 * 
 * Returns: the list of triangles.
 */
GSList * gts_triangles_from_edges (GSList * edges)
{
  GHashTable * hash;
  GSList * triangles = NULL, * i;

  hash = g_hash_table_new (NULL, NULL);
  i = edges;
  while (i) {
    GSList * j = GTS_EDGE (i->data)->triangles;
    while (j) {
      GtsTriangle * t = j->data;
      if (g_hash_table_lookup (hash, t) == NULL) {
	triangles = g_slist_prepend (triangles, t);
	g_hash_table_insert (hash, t, i);
      }
      j = j->next;
    }
    i = i->next;
  }
  g_hash_table_destroy (hash);

  return triangles;
}

/**
 * gts_triangle_vertices_edges:
 * @t: a #GtsTriangle.
 * @e: a #GtsEdge belonging to the edges of @t or %NULL.
 * @v1: a #GtsVertex used by @t.
 * @v2: a #GtsVertex used by @t.
 * @v3: a #GtsVertex used by @t.
 * @e1: a #GtsEdge used by @t.
 * @e2: a #GtsEdge used by @t.
 * @e3: a #GtsEdge used by @t.
 *
 * Given @t and @e, returns @v1, @v2, @v3, @e1, @e2 and @e3. @e1
 * has @v1 and @v2 as vertices, @e2 has @v2 and @v3 as vertices
 * and @e3 has @v3 and @v1 as vertices. @v1, @v2 and @v3 respects
 * the orientation of @t. If @e is not NULL, @e1 and @e are
 * identical.
 */
void gts_triangle_vertices_edges (GtsTriangle * t, 
				  GtsEdge * e,
				  GtsVertex ** v1, 
				  GtsVertex ** v2, 
				  GtsVertex ** v3,
				  GtsEdge ** e1,
				  GtsEdge ** e2,
				  GtsEdge ** e3)
{
  GtsEdge * ee1, * ee2;

  g_return_if_fail (t != NULL);
  
  if (e == t->e1 || e == NULL) {
    *e1 = ee1 = t->e1; *e2 = ee2 = t->e2; *e3 = t->e3;    
  }
  else if (e == t->e2) {
    *e1 = ee1 = e; *e2 = ee2 = t->e3; *e3 = t->e1;
  }
  else if (e == t->e3) {
    *e1 = ee1 = e; *e2 = ee2 = t->e1; *e3 = t->e2;
  }
  else {
    g_assert_not_reached ();
    ee1 = ee2 = NULL; /* to avoid complaints from the compiler */
  }
  if (GTS_SEGMENT (ee1)->v2 == GTS_SEGMENT (ee2)->v1) {
    *v1 = GTS_SEGMENT (ee1)->v1; 
    *v2 = GTS_SEGMENT (ee1)->v2; 
    *v3 = GTS_SEGMENT (ee2)->v2;
  }
  else if (GTS_SEGMENT (ee1)->v2 == GTS_SEGMENT (ee2)->v2) {
    *v1 = GTS_SEGMENT (ee1)->v1; 
    *v2 = GTS_SEGMENT (ee1)->v2; 
    *v3 = GTS_SEGMENT (ee2)->v1;
  }
  else if (GTS_SEGMENT (ee1)->v1 == GTS_SEGMENT (ee2)->v1) {
    *v1 = GTS_SEGMENT (ee1)->v2; 
    *v2 = GTS_SEGMENT (ee1)->v1; 
    *v3 = GTS_SEGMENT (ee2)->v2;
  }
  else if (GTS_SEGMENT (ee1)->v1 == GTS_SEGMENT (ee2)->v2) {
    *v1 = GTS_SEGMENT (ee1)->v2; 
    *v2 = GTS_SEGMENT (ee1)->v1; 
    *v3 = GTS_SEGMENT (ee2)->v1;
  }
  else
    g_assert_not_reached ();
}

/* sqrt(3) */
#define SQRT3 1.73205080757

/**
 * gts_triangle_enclosing:
 * @klass: the class of the new triangle.
 * @points: a list of #GtsPoint.
 * @scale: a scaling factor (must be larger than one).
 * 
 * Builds a new triangle (including new vertices and edges) enclosing
 * the plane projection of all the points in @points. This triangle is
 * equilateral and encloses a rectangle defined by the maximum and
 * minimum x and y coordinates of the points. @scale is an homothetic
 * scaling factor. If equal to one, the triangle encloses exactly the
 * enclosing rectangle.
 * 
 * Returns: a new #GtsTriangle.  
 */
GtsTriangle * gts_triangle_enclosing (GtsTriangleClass * klass,
				      GSList * points, gdouble scale)
{
  gdouble xmax, xmin, ymax, ymin;
  gdouble xo, yo, r;
  GtsVertex * v1, * v2, * v3;
  GtsEdge * e1, * e2, * e3;

  if (points == NULL)
    return NULL;
  
  xmax = xmin = GTS_POINT (points->data)->x;
  ymax = ymin = GTS_POINT (points->data)->y;
  points = points->next;
  while (points) {
    GtsPoint * p = points->data;
    if (p->x > xmax) xmax = p->x;
    else if (p->x < xmin) xmin = p->x;
    if (p->y > ymax) ymax = p->y;
    else if (p->y < ymin) ymin = p->y;    
    points = points->next;
  }
  xo = (xmax + xmin)/2.;
  yo = (ymax + ymin)/2.;
  r = scale*sqrt((xmax - xo)*(xmax - xo) + (ymax - yo)*(ymax - yo));
  if (r == 0.0) r = scale;
  v1 = gts_vertex_new (gts_vertex_class (),
		       xo + r*SQRT3, yo - r, 0.0);
  v2 = gts_vertex_new (gts_vertex_class (),
		       xo, yo + 2.*r, 0.0);
  v3 = gts_vertex_new (gts_vertex_class (),
		       xo - r*SQRT3, yo - r, 0.0);
  e1 = gts_edge_new (gts_edge_class (), v1, v2);
  e2 = gts_edge_new (gts_edge_class (), v2, v3);
  e3 = gts_edge_new (gts_edge_class (), v3, v1);
  return gts_triangle_new (gts_triangle_class (), e1, e2, e3);
}

/**
 * gts_triangle_neighbor_number:
 * @t: a #GtsTriangle.
 *
 * Returns: the number of triangles neighbors of @t.
 */
guint gts_triangle_neighbor_number (GtsTriangle * t)
{
  GSList * i;
  guint nn = 0;
  GtsEdge * ee[4], ** e = ee;
  
  g_return_val_if_fail (t != NULL, 0);

  ee[0] = t->e1; ee[1] = t->e2; ee[2] = t->e3; ee[3] = NULL;
  while (*e) {
    i = (*e++)->triangles;
    while (i) {
      GtsTriangle * t1 = i->data;
      if (t1 != t)
	nn++;
      i = i->next;
    }
  }
  return nn;
}

/**
 * gts_triangle_neighbors:
 * @t: a #GtsTriangle.
 *
 * Returns: a list of #GtsTriangle neighbors of @t.
 */
GSList * gts_triangle_neighbors (GtsTriangle * t)
{
  GSList * i, * list = NULL;
  GtsEdge * ee[4], ** e = ee;
  
  g_return_val_if_fail (t != NULL, NULL);

  ee[0] = t->e1; ee[1] = t->e2; ee[2] = t->e3; ee[3] = NULL;
  while (*e) {
    i = (*e++)->triangles;
    while (i) {
      GtsTriangle * t1 = i->data;
      if (t1 != t)
	list = g_slist_prepend (list, t1);
      i = i->next;
    }
  }
  return list;
}

/**
 * gts_triangles_common_edge:
 * @t1: a #GtsTriangle.
 * @t2: a #GtsTriangle.
 *
 * Returns: a #GtsEdge common to both @t1 and @t2 or %NULL if @t1 and @t2
 * do not share any edge.
 */
GtsEdge * gts_triangles_common_edge (GtsTriangle * t1,
				     GtsTriangle * t2)
{
  g_return_val_if_fail (t1 != NULL, NULL);
  g_return_val_if_fail (t2 != NULL, NULL);

  if (t1->e1 == t2->e1 || t1->e1 == t2->e2 || t1->e1 == t2->e3)
    return t1->e1;
  if (t1->e2 == t2->e1 || t1->e2 == t2->e2 || t1->e2 == t2->e3)
    return t1->e2;
  if (t1->e3 == t2->e1 || t1->e3 == t2->e2 || t1->e3 == t2->e3)
    return t1->e3;
  return NULL;
}

/**
 * gts_triangle_is_duplicate:
 * @t: a #GtsTriangle.
 *
 * Returns: a #GtsTriangle different from @t but sharing all its edges 
 * with @t or %NULL if there is none.
 */
GtsTriangle * gts_triangle_is_duplicate (GtsTriangle * t)
{
  GSList * i;
  GtsEdge * e2, * e3;

  g_return_val_if_fail (t != NULL, NULL);

  e2 = t->e2;
  e3 = t->e3;
  i = t->e1->triangles;
  while (i) {
    GtsTriangle * t1 = i->data;
    if (t1 != t && 
	(t1->e1 == e2 || t1->e2 == e2 || t1->e3 == e2) &&
	(t1->e1 == e3 || t1->e2 == e3 || t1->e3 == e3))
      return t1;
    i = i->next;
  }
  
  return NULL;
}

/**
 * gts_triangle_use_edges:
 * @e1: a #GtsEdge.
 * @e2: a #GtsEdge.
 * @e3: a #GtsEdge.
 *
 * Returns: a #GtsTriangle having @e1, @e2 and @e3 as edges or %NULL if @e1,
 * @e2 and @e3 are not part of any triangle.
 */
GtsTriangle * gts_triangle_use_edges (GtsEdge * e1,
				      GtsEdge * e2,
				      GtsEdge * e3)
{
  GSList * i;
  
  g_return_val_if_fail (e1 != NULL, NULL);
  g_return_val_if_fail (e2 != NULL, NULL);
  g_return_val_if_fail (e3 != NULL, NULL);

  i = e1->triangles;
  while (i) {
    GtsTriangle * t = i->data;
    if ((t->e1 == e2 && (t->e2 == e3 || t->e3 == e3)) ||
	(t->e2 == e2 && (t->e1 == e3 || t->e3 == e3)) ||
	(t->e3 == e2 && (t->e1 == e3 || t->e2 == e3)))
      return t;
    i = i->next;
  }
  
  return NULL;
}

/**
 * gts_triangle_is_ok:
 * @t: a #GtsTriangle.
 *
 * Returns: %TRUE if @t is a non-degenerate, non-duplicate triangle,
 * %FALSE otherwise.
 */
gboolean gts_triangle_is_ok (GtsTriangle * t)
{
  g_return_val_if_fail (t != NULL, FALSE);
  g_return_val_if_fail (t->e1 != NULL, FALSE);
  g_return_val_if_fail (t->e2 != NULL, FALSE);
  g_return_val_if_fail (t->e3 != NULL, FALSE);
  g_return_val_if_fail (t->e1 != t->e2 && t->e1 != t->e3 && t->e2 != t->e3, 
			FALSE);
  g_return_val_if_fail (gts_segments_touch (GTS_SEGMENT (t->e1), 
					    GTS_SEGMENT (t->e2)),
			FALSE);
  g_return_val_if_fail (gts_segments_touch (GTS_SEGMENT (t->e1), 
					    GTS_SEGMENT (t->e3)), 
			FALSE);
  g_return_val_if_fail (gts_segments_touch (GTS_SEGMENT (t->e2), 
					    GTS_SEGMENT (t->e3)), 
			FALSE);
  g_return_val_if_fail (GTS_SEGMENT (t->e1)->v1 != GTS_SEGMENT (t->e1)->v2, 
			FALSE);
  g_return_val_if_fail (GTS_SEGMENT (t->e2)->v1 != GTS_SEGMENT (t->e2)->v2, 
			FALSE);
  g_return_val_if_fail (GTS_SEGMENT (t->e3)->v1 != GTS_SEGMENT (t->e3)->v2, 
			FALSE);
  g_return_val_if_fail (GTS_OBJECT (t)->reserved == NULL, FALSE);
  g_return_val_if_fail (!gts_triangle_is_duplicate (t), FALSE);
  return TRUE;
}

/**
 * gts_triangle_vertices:
 * @t: a #GtsTriangle.
 * @v1: a pointer on a #GtsVertex.
 * @v2: a pointer on a #GtsVertex.
 * @v3: a pointer on a #GtsVertex.
 *
 * Fills @v1, @v2 and @v3 with the oriented set of vertices, summits of @t.
 */
void gts_triangle_vertices (GtsTriangle * t,
			    GtsVertex ** v1, GtsVertex ** v2, GtsVertex ** v3)
{
  GtsSegment * e1, * e2;

  g_return_if_fail (t != NULL);
  g_return_if_fail (v1 != NULL && v2 != NULL && v3 != NULL);

  e1 = GTS_SEGMENT (t->e1);
  e2 = GTS_SEGMENT (t->e2);

  if (GTS_SEGMENT (e1)->v2 == GTS_SEGMENT (e2)->v1) {
    *v1 = GTS_SEGMENT (e1)->v1; 
    *v2 = GTS_SEGMENT (e1)->v2; 
    *v3 = GTS_SEGMENT (e2)->v2;
  }
  else if (GTS_SEGMENT (e1)->v2 == GTS_SEGMENT (e2)->v2) {
    *v1 = GTS_SEGMENT (e1)->v1; 
    *v2 = GTS_SEGMENT (e1)->v2; 
    *v3 = GTS_SEGMENT (e2)->v1;
  }
  else if (GTS_SEGMENT (e1)->v1 == GTS_SEGMENT (e2)->v1) {
    *v1 = GTS_SEGMENT (e1)->v2; 
    *v2 = GTS_SEGMENT (e1)->v1; 
    *v3 = GTS_SEGMENT (e2)->v2;
  }
  else {
    *v1 = GTS_SEGMENT (e1)->v2; 
    *v2 = GTS_SEGMENT (e1)->v1; 
    *v3 = GTS_SEGMENT (e2)->v1;
  }
}

/**
 * gts_triangle_circumcircle_center:
 * @t: a #GtsTriangle.
 * @point_class: a #GtsPointClass.
 *
 * Returns: a new #GtsPoint, center of the circumscribing circle of @t or
 * %NULL if the circumscribing circle is not defined.
 */
GtsPoint * gts_triangle_circumcircle_center (GtsTriangle * t,
					     GtsPointClass * point_class)
{
  GtsVertex * v1, * v2, * v3;
  gdouble xa, ya, xb, yb, xc, yc;
  gdouble xd, yd, xe, ye;
  gdouble xad, yad, xae, yae;
  gdouble det;
  
  g_return_val_if_fail (t != NULL, NULL);
  g_return_val_if_fail (point_class != NULL, NULL);

  gts_triangle_vertices (t, &v1, &v2, &v3);

  xa = GTS_POINT (v1)->x; ya = GTS_POINT (v1)->y;
  xb = GTS_POINT (v2)->x; yb = GTS_POINT (v2)->y;
  xc = GTS_POINT (v3)->x; yc = GTS_POINT (v3)->y;
  xd = (xa + xb)/2.; yd = (ya + yb)/2.;
  xe = (xa + xc)/2.; ye = (ya + yc)/2.;
  xad = xd - xa; yad = yd - ya;
  xae = xe - xa; yae = ye - ya;
  det = xad*yae - xae*yad;
  if (det == 0.)
    return NULL;
  return gts_point_new (point_class,
			(yae*yad*(yd - ye) + xad*yae*xd - xae*yad*xe)/det,
			-(xae*xad*(xd - xe) + yad*xae*yd - yae*xad*ye)/det,
			0.);
}

/* square of the maximum area ratio admissible */
#define AREA_RATIO_MAX2 1e8

static gboolean points_are_folded (GtsPoint * A,
				   GtsPoint * B,
				   GtsPoint * C,
				   GtsPoint * D,
				   gdouble max)
{
  GtsVector AB, AC, AD;
  GtsVector n1, n2;
  gdouble nn1, nn2, n1n2;

  gts_vector_init (AB, A, B);
  gts_vector_init (AC, A, C);
  gts_vector_init (AD, A, D);
  gts_vector_cross (n1, AB, AC);
  gts_vector_cross (n2, AD, AB);

  nn1 = gts_vector_scalar (n1, n1);
  nn2 = gts_vector_scalar (n2, n2);
  if (nn1 >= AREA_RATIO_MAX2*nn2 || nn2 >= AREA_RATIO_MAX2*nn1)
    return TRUE;
  n1n2 = gts_vector_scalar (n1, n2);
  if (n1n2 > 0.)
    return FALSE;
  if (n1n2*n1n2/(nn1*nn2) > max)
    return TRUE;
  return FALSE;
}

static GtsVertex * triangle_use_vertices (GtsTriangle * t,
					  GtsVertex * A, 
					  GtsVertex * B)
{
  GtsVertex 
    * v1 = GTS_SEGMENT (t->e1)->v1, 
    * v2 = GTS_SEGMENT (t->e1)->v2, 
    * v3 = gts_triangle_vertex (t);

  if (v1 == A) {
    if (v2 == B)
      return v3;
    g_assert (v3 == B);
    return v2;
  }
  if (v2 == A) {
    if (v1 == B)
      return v3;
    g_assert (v3 == B);
    return v1;
  }
  if (v3 == A) {
    if (v1 == B)
      return v2;
    g_assert (v2 == B);
    return v1;
  }
  g_assert_not_reached ();
  return NULL;
}

/**
 * gts_triangles_are_folded:
 * @triangles: a list of #GtsTriangle.
 * @A: a #GtsVertex.
 * @B: another #GtsVertex.
 * @max: the maximum value of the square of the cosine of the angle between
 * two triangles.
 *
 * Given a list of triangles sharing @A and @B as vertices, checks if any
 * two triangles in the list make an angle larger than a given value defined
 * by @max.
 * 
 * Returns: %TRUE if any pair of triangles in @triangles makes an angle larger 
 * than the maximum value, %FALSE otherwise.
 */
gboolean gts_triangles_are_folded (GSList * triangles,
				   GtsVertex * A, GtsVertex * B,
				   gdouble max)
{
  GSList * i;

  g_return_val_if_fail (A != NULL, TRUE);
  g_return_val_if_fail (B != NULL, TRUE);

  i = triangles;
  while (i) {
    GtsVertex * C = triangle_use_vertices (i->data, A, B);
    GSList * j = i->next;    
    while (j) {
      GtsVertex * D = triangle_use_vertices (j->data, A, B);
      if (points_are_folded (GTS_POINT (A), 
			     GTS_POINT (B), 
			     GTS_POINT (C), 
			     GTS_POINT (D), 
			     max))
	return TRUE;
      j = j->next;
    }
    i = i->next;
  }
  return FALSE;
}

/**
 * gts_triangle_is_stabbed:
 * @t: a #GtsTriangle.
 * @p: a #GtsPoint.
 * @orientation: a pointer or %NULL.
 *
 * Returns: one of the vertices of @t, one of the edges of @t or @t if
 * any of these are stabbed by the ray starting at @p (included) and
 * ending at (@p->x, @p->y, +infty), %NULL otherwise. If the ray is
 * contained in the plane of the triangle %NULL is also returned. If
 * @orientation is not %NULL, it is set to the value of the
 * orientation of @p relative to @t (as given by
 * gts_point_orientation_3d()).  
 */
GtsObject * gts_triangle_is_stabbed (GtsTriangle * t, 
				     GtsPoint * p,
				     gdouble * orientation)
{
  GtsVertex * v1, * v2, * v3, * inverted = NULL;
  GtsEdge * e1, * e2, * e3, * tmp;
  gdouble o, o1, o2, o3;

  g_return_val_if_fail (t != NULL, NULL);
  g_return_val_if_fail (p != NULL, NULL);

  gts_triangle_vertices_edges (t, NULL, &v1, &v2, &v3, &e1, &e2, &e3);
  o = gts_point_orientation (GTS_POINT (v1), GTS_POINT (v2), GTS_POINT (v3));
  if (o == 0.)
    return NULL;
  if (o < 0.) {
    inverted = v1;
    v1 = v2;
    v2 = inverted;
    tmp = e2;
    e2 = e3;
    e3 = tmp;
  }
  o = gts_point_orientation_3d (GTS_POINT (v1),
				GTS_POINT (v2), 
				GTS_POINT (v3), 
				p);
  if (o < 0.)
    return NULL;
  o1 = gts_point_orientation (GTS_POINT (v1), GTS_POINT (v2), p);
  if (o1 < 0.)
    return NULL;
  o2 = gts_point_orientation (GTS_POINT (v2), GTS_POINT (v3), p);
  if (o2 < 0.)
    return NULL;
  o3 = gts_point_orientation (GTS_POINT (v3), GTS_POINT (v1), p);
  if (o3 < 0.)
    return NULL;  
  if (orientation) *orientation = inverted ? -o : o;
  if (o1 == 0.) {
    if (o2 == 0.)
      return GTS_OBJECT (v2);
    if (o3 == 0.)
      return GTS_OBJECT (v1);
    return GTS_OBJECT (e1);
  }
  if (o2 == 0.) {
    if (o3 == 0.)
      return GTS_OBJECT (v3);
    return GTS_OBJECT (e2);
  }
  if (o3 == 0.)
    return GTS_OBJECT (e3);
  return GTS_OBJECT (t);
}

