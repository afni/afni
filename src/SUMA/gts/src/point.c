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
#include <stdlib.h>
#include "gts.h"
#include "gts-private.h"
#ifdef USE_ROBUST_PREDICATES
#include "predicates.h"
#endif /* USE_ROBUST_PREDICATES */

static void point_read (GtsObject ** o, GtsFile * f)
{
  GtsPoint * p = GTS_POINT (*o);

  if (GTS_POINT_CLASS ((*o)->klass)->binary) {
    if (gts_file_read (f, &(p->x), sizeof (gdouble), 1) != 1) {
      gts_file_error (f, "expecting a binary number (x coordinate)");
      return;
    }
    if (gts_file_read (f, &(p->y), sizeof (gdouble), 1) != 1) {
      gts_file_error (f, "expecting a binary number (y coordinate)");
      return;
    }
    if (gts_file_read (f, &(p->z), sizeof (gdouble), 1) != 1) {
      gts_file_error (f, "expecting a binary number (z coordinate)");
      return;
    }
  }
  else {
    if (f->type != GTS_INT && f->type != GTS_FLOAT) {
      gts_file_error (f, "expecting a number (x coordinate)");
      return;
    }
    p->x = atof (f->token->str);
    
    gts_file_next_token (f);
    if (f->type != GTS_INT && f->type != GTS_FLOAT) {
      gts_file_error (f, "expecting a number (y coordinate)");
      return;
    }
    p->y = atof (f->token->str);
    
    gts_file_next_token (f);
    if (f->type != GTS_INT && f->type != GTS_FLOAT) {
      gts_file_error (f, "expecting a number (z coordinate)");
      return;
    }
    p->z = atof (f->token->str);
    
    gts_file_next_token (f);
  }
}

static void point_write (GtsObject * o, FILE * fptr)
{
  GtsPoint * p = GTS_POINT (o);

  if (GTS_POINT_CLASS ((o)->klass)->binary) {
    fwrite (&(p->x), sizeof (gdouble), 1, fptr);
    fwrite (&(p->y), sizeof (gdouble), 1, fptr);
    fwrite (&(p->z), sizeof (gdouble), 1, fptr);
  }
  else
    fprintf (fptr, "%.10g %.10g %.10g", p->x, p->y, p->z);
}

static void point_class_init (GtsObjectClass * klass)
{
  klass->read = point_read;
  klass->write = point_write;
}

/**
 * gts_point_class:
 *
 * Returns: the #GtsPointClass.
 */
GtsPointClass * gts_point_class (void)
{
  static GtsPointClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo point_info = {
      "GtsPoint",
      sizeof (GtsPoint),
      sizeof (GtsPointClass),
      (GtsObjectClassInitFunc) point_class_init,
      (GtsObjectInitFunc) NULL,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (), 
				  &point_info);
  }

  return klass;
}

/**
 * gts_point_new:
 * @klass: a #GtsPointClass.
 * @x: the x-coordinate.
 * @y: the y-coordinate.
 * @z: the z-coordinate.
 *
 * Returns: a new #GtsPoint.
 */
GtsPoint * gts_point_new (GtsPointClass * klass,
			  gdouble x, gdouble y, gdouble z)
{
  GtsPoint * p;
  
  p = GTS_POINT (gts_object_new (GTS_OBJECT_CLASS (klass)));
  p->x = x;
  p->y = y;
  p->z = z;

  return p;
}

/**
 * gts_point_set:
 * @p: a #GtsPoint.
 * @x: the x-coordinate.
 * @y: the y-coordinate.
 * @z: the z-coordinate.
 *
 * Sets the coordinates of @p.
 */
void gts_point_set (GtsPoint * p, gdouble x, gdouble y, gdouble z)
{
  g_return_if_fail (p != NULL);

  p->x = x;
  p->y = y;
  p->z = z;
}

/**
 * gts_point_distance:
 * @p1: a #GtsPoint.
 * @p2: another #GtsPoint.
 *
 * Returns: the Euclidean distance between @p1 and @p2.
 */
gdouble gts_point_distance (GtsPoint * p1, GtsPoint * p2)
{
  g_return_val_if_fail (p1 != NULL && p2 != NULL, 0.0);
  
  return sqrt ((p1->x - p2->x)*(p1->x - p2->x) + 
	       (p1->y - p2->y)*(p1->y - p2->y) + 
	       (p1->z - p2->z)*(p1->z - p2->z));
}

/**
 * gts_point_distance2:
 * @p1: a #GtsPoint.
 * @p2: another #GtsPoint.
 *
 * Returns: the square of the Euclidean distance between @p1 and @p2.
 */
gdouble gts_point_distance2 (GtsPoint * p1, GtsPoint * p2)
{
  g_return_val_if_fail (p1 != NULL && p2 != NULL, 0.0);
  
  return
    (p1->x - p2->x)*(p1->x - p2->x) +
    (p1->y - p2->y)*(p1->y - p2->y) + 
    (p1->z - p2->z)*(p1->z - p2->z);
}

/**
 * gts_point_orientation_3d:
 * @p1: a #GtsPoint.
 * @p2: a #GtsPoint.
 * @p3: a #GtsPoint.
 * @p4: a #GtsPoint.
 *
 * Checks if @p4 lies above, below or on the plane passing through the
 * points @p1, @p2 and @p3. Below is defined so that @p1, @p2 and @p3
 * appear in counterclockwise order when viewed from above the
 * plane. The returned value is an approximation of six times the
 * signed volume of the tetrahedron defined by the four points. This
 * function uses adaptive floating point arithmetic and is
 * consequently geometrically robust.
 *
 * Returns: a positive value if @p4 lies below, a negative value if
 * @p4 lies above the plane, zero if the four points are coplanar.  
 */
gdouble gts_point_orientation_3d (GtsPoint * p1,
				  GtsPoint * p2,
				  GtsPoint * p3,
				  GtsPoint * p4)
{
  g_return_val_if_fail (p1 != NULL && p2 != NULL && 
			p3 != NULL && p4 != NULL, 0.0);
#ifdef USE_ROBUST_PREDICATES
  return orient3d ((gdouble *) &p1->x, 
		   (gdouble *) &p2->x, 
		   (gdouble *) &p3->x, 
		   (gdouble *) &p4->x);
#else
  {
    gdouble adx, bdx, cdx;
    gdouble ady, bdy, cdy;
    gdouble adz, bdz, cdz;
    
    adx = p1->x - p4->x;
    bdx = p2->x - p4->x;
    cdx = p3->x - p4->x;
    ady = p1->y - p4->y;
    bdy = p2->y - p4->y;
    cdy = p3->y - p4->y;
    adz = p1->z - p4->z;
    bdz = p2->z - p4->z;
    cdz = p3->z - p4->z;
    
    return adx * (bdy * cdz - bdz * cdy)
      + bdx * (cdy * adz - cdz * ady)
      + cdx * (ady * bdz - adz * bdy);
  }
#endif /* USE_ROBUST_PREDICATES */
}

/**
 * gts_point_is_in_triangle:
 * @p: a #GtsPoint.
 * @t: a #GtsTriangle.
 *
 * Tests if the planar projection (x, y) of @p is inside, outside or
 * on the boundary of the planar projection of @t.  This function is
 * geometrically robust.
 * 
 * Returns: %GTS_IN if @p is inside @t, %GTS_ON if @p is on the boundary of
 * @t, %GTS_OUT otherwise.  
 */
GtsIntersect gts_point_is_in_triangle (GtsPoint * p, GtsTriangle * t)
{
  GtsVertex * v1, * v2, * v3;
  gdouble d1, d2, d3;

  g_return_val_if_fail (p != NULL && t != NULL, FALSE);

  gts_triangle_vertices (t, &v1, &v2, &v3);

  d1 = gts_point_orientation (GTS_POINT (v1), GTS_POINT (v2), p);
  if (d1 < 0.0)
    return GTS_OUT;
  d2 = gts_point_orientation (GTS_POINT (v2), GTS_POINT (v3), p);
  if (d2 < 0.0)
    return GTS_OUT;
  d3 = gts_point_orientation (GTS_POINT (v3), GTS_POINT (v1), p);
  if (d3 < 0.0)
    return GTS_OUT;
  if (d1 == 0.0 || d2 == 0.0 || d3 == 0.0)
    return GTS_ON;
  return GTS_IN;
}

/**
 * gts_point_in_triangle_circle:
 * @p: a #GtsPoint.
 * @t: a #GtsTriangle.
 *
 * Tests if the planar projection (x, y) of @p is inside or outside
 * the circumcircle of the planar projection of @t. This function is
 * geometrically robust.
 * 
 * Returns: a positive number if @p lies inside,
 * a negative number if @p lies outside and zero if @p lies on 
 * the circumcircle of @t.  
 */
gdouble gts_point_in_triangle_circle (GtsPoint * p, GtsTriangle * t)
{
  GtsPoint * p1, * p2, * p3;

  g_return_val_if_fail (p != NULL && t != NULL, 0.0);

  gts_triangle_vertices (t, 
			 (GtsVertex **) &p1, 
			 (GtsVertex **) &p2, 
			 (GtsVertex **) &p3);

#ifdef USE_ROBUST_PREDICATES
  return incircle ((gdouble *) &p1->x, 
		   (gdouble *) &p2->x, 
		   (gdouble *) &p3->x, 
		   (gdouble *) &p->x);
#else
  {
    gdouble adx, bdx, cdx, ady, bdy, cdy;
    gdouble bdxcdy, cdxbdy, cdxady, adxcdy, adxbdy, bdxady;
    gdouble alift, blift, clift;
    gdouble det;
    
    adx = p1->x - p->x;
    bdx = p2->x - p->x;
    cdx = p3->x - p->x;
    ady = p1->y - p->y;
    bdy = p2->y - p->y;
    cdy = p3->y - p->y;
    
    bdxcdy = bdx*cdy;
    cdxbdy = cdx*bdy;
    alift = adx*adx + ady*ady;
    
    cdxady = cdx*ady;
    adxcdy = adx*cdy;
    blift = bdx*bdx + bdy*bdy;
    
    adxbdy = adx*bdy;
    bdxady = bdx*ady;
    clift = cdx*cdx + cdy*cdy;
    
    det = alift*(bdxcdy - cdxbdy)
      + blift*(cdxady - adxcdy)
      + clift*(adxbdy - bdxady);
    return det;
  }
#endif /* USE_ROBUST_PREDICATES */
}

/**
 * gts_point_in_circle:
 * @p: a #GtsPoint.
 * @p1: a #GtsPoint.
 * @p2: a #GtsPoint.
 * @p3: a #GtsPoint.
 *
 * Tests if the planar projection (x, y) of @p is inside or outside the
 * circle defined by the planar projection of @p1, @p2 and @p3.
 * 
 * Returns: a positive number if @p lies inside,
 * a negative number if @p lies outside and zero if @p lies on 
 * the circle.
 */
gdouble gts_point_in_circle (GtsPoint * p, 
			     GtsPoint * p1, GtsPoint * p2, GtsPoint * p3)
{
  g_return_val_if_fail (p != NULL && p1 != NULL && p2 != NULL && p3 != NULL, 
			0.0);

#ifdef USE_ROBUST_PREDICATES
  return incircle ((gdouble *) &p1->x, 
		   (gdouble *) &p2->x, 
		   (gdouble *) &p3->x, 
		   (gdouble *) &p->x);
#else
  {
    gdouble adx, bdx, cdx, ady, bdy, cdy;
    gdouble bdxcdy, cdxbdy, cdxady, adxcdy, adxbdy, bdxady;
    gdouble alift, blift, clift;
    gdouble det;
    
    adx = p1->x - p->x;
    bdx = p2->x - p->x;
    cdx = p3->x - p->x;
    ady = p1->y - p->y;
    bdy = p2->y - p->y;
    cdy = p3->y - p->y;
    
    bdxcdy = bdx*cdy;
    cdxbdy = cdx*bdy;
    alift = adx*adx + ady*ady;
    
    cdxady = cdx*ady;
    adxcdy = adx*cdy;
    blift = bdx*bdx + bdy*bdy;
    
    adxbdy = adx*bdy;
    bdxady = bdx*ady;
    clift = cdx*cdx + cdy*cdy;
    
    det = alift*(bdxcdy - cdxbdy)
      + blift*(cdxady - adxcdy)
      + clift*(adxbdy - bdxady);
    return det;
  }
#endif /* USE_ROBUST_PREDICATES */
}

/**
 * gts_point_segment_distance2:
 * @p: a #GtsPoint.
 * @s: a #GtsSegment.
 *
 * Returns: the square of the minimun Euclidean distance between @p and @s.
 */
gdouble gts_point_segment_distance2 (GtsPoint * p, GtsSegment * s)
{
  gdouble t, ns2, x, y, z;
  GtsPoint * p1, * p2;

  g_return_val_if_fail (p != NULL, 0.0);
  g_return_val_if_fail (s != NULL, 0.0);

  p1 = GTS_POINT (s->v1);
  p2 = GTS_POINT (s->v2);
  ns2 = gts_point_distance2 (p1, p2);
  if (ns2 == 0.0)
    return gts_point_distance2 (p, p1);
  t = ((p2->x - p1->x)*(p->x - p1->x) + 
       (p2->y - p1->y)*(p->y - p1->y) +
       (p2->z - p1->z)*(p->z - p1->z))/ns2;
  if (t > 1.0)
    return gts_point_distance2 (p, p2);
  if (t < 0.0)
    return gts_point_distance2 (p, p1);
  x = (1. - t)*p1->x + t*p2->x - p->x;
  y = (1. - t)*p1->y + t*p2->y - p->y;
  z = (1. - t)*p1->z + t*p2->z - p->z;
  return x*x + y*y + z*z;
}

/**
 * gts_point_segment_distance:
 * @p: a #GtsPoint.
 * @s: a #GtsSegment.
 *
 * Returns: the minimun Euclidean distance between @p and @s.
 */
gdouble gts_point_segment_distance (GtsPoint * p, GtsSegment * s)
{
  g_return_val_if_fail (p != NULL, 0.0);
  g_return_val_if_fail (s != NULL, 0.0);

  return sqrt (gts_point_segment_distance2 (p, s));
}

/**
 * gts_point_segment_closest:
 * @p: a #GtsPoint.
 * @s: a #GtsSegment.
 * @closest: a #GtsPoint.
 *
 * Set the coordinates of @closest to the coordinates of the point belonging
 * to @s closest to @p.
 */
void gts_point_segment_closest (GtsPoint * p, 
				GtsSegment * s,
				GtsPoint * closest)
{
  gdouble t, ns2;
  GtsPoint * p1, * p2;

  g_return_if_fail (p != NULL);
  g_return_if_fail (s != NULL);
  g_return_if_fail (closest != NULL);

  p1 = GTS_POINT (s->v1);
  p2 = GTS_POINT (s->v2);
  ns2 = gts_point_distance2 (p1, p2);

  if (ns2 == 0.0) {
    gts_point_set (closest, p1->x, p1->y, p1->z);
    return;
  }

  t = ((p2->x - p1->x)*(p->x - p1->x) + 
       (p2->y - p1->y)*(p->y - p1->y) +
       (p2->z - p1->z)*(p->z - p1->z))/ns2;

  if (t > 1.0)
    gts_point_set (closest, p2->x, p2->y, p2->z);
  else if (t < 0.0)
    gts_point_set (closest, p1->x, p1->y, p1->z);
  else
    gts_point_set (closest,
		   (1. - t)*p1->x + t*p2->x,
		   (1. - t)*p1->y + t*p2->y,
		   (1. - t)*p1->z + t*p2->z);
}

/**
 * gts_point_triangle_distance2:
 * @p: a #GtsPoint.
 * @t: a #GtsTriangle.
 *
 * Returns: the square of the minimun Euclidean distance between @p and @t.
 */
gdouble gts_point_triangle_distance2 (GtsPoint * p, GtsTriangle * t)
{
  GtsPoint * p1, * p2, * p3;
  GtsEdge * e1, * e2, * e3;
  GtsVector p1p2, p1p3, pp1;
  gdouble A, B, C, D, E, det;
  gdouble t1, t2;
  gdouble x, y, z;

  g_return_val_if_fail (p != NULL, 0.0);
  g_return_val_if_fail (t != NULL, 0.0);

  gts_triangle_vertices_edges (t, NULL, 
			       (GtsVertex **) &p1, 
			       (GtsVertex **) &p2, 
			       (GtsVertex **) &p3, 
			       &e1, &e2, &e3);

  gts_vector_init (p1p2, p1, p2);
  gts_vector_init (p1p3, p1, p3);
  gts_vector_init (pp1, p, p1);

  B = gts_vector_scalar (p1p3, p1p2);
  E = gts_vector_scalar (p1p2, p1p2);
  C = gts_vector_scalar (p1p3, p1p3);
  
  det = B*B - E*C;
  if (det == 0.) { /* p1p2 and p1p3 are colinear */
    gdouble d1 = gts_point_segment_distance2 (p, GTS_SEGMENT (e1));
    gdouble d2 = gts_point_segment_distance2 (p, GTS_SEGMENT (e3));
    if (d1 < d2)
      return d1;
    return d2;
  }

  A = gts_vector_scalar (p1p3, pp1);
  D = gts_vector_scalar (p1p2, pp1);
  
  t1 = (D*C - A*B)/det;
  t2 = (A*E - D*B)/det;

  if (t1 < 0.)
    return gts_point_segment_distance2 (p, GTS_SEGMENT (e3));
  if (t2 < 0.)
    return gts_point_segment_distance2 (p, GTS_SEGMENT (e1));
  if (t1 + t2 > 1.)
    return gts_point_segment_distance2 (p, GTS_SEGMENT (e2));

  x = pp1[0] + t1*p1p2[0] + t2*p1p3[0];
  y = pp1[1] + t1*p1p2[1] + t2*p1p3[1];
  z = pp1[2] + t1*p1p2[2] + t2*p1p3[2];

  return x*x + y*y + z*z;
}

/**
 * gts_point_triangle_distance:
 * @p: a #GtsPoint.
 * @t: a #GtsTriangle.
 *
 * Returns: the minimun Euclidean distance between @p and @t.
 */
gdouble gts_point_triangle_distance (GtsPoint * p, GtsTriangle * t)
{
  g_return_val_if_fail (p != NULL, 0.0);
  g_return_val_if_fail (t != NULL, 0.0);

  return sqrt (gts_point_triangle_distance2 (p, t));
}

/**
 * gts_point_triangle_closest:
 * @p: a #GtsPoint.
 * @t: a #GtsTriangle.
 * @closest: a #GtsPoint.
 *
 * Set the coordinates of @closest to those of the point belonging to @t and 
 * closest to @p.
 */
void gts_point_triangle_closest (GtsPoint * p, 
				 GtsTriangle * t, 
				 GtsPoint * closest)
{
  GtsPoint * p1, * p2, * p3;
  GtsEdge * e1, * e2, * e3;
  GtsVector p1p2, p1p3, pp1;
  gdouble A, B, C, D, E, det;
  gdouble t1, t2;

  g_return_if_fail (p != NULL);
  g_return_if_fail (t != NULL);
  g_return_if_fail (closest != NULL);

  gts_triangle_vertices_edges (t, NULL, 
			       (GtsVertex **) &p1, 
			       (GtsVertex **) &p2, 
			       (GtsVertex **) &p3, 
			       &e1, &e2, &e3);

  gts_vector_init (p1p2, p1, p2);
  gts_vector_init (p1p3, p1, p3);
  gts_vector_init (pp1, p, p1);

  B = gts_vector_scalar (p1p3, p1p2);
  E = gts_vector_scalar (p1p2, p1p2);
  C = gts_vector_scalar (p1p3, p1p3);
  
  det = B*B - E*C;
  if (det == 0.) { /* p1p2 and p1p3 are colinear */
    GtsPoint * cp = 
      GTS_POINT (gts_object_new (GTS_OBJECT_CLASS (gts_point_class ())));
    gts_point_segment_closest (p, GTS_SEGMENT (e1), cp);
    gts_point_segment_closest (p, GTS_SEGMENT (e3), closest);

    if (gts_point_distance2 (cp, p) < gts_point_distance2 (closest, p))
      gts_point_set (closest, cp->x, cp->y, cp->z);
    gts_object_destroy (GTS_OBJECT (cp));
    return;
  }

  A = gts_vector_scalar (p1p3, pp1);
  D = gts_vector_scalar (p1p2, pp1);
  
  t1 = (D*C - A*B)/det;
  t2 = (A*E - D*B)/det;

  if (t1 < 0.)
    gts_point_segment_closest (p, GTS_SEGMENT (e3), closest);
  else if (t2 < 0.)
    gts_point_segment_closest (p, GTS_SEGMENT (e1), closest);
  else if (t1 + t2 > 1.)
    gts_point_segment_closest (p, GTS_SEGMENT (e2), closest);
  else
    gts_point_set (closest, 
		   p1->x + t1*p1p2[0] + t2*p1p3[0],
		   p1->y + t1*p1p2[1] + t2*p1p3[1],
		   p1->z + t1*p1p2[2] + t2*p1p3[2]);
}

/**
 * gts_segment_triangle_intersection:
 * @s: a #GtsSegment.
 * @t: a #GtsTriangle.
 * @boundary: if %TRUE, the boundary of @t is taken into account.
 * @klass: a #GtsPointClass to be used for the new point.
 *
 * Checks if @s intersects @t. If this is the case, creates a new
 * point pi intersection of @s with @t.
 *
 * This function is geometrically robust in the sense that it will not
 * return a point if @s and @t do not intersect and will return a
 * point if @s and @t do intersect. However, the point coordinates are
 * subject to round-off errors.
 *
 * Note that this function will not return any point if @s is contained in
 * the plane defined by @t.
 * 
 * Returns: a summit of @t (if @boundary is set to %TRUE), one of the endpoints
 * of @s or a new #GtsPoint, intersection of @s with @t or %NULL if @s 
 * and @t don't intersect.  
 */
GtsPoint * gts_segment_triangle_intersection (GtsSegment * s,
					      GtsTriangle * t,
					      gboolean boundary,
					      GtsPointClass * klass)
{
  GtsPoint * A, * B, * C, * D, * E, * I;
  gdouble ABCE, ABCD, ADCE, ABDE, BCDE;
  gdouble c;

  g_return_val_if_fail (s != NULL, NULL);
  g_return_val_if_fail (t != NULL, NULL);
  g_return_val_if_fail (klass != NULL, NULL);

  A = GTS_POINT (GTS_SEGMENT (t->e1)->v1);
  B = GTS_POINT (GTS_SEGMENT (t->e1)->v2);
  C = GTS_POINT (gts_triangle_vertex (t));
  D = GTS_POINT (s->v1); 
  E = GTS_POINT (s->v2);

  ABCE = gts_point_orientation_3d (A, B, C, E);
  ABCD = gts_point_orientation_3d (A, B, C, D);
  if (ABCE < 0.0 || ABCD > 0.0) {
    GtsPoint * tmpp;
    gdouble tmp;
    tmpp = E; E = D; D = tmpp;
    tmp = ABCE; ABCE = ABCD; ABCD = tmp;
  }
  if (ABCE < 0.0 || ABCD > 0.0)
    return NULL;
  ADCE = gts_point_orientation_3d (A, D, C, E);
  if ((boundary && ADCE < 0.) || (!boundary && ADCE <= 0.))
    return NULL;
  ABDE = gts_point_orientation_3d (A, B, D, E);
  if ((boundary && ABDE < 0.) || (!boundary && ABDE <= 0.))
    return NULL;
  BCDE = gts_point_orientation_3d (B, C, D, E);
  if ((boundary && BCDE < 0.) || (!boundary && BCDE <= 0.))
    return NULL;
  if (ABCE == 0.0) {
    if (ABCD == 0.0)
      /* s is contained in the plane defined by t*/
      return NULL;
    return E;
  }
  if (ABCD == 0.0)
    return D;
  if (boundary) { /* corners of @t */
    if (ABDE == 0.) {
      if (ADCE == 0.)
	return A;
      if (BCDE == 0.)
	return B;
    }
    else if (BCDE == 0. && ADCE == 0.)
      return C;
  }
  c = ABCE/(ABCE - ABCD);
  I = GTS_POINT (gts_object_new (GTS_OBJECT_CLASS (klass)));
  gts_point_set (I,
		 E->x + c*(D->x - E->x),
		 E->y + c*(D->y - E->y),
		 E->z + c*(D->z - E->z));
  return I;
}

/**
 * gts_point_transform:
 * @p: a #GtsPoint.
 * @m: the #GtsMatrix representing the transformation to 
 * apply to the coordinates of @p.
 *
 * Transform the coordinates of @p according to @m. (p[] becomes m[][].p[]).
 */
void gts_point_transform (GtsPoint * p, GtsMatrix * m)
{
  gdouble x, y, z;
  g_return_if_fail (p != NULL && m != NULL);
  x = m[0][0]*p->x + m[0][1]*p->y + m[0][2]*p->z + m[0][3];
  y = m[1][0]*p->x + m[1][1]*p->y + m[1][2]*p->z + m[1][3];
  z = m[2][0]*p->x + m[2][1]*p->y + m[2][2]*p->z + m[2][3];
  p->x = x; p->y = y; p->z = z;
}

/**
 * gts_point_orientation:
 * @p1: a #GtsPoint.
 * @p2: a #GtsPoint.
 * @p3: a #GtsPoint.
 *
 * Checks for orientation of the projection of three points on the
 * (x,y) plane. The result is also an approximation of twice the
 * signed area of the triangle defined by the three points. This
 * function uses adaptive floating point arithmetic and is
 * consequently geometrically robust.
 *
 * Returns: a positive value if @p1, @p2 and @p3 appear in
 * counterclockwise order, a negative value if they appear in
 * clockwise order and zero if they are colinear.  
 */
gdouble gts_point_orientation (GtsPoint * p1, GtsPoint * p2, GtsPoint * p3)
{
#ifdef USE_ROBUST_PREDICATES
  g_return_val_if_fail (p1 != NULL && p2 != NULL && p3 != NULL, 0.0);

  return orient2d ((gdouble *) &p1->x, 
		   (gdouble *) &p2->x, 
		   (gdouble *) &p3->x);
#else
  gdouble acx, bcx, acy, bcy;
  
  g_return_val_if_fail (p1 != NULL && p2 != NULL && p3 != NULL, 0.0);

  acx = p1->x - p3->x;
  bcx = p2->x - p3->x;
  acy = p1->y - p3->y;
  bcy = p2->y - p3->y;
  return acx * bcy - acy * bcx;
#endif /* USE_ROBUST_PREDICATES */
}

static gboolean ray_intersects_triangle (GtsPoint * D, GtsPoint * E,
					 GtsTriangle * t)
{
  GtsPoint * A, * B, * C;
  gint ABCE, ABCD, ADCE, ABDE, BCDE;

  gts_triangle_vertices (t, (GtsVertex **) &A, 
			 (GtsVertex **) &B, 
			 (GtsVertex **) &C);

  ABCE = gts_point_orientation_3d_sos (A, B, C, E);
  ABCD = gts_point_orientation_3d_sos (A, B, C, D);
  if (ABCE < 0 || ABCD > 0) {
    GtsPoint * tmpp;
    gint tmp;

    tmpp = E; E = D; D = tmpp;
    tmp = ABCE; ABCE = ABCD; ABCD = tmp;
  }
  if (ABCE < 0 || ABCD > 0)
    return FALSE;
  ADCE = gts_point_orientation_3d_sos (A, D, C, E);
  if (ADCE < 0)
    return FALSE;
  ABDE = gts_point_orientation_3d_sos (A, B, D, E);
  if (ABDE < 0)
    return FALSE;
  BCDE = gts_point_orientation_3d_sos (B, C, D, E);
  if (BCDE < 0)
    return FALSE;
  return TRUE;
}

/** 
 * gts_point_is_inside_surface: 
 * @p: a #GtsPoint.  
 * @tree: a bounding box tree of the faces of a closed, orientable
 * surface (see gts_bb_tree_surface()).
 * @is_open: %TRUE if the surface defined by @tree is "open" i.e. its volume 
 * is negative, %FALSE otherwise.
 *
 * Returns: %TRUE if @p is inside the surface defined by @tree, %FALSE
 * otherwise.  
 */
gboolean gts_point_is_inside_surface (GtsPoint * p, 
				      GNode * tree,
				      gboolean is_open)
{
  GSList * list, * i;
  guint nc = 0;
  GtsPoint * p1;
  GtsBBox * bb;

  g_return_val_if_fail (p != NULL, FALSE);
  g_return_val_if_fail (tree != NULL, FALSE);

  bb = tree->data;
  p1 = gts_point_new (gts_point_class (), p->x, p->y, 
		      bb->z2 + fabs (bb->z2)/10.);
  i = list = gts_bb_tree_stabbed (tree, p);
  while (i) {
    GtsTriangle * t = GTS_TRIANGLE (GTS_BBOX (i->data)->bounded);

    if (ray_intersects_triangle (p, p1, t))
      nc++;
    i = i->next;
  }
  g_slist_free (list);
  gts_object_destroy (GTS_OBJECT (p1));

  return is_open ? (nc % 2 == 0) : (nc % 2 != 0);
}

#define SIGN(x) ((x) > 0. ? 1 : -1)
#define ORIENT1D(a,b) ((a) > (b) ? 1 : (a) < (b) ? -1 : 0)

static gint sortp (gpointer * p, guint n)
{
  gint sign = 1;
  guint i, j;

  for (i = 0; i < n - 1; i++)
    for (j = 0; j < n - 1 - i; j++)
      if (GPOINTER_TO_UINT (p[j+1]) < GPOINTER_TO_UINT (p[j])) {
	gpointer tmp = p[j];

	p[j] = p[j+1];
	p[j+1] = tmp;
	sign = - sign;
      }
  return sign;
}

/**
 * gts_point_orientation_3d_sos:
 * @p1: a #GtsPoint.
 * @p2: a #GtsPoint.
 * @p3: a #GtsPoint.
 * @p4: a #GtsPoint.
 *
 * Checks if @p4 lies above or below the plane passing through the
 * points @p1, @p2 and @p3. Below is defined so that @p1, @p2 and @p3
 * appear in counterclockwise order when viewed from above the
 * plane. This function uses adaptive floating point arithmetic and is
 * consequently geometrically robust.
 *
 * Simulation of Simplicity (SoS) is used to break ties when the
 * orientation is degenerate (i.e. @p4 lies on the plane defined by
 * @p1, @p2 and @p3).
 *
 * Returns: +1 if @p4 lies below, -1 if @p4 lies above the plane.  
 */
gint gts_point_orientation_3d_sos (GtsPoint * p1,
				   GtsPoint * p2,
				   GtsPoint * p3,
				   GtsPoint * p4)
{
#ifndef USE_ROBUST_PREDICATES
  g_assert_not_reached ();
  return 0;
#else /* USE_ROBUST_PREDICATES */
  gdouble o;

  g_return_val_if_fail (p1 != NULL && p2 != NULL && 
			p3 != NULL && p4 != NULL, 0);

  o = orient3d ((gdouble *) &p1->x, 
		(gdouble *) &p2->x, 
		(gdouble *) &p3->x, 
		(gdouble *) &p4->x);
  if (o != 0.)
    return SIGN (o);
  else {
    GtsPoint * p[4];
    gdouble a[2], b[2], c[2];
    gint sign;

    p[0] = p1; p[1] = p2; p[2] = p3; p[3] = p4;
    sign = sortp ((gpointer *) p, 4);
    
    /* epsilon^1/8 */
    a[0] = p[1]->x; a[1] = p[1]->y;
    b[0] = p[2]->x; b[1] = p[2]->y;
    c[0] = p[3]->x; c[1] = p[3]->y;
    o = orient2d (a, b, c);
    if (o != 0.)
      return SIGN (o)*sign;
    
    /* epsilon^1/4 */
    a[0] = p[1]->x; a[1] = p[1]->z;
    b[0] = p[2]->x; b[1] = p[2]->z;
    c[0] = p[3]->x; c[1] = p[3]->z;
    o = orient2d (a, b, c);
    if (o != 0.)
      return - SIGN (o)*sign;
    
    /* epsilon^1/2 */
    a[0] = p[1]->y; a[1] = p[1]->z;
    b[0] = p[2]->y; b[1] = p[2]->z;
    c[0] = p[3]->y; c[1] = p[3]->z;
    o = orient2d (a, b, c);
    if (o != 0.)
      return SIGN (o)*sign;
    
    /* epsilon */
    a[0] = p[0]->x; a[1] = p[0]->y;
    b[0] = p[2]->x; b[1] = p[2]->y;
    c[0] = p[3]->x; c[1] = p[3]->y;
    o = orient2d (a, b, c);
    if (o != 0.)
      return - SIGN (o)*sign;
    
    /* epsilon^5/4 */
    o = ORIENT1D (p[2]->x, p[3]->x);
    if (o != 0.)
      return SIGN (o)*sign;
    
    /* epsilon^3/2 */
    o = ORIENT1D (p[2]->y, p[3]->y);
    if (o != 0.)
      return - SIGN (o)*sign;
    
    /* epsilon^2 */
    a[0] = p[0]->x; a[1] = p[0]->z;
    b[0] = p[2]->x; b[1] = p[2]->z;
    c[0] = p[3]->x; c[1] = p[3]->z;
    o = orient2d (a, b, c);
    if (o != 0.)
      return SIGN (o)*sign;

    /* epsilon^5/2 */
    o = ORIENT1D (p[2]->z, p[3]->z);
    if (o != 0.)
      return SIGN (o)*sign;
    
    /* epsilon^4 */
    a[0] = p[0]->y; a[1] = p[0]->z;
    b[0] = p[2]->y; b[1] = p[2]->z;
    c[0] = p[3]->y; c[1] = p[3]->z;
    o = orient2d (a, b, c);
    if (o != 0.)
      return - SIGN (o)*sign;

    /* epsilon^8 */
    a[0] = p[0]->x; a[1] = p[0]->y;
    b[0] = p[1]->x; b[1] = p[1]->y;
    c[0] = p[3]->x; c[1] = p[3]->y;
    o = orient2d (a, b, c);
    if (o != 0.)
      return SIGN (o)*sign;

    /* epsilon^33/4 */
    o = ORIENT1D (p[1]->x, p[3]->x);
    if (o != 0.)
      return - SIGN (o)*sign;
    
    /* epsilon^17/2 */
    o = ORIENT1D (p[1]->y, p[3]->y);
    if (o != 0.)
      return SIGN (o)*sign;
    
    /* epsilon^10 */
    o = ORIENT1D (p[0]->x, p[3]->x);
    if (o != 0.)
      return SIGN (o)*sign;
    
    /* epsilon^21/2 */
    return sign;
  }
#endif /* USE_ROBUST_PREDICATES */
}

/**
 * gts_point_orientation_sos:
 * @p1: a #GtsPoint.
 * @p2: a #GtsPoint.
 * @p3: a #GtsPoint.
 *
 * Checks for orientation of the projection of three points on the
 * (x,y) plane.
 *
 * Simulation of Simplicity (SoS) is used to break ties when the
 * orientation is degenerate (i.e. @p3 lies on the line defined by
 * @p1 and @p2).
 *
 * Returns: a positive value if @p1, @p2 and @p3 appear in
 * counterclockwise order or a negative value if they appear in
 * clockwise order.  
 */
gint gts_point_orientation_sos (GtsPoint * p1,
				GtsPoint * p2,
				GtsPoint * p3)
{
#ifndef USE_ROBUST_PREDICATES
  g_assert_not_reached ();
  return 0;
#else /* USE_ROBUST_PREDICATES */
  gdouble o;

  g_return_val_if_fail (p1 != NULL && p2 != NULL && p3 != NULL, 0);

  o = orient2d ((gdouble *) &p1->x, 
		(gdouble *) &p2->x, 
		(gdouble *) &p3->x);
  if (o != 0.)
    return SIGN (o);
  else {
    GtsPoint * p[3];
    gint sign;

    p[0] = p1; p[1] = p2; p[2] = p3;
    sign = sortp ((gpointer *) p, 3);
    
    /* epsilon^1/4 */
    o = ORIENT1D (p[1]->x, p[2]->x);
    if (o != 0.)
      return - SIGN (o)*sign;
    
    /* epsilon^1/2 */
    o = ORIENT1D (p[1]->y, p[2]->y);
    if (o != 0.)
      return SIGN (o)*sign;
    
    /* epsilon */
    o = ORIENT1D (p[0]->x, p[2]->x);
    if (o != 0.)
      return SIGN (o)*sign;
    
    /* epsilon^3/2 */
    return sign;
  }
#endif /* USE_ROBUST_PREDICATES */
}
