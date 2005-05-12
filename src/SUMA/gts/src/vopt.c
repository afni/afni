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

/* #define DEBUG_VOPT */

/* compute the normal (nx, ny, nz) as the cross-product of the first two 
   oriented edges and the norm nt = |t| as (v1xv2).v3 */
static void triangle_normal (GtsTriangle * t, 
			     gdouble * nx, 
			     gdouble * ny, 
			     gdouble * nz,
			     gdouble * nt)
{
  GtsPoint * p1, * p2 = NULL, * p3 = NULL;
  gdouble x1, y1, z1, x2, y2, z2;

  g_return_if_fail (t != NULL);

  p1 = GTS_POINT (GTS_SEGMENT (t->e1)->v1);
  if (GTS_SEGMENT (t->e1)->v1 == GTS_SEGMENT (t->e2)->v1) {
    p2 = GTS_POINT (GTS_SEGMENT (t->e2)->v2);
    p3 = GTS_POINT (GTS_SEGMENT (t->e1)->v2);
  }
  else if (GTS_SEGMENT (t->e1)->v2 == GTS_SEGMENT (t->e2)->v2) {
    p2 = GTS_POINT (GTS_SEGMENT (t->e1)->v2);
    p3 = GTS_POINT (GTS_SEGMENT (t->e2)->v1);
  }
  else if (GTS_SEGMENT (t->e1)->v1 == GTS_SEGMENT (t->e2)->v2) {
    p2 = GTS_POINT (GTS_SEGMENT (t->e2)->v1);
    p3 = GTS_POINT (GTS_SEGMENT (t->e1)->v2);
  }
  else if (GTS_SEGMENT (t->e1)->v2 == GTS_SEGMENT (t->e2)->v1) {
    p2 = GTS_POINT (GTS_SEGMENT (t->e1)->v2);
    p3 = GTS_POINT (GTS_SEGMENT (t->e2)->v2);
  }
  else
    g_assert_not_reached ();

  x1 = p2->x - p1->x;
  y1 = p2->y - p1->y;
  z1 = p2->z - p1->z;

  x2 = p3->x - p1->x;
  y2 = p3->y - p1->y;
  z2 = p3->z - p1->z;

  *nt = ((p1->y*p2->z - p1->z*p2->y)*p3->x + 
	 (p1->z*p2->x - p1->x*p2->z)*p3->y + 
	 (p1->x*p2->y - p1->y*p2->x)*p3->z);
  *nx = y1*z2 - z1*y2;
  *ny = z1*x2 - x1*z2;
  *nz = x1*y2 - y1*x2;
}

static void boundary_preservation (GtsEdge * edge,
				   GtsFace * f,
				   GtsVector e1, GtsVector e2,
				   GtsMatrix * H, GtsVector c)
{
  GtsTriangle * t = GTS_TRIANGLE (f);
  GtsEdge * edge2;
  GtsVertex * v1 = GTS_SEGMENT (edge)->v1, * v2 = GTS_SEGMENT (edge)->v2;
  GtsPoint * p1, * p2;
  GtsVector e, e3;

  /* find orientation of segment */
  edge2 = edge == t->e1 ? t->e2 : edge == t->e2 ? t->e3 : t->e1;
  if (v2 != GTS_SEGMENT (edge2)->v1 && v2 != GTS_SEGMENT (edge2)->v2) {
    v2 = v1; v1 = GTS_SEGMENT (edge)->v2;
  }
  p1 = GTS_POINT (v1);
  p2 = GTS_POINT (v2);

  e[0] = p2->x - p1->x;
  e[1] = p2->y - p1->y;
  e[2] = p2->z - p1->z;

  e1[0] += e[0];
  e1[1] += e[1];
  e1[2] += e[2];

  e3[0] = p2->y*p1->z - p2->z*p1->y;
  e3[1] = p2->z*p1->x - p2->x*p1->z;
  e3[2] = p2->x*p1->y - p2->y*p1->x;

  e2[0] += e3[0];
  e2[1] += e3[1];
  e2[2] += e3[2];

  H[0][0] += e[1]*e[1] + e[2]*e[2];
  H[0][1] -= e[0]*e[1];
  H[0][2] -= e[0]*e[2];
  H[1][0] = H[0][1];
  H[1][1] += e[0]*e[0] + e[2]*e[2];
  H[1][2] -= e[1]*e[2];
  H[2][0] = H[0][2];
  H[2][1] = H[1][2];
  H[2][2] += e[0]*e[0] + e[1]*e[1];

  c[0] += e[1]*e3[2] - e[2]*e3[1];
  c[1] += e[2]*e3[0] - e[0]*e3[2];
  c[2] += e[0]*e3[1] - e[1]*e3[0];
}

static gdouble boundary_cost (GtsEdge * edge, 
			      GtsFace * f,
			      GtsVertex * v)
{
  GtsTriangle * t = GTS_TRIANGLE (f);
  GtsEdge * edge2;
  GtsVertex * v1 = GTS_SEGMENT (edge)->v1, * v2 = GTS_SEGMENT (edge)->v2;
  GtsPoint * p1, * p2;
  GtsVector e;
  GtsPoint * p = GTS_POINT (v);

  /* find orientation of segment */
  edge2 = edge == t->e1 ? t->e2 : edge == t->e2 ? t->e3 : t->e1;
  if (v2 != GTS_SEGMENT (edge2)->v1 && v2 != GTS_SEGMENT (edge2)->v2) {
    v2 = v1; v1 = GTS_SEGMENT (edge)->v2;
  }
  p1 = GTS_POINT (v1);
  p2 = GTS_POINT (v2);  

  e[0] = (p2->y - p1->y)*(p->z - p2->z) - (p2->z - p1->z)*(p->y - p2->y);
  e[1] = (p2->z - p1->z)*(p->x - p2->x) - (p2->x - p1->x)*(p->z - p2->z);
  e[2] = (p2->x - p1->x)*(p->y - p2->y) - (p2->y - p1->y)*(p->x - p2->x);

  return e[0]*e[0] + e[1]*e[1] + e[2]*e[2];
}

static gdouble edge_boundary_cost (GtsEdge * e, GtsVertex * v)
{
  gdouble cost = 0.;
  GSList * i;

  i = GTS_SEGMENT (e)->v1->segments;
  while (i) {
    GtsFace * f;
    if (GTS_IS_EDGE (i->data) && 
	(f = gts_edge_is_boundary (i->data, NULL)))
      cost += boundary_cost (i->data, f, v);
    i = i->next;
  }
  i = GTS_SEGMENT (e)->v2->segments;
  while (i) {
    GtsFace * f;
    if (i->data != e && 
	GTS_IS_EDGE (i->data) && 
	(f = gts_edge_is_boundary (i->data, NULL)))
      cost += boundary_cost (i->data, f, v);
    i = i->next;
  }

  return cost/4.;
}

static gdouble edge_volume_cost (GtsEdge * e, GtsVertex * v)
{
  GSList * i, * triangles;
  gdouble n1, n2, n3, nt;
  gdouble cost = 0.0, a;

  triangles = gts_vertex_triangles (GTS_SEGMENT (e)->v1, NULL);
  triangles = gts_vertex_triangles (GTS_SEGMENT (e)->v2, triangles);

  i = triangles;
  while (i) {
    if (GTS_IS_FACE (i->data)) {
      triangle_normal (i->data, &n1, &n2, &n3, &nt);
      a = GTS_POINT (v)->x*n1 + 
	GTS_POINT (v)->y*n2 + 
	GTS_POINT (v)->z*n3 - nt;
      cost += a*a;
    }
    i = i->next;
  }
  g_slist_free (triangles);

  return cost/36.;
}

static gdouble edge_shape_cost (GtsEdge * e, GtsVertex * v)
{
  GSList * list, * i;
  GtsVertex 
    * v1 = GTS_SEGMENT (e)->v1,
    * v2 = GTS_SEGMENT (e)->v2;
  gdouble cost = 0.;

  list = gts_vertex_neighbors (v1, NULL, NULL);
  list = gts_vertex_neighbors (v2, list, NULL);
  i = list;
  while (i) {
    GtsPoint * p = i->data;
    if (p != GTS_POINT (v1) && p != GTS_POINT (v2))
      cost += gts_point_distance2 (p, GTS_POINT (v));
    i = i->next;
  }
  g_slist_free (list);

  return cost;
}

/**
 * gts_volume_optimized_vertex:
 * @edge: a #GtsEdge.
 * @klass: a #GtsVertexClass to be used for the new vertex.
 * @params: a #GtsVolumeOptimizedParms.
 *
 * Returns: a #GtsVertex which can be used to replace @edge for an
 * edge collapse operation. The position of the vertex is optimized in
 * order to minimize the changes in area and volume for the surface
 * using @edge. The volume enclosed by the surface is locally
 * preserved. For more details see "Fast and memory efficient
 * polygonal simplification" (1998) and "Evaluation of memoryless
 * simplification" (1999) by Lindstrom and Turk.  
 */
GtsVertex * gts_volume_optimized_vertex (GtsEdge * edge,
					 GtsVertexClass * klass,
					 GtsVolumeOptimizedParams * params)
{
  GSList * triangles, * i;
  gdouble sn1 = 0., sn2 = 0., sn3 = 0.;
  gdouble sn11 = 0., sn22 = 0., sn33 = 0.;
  gdouble sn12 = 0., sn13 = 0., sn23 = 0.;
  gdouble st = 0., stn1 = 0., stn2 = 0., stn3 = 0.;
  gdouble n1, n2, n3, nt;
  GtsMatrix * A, * Ai;
  GtsVector A1, b;
  GtsVector e1 = {0., 0., 0.}, e2 = {0., 0., 0.};
  GtsMatrix * Hb;
  GtsVector cb = {0., 0., 0.};
  GtsVertex * v;
  GtsVertex * v1, * v2;
  guint n = 0, nb = 0;
#ifdef DEBUG_VOPT
  guint nold = 0;
#endif

  g_return_val_if_fail (edge != NULL, NULL);
  g_return_val_if_fail (klass != NULL, NULL);
  g_return_val_if_fail (params != NULL, NULL);

  A = gts_matrix_zero (NULL);
  Hb = gts_matrix_zero (NULL);
  v1 = GTS_SEGMENT (edge)->v1;
  v2 = GTS_SEGMENT (edge)->v2;

  /* boundary preservation */
  i = v1->segments;
  while (i) {
    GtsEdge * edge1 = i->data;
    GtsFace * f;
    if (GTS_IS_EDGE (edge1) &&
	(f = gts_edge_is_boundary (edge1, NULL))) {
      boundary_preservation (edge1, f, e1, e2, Hb, cb);
      nb++;
    }
    i = i->next;
  }
  i = v2->segments;
  while (i) {
    GtsEdge * edge1 = i->data;
    GtsFace * f;
    if (edge1 != edge && 
	GTS_IS_EDGE (edge1) &&
	(f = gts_edge_is_boundary (edge1, NULL))) {
      boundary_preservation (edge1, f, e1, e2, Hb, cb);
      nb++;
    }
    i = i->next;
  }
  if (nb > 0) {
    GtsMatrix * H = gts_matrix_new (
	       e1[2]*e1[2] + e1[1]*e1[1], - e1[0]*e1[1], - e1[0]*e1[2], 0.,
	       - e1[0]*e1[1], e1[2]*e1[2] + e1[0]*e1[0], - e1[1]*e1[2], 0.,
	       - e1[0]*e1[2], - e1[1]*e1[2], e1[1]*e1[1] + e1[0]*e1[0], 0.,
	       0., 0., 0., 0.);
    GtsVector c;

    c[0] = e1[1]*e2[2] - e1[2]*e2[1];
    c[1] = e1[2]*e2[0] - e1[0]*e2[2];
    c[2] = e1[0]*e2[1] - e1[1]*e2[0];
    n = gts_matrix_quadratic_optimization (A, b, n, H, c);
    gts_matrix_destroy (H);
  }

  g_assert (n <= 2);

#ifdef DEBUG_VOPT
  if (n != nold) {
    fprintf (stderr, "--- boundary preservation ---\n");
    gts_matrix_print (A, stderr);
    gts_vector_print (b, stderr);
    nold = n;
  }
#endif

  /* volume preservation */
  triangles = gts_vertex_triangles (v1, NULL);
  triangles = gts_vertex_triangles (v2, triangles);

  i = triangles;
  while (i) {
    if (GTS_IS_FACE (i->data)) {
      triangle_normal (i->data, &n1, &n2, &n3, &nt);
      sn1 += n1; sn2 += n2; sn3 += n3;
      sn11 += n1*n1; sn22 += n2*n2; sn33 += n3*n3;
      sn12 += n1*n2; sn13 += n1*n3; sn23 += n2*n3;
      st += nt; stn1 += nt*n1; stn2 += nt*n2; stn3 += nt*n3;
    }
    i = i->next;
  }
  g_slist_free (triangles);

  A1[0] = sn1; A1[1] = sn2; A1[2] = sn3;
  n = gts_matrix_compatible_row (A, b, n, A1, st);

#ifdef DEBUG_VOPT
  if (n != nold) {
    fprintf (stderr, "--- volume preservation ---\n");
    gts_matrix_print (A, stderr);
    gts_vector_print (b, stderr);
    nold = n;
  }
#endif

#if 1 /* Weighted average of volume and boundary optimization */
  if (n < 3) {
    /* volume optimization and boundary optimization */
    GtsMatrix * H = gts_matrix_new (sn11, sn12, sn13, 0.,
				    sn12, sn22, sn23, 0.,
				    sn13, sn23, sn33, 0.,
				    0., 0., 0., 0.);
    GtsVector c;
    gdouble le = 9.*params->boundary_weight*
      gts_point_distance2 (GTS_POINT (v1), 
			   GTS_POINT (v2));
    guint i, j;

    c[0] = - stn1; c[1] = - stn2; c[2] = - stn3;
    if (nb > 0)
      for (i = 0; i < 3; i++) {
	for (j = 0; j < 3; j++)
	  H[i][j] = params->volume_weight*H[i][j] + le*Hb[i][j];
	c[i] = params->volume_weight*c[i] + le*cb[i];
      }
    n = gts_matrix_quadratic_optimization (A, b, n, H, c);
    gts_matrix_destroy (H);
  }

#ifdef DEBUG_VOPT
  if (n != nold) {
    fprintf (stderr, "--- volume and boundary optimization ---\n");
    gts_matrix_print (A, stderr);
    gts_vector_print (b, stderr);
    nold = n;
  }
#endif

  if (n < 3) {
    /* triangle shape optimization */
    gdouble nv = 0.0;
    GtsMatrix * H;
    GtsVector c = {0., 0., 0.};
    GSList * list, * i;

    list = gts_vertex_neighbors (v1, NULL, NULL);
    list = gts_vertex_neighbors (v2, list, NULL);

    i = list;
    while (i) {
      GtsPoint * p1 = i->data;
      if (p1 != GTS_POINT (v1) && p1 != GTS_POINT (v2)) {
	nv += 1.0;
	c[0] -= p1->x;
	c[1] -= p1->y;
	c[2] -= p1->z;
      }
      i = i->next;
    }
    g_slist_free (list);
    
    H = gts_matrix_new (nv, 0., 0., 0.,
			0., nv, 0., 0.,
			0., 0., nv, 0.,
			0., 0., 0., 0.);
    n = gts_matrix_quadratic_optimization (A, b, n, H, c);
    gts_matrix_destroy (H);
  }

#ifdef DEBUG_VOPT
  if (n != nold) {
    fprintf (stderr, "--- triangle shape optimization ---\n");
    gts_matrix_print (A, stderr);
    gts_vector_print (b, stderr);
    nold = n;
  }
#endif
#else /* Weighted average of volume, boundary and shape optimization */
  if (n < 3) {
    /* volume optimization, boundary and shape optimization */
    GtsMatrix * H; 
    GtsVector c;
    gdouble l2 = gts_point_distance2 (GTS_POINT (v1), 
				      GTS_POINT (v2));
    gdouble wv = params->volume_weight/32.;
    gdouble wb = params->boundary_weight/4.*l2;
    gdouble ws = params->shape_weight*l2*l2;
    
    gdouble nv = 0.0;
    GtsVector cs = {0., 0., 0.};
    GSList * list, * i;

    list = gts_vertex_neighbors (v1, NULL, NULL);
    list = gts_vertex_neighbors (v2, list, NULL);

    i = list;
    while (i) {
      GtsPoint * p1 = i->data;
      if (p1 != GTS_POINT (v1) && p1 != GTS_POINT (v2)) {
	nv += 1.0;
	cs[0] -= p1->x;
	cs[1] -= p1->y;
	cs[2] -= p1->z;
      }
      i = i->next;
    }
    g_slist_free (list);

    H = gts_matrix_new (wv*sn11 + wb*Hb[0][0] + ws*nv, 
			wv*sn12 + wb*Hb[0][1], 
			wv*sn13 + wb*Hb[0][2],
			wv*sn12 + wb*Hb[1][0], 
			wv*sn22 + wb*Hb[1][1] + ws*nv, 
			wv*sn23 + wb*Hb[1][2],
			wv*sn13 + wb*Hb[2][0], 
			wv*sn23 + wb*Hb[2][1], 
			wv*sn33 + wb*Hb[2][2] + ws*nv);

    c[0] = - wv*stn1 + wb*cb[0] + ws*cs[0];
    c[1] = - wv*stn2 + wb*cb[1] + ws*cs[1];
    c[2] = - wv*stn3 + wb*cb[2] + ws*cs[2];

    n = gts_matrix_quadratic_optimization (A, b, n, H, c);
    gts_matrix_destroy (H);
  }

#ifdef DEBUG_VOPT
  if (n != nold) {
    fprintf (stderr, "--- volume, boundary and shape optimization ---\n");
    gts_matrix_print (A, stderr);
    gts_vector_print (b, stderr);
    nold = n;
  }
#endif
#endif /* Weighted average of volume, boundary and shape optimization */

  g_assert (n == 3);
  g_assert ((Ai = gts_matrix3_inverse (A)));

  v = gts_vertex_new (klass,
		      Ai[0][0]*b[0] + Ai[0][1]*b[1] + Ai[0][2]*b[2],
		      Ai[1][0]*b[0] + Ai[1][1]*b[1] + Ai[1][2]*b[2],
		      Ai[2][0]*b[0] + Ai[2][1]*b[1] + Ai[2][2]*b[2]);

  gts_matrix_destroy (A);
  gts_matrix_destroy (Ai);
  gts_matrix_destroy (Hb);
  
  return v;
}

/**
 * gts_volume_optimized_cost:
 * @e: a #GtsEdge.
 * @params: a #GtsVolumeOptimizedParams.
 * 
 * Returns: the cost for the collapse of @e as minimized by the function
 * gts_volume_optimized_vertex().
 */
gdouble gts_volume_optimized_cost (GtsEdge * e, 
				   GtsVolumeOptimizedParams * params)
{
  GtsVertex * v;
  gdouble cost;
  gdouble length2;

  g_return_val_if_fail (e != NULL, G_MAXDOUBLE);
  g_return_val_if_fail (params != NULL, G_MAXDOUBLE);

  v = gts_volume_optimized_vertex (e, gts_vertex_class (), params);

  length2 = gts_point_distance2 (GTS_POINT (GTS_SEGMENT (e)->v1), 
				 GTS_POINT (GTS_SEGMENT (e)->v2));
  cost = 
    params->volume_weight*edge_volume_cost (e, v) +
    params->boundary_weight*length2*edge_boundary_cost (e, v) +
    params->shape_weight*length2*length2*edge_shape_cost (e, v);
  gts_object_destroy (GTS_OBJECT (v));

  return cost;
}
