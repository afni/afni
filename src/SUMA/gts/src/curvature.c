/* GTS - Library for the manipulation of triangulated surfaces
 * Copyright (C) 1999-2002 Ray Jones, Stéphane Popinet
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

static gboolean angle_obtuse (GtsVertex * v, GtsFace * f)
{
  GtsEdge * e = gts_triangle_edge_opposite (GTS_TRIANGLE (f), v);
  GtsVector vec1, vec2;

  gts_vector_init (vec1, GTS_POINT (v), GTS_POINT (GTS_SEGMENT (e)->v1));
  gts_vector_init (vec2, GTS_POINT (v), GTS_POINT (GTS_SEGMENT (e)->v2));

  return (gts_vector_scalar (vec1, vec2) < 0.0);
}

static gboolean triangle_obtuse (GtsVertex * v, GtsFace * f)
{
  GtsEdge * e = gts_triangle_edge_opposite (GTS_TRIANGLE (f), v);

  return (angle_obtuse (v, f) ||
          angle_obtuse (GTS_SEGMENT (e)->v1, f) ||
          angle_obtuse (GTS_SEGMENT (e)->v2, f));
} 

static gdouble cotan (GtsVertex * vo, GtsVertex * v1, GtsVertex * v2)
{
  /* cf. Appendix B of [Meyer et al 2002] */
  GtsVector u, v;
  gdouble udotv, denom;

  gts_vector_init (u, GTS_POINT (vo), GTS_POINT (v1));
  gts_vector_init (v, GTS_POINT (vo), GTS_POINT (v2));

  udotv = gts_vector_scalar (u, v);
  denom = sqrt (gts_vector_scalar (u,u)*gts_vector_scalar (v,v) -
		udotv*udotv);


  /* denom can be zero if u==v.  Returning 0 is acceptable, based on
   * the callers of this function below. */
  if (denom == 0.0) return (0.0);

  return (udotv/denom);
}

static gdouble angle_from_cotan (GtsVertex * vo, 
				 GtsVertex * v1, GtsVertex * v2)
{
  /* cf. Appendix B and the caption of Table 1 from [Meyer et al 2002] */
  GtsVector u, v;
  gdouble udotv, denom;

  gts_vector_init (u, GTS_POINT (vo), GTS_POINT (v1));
  gts_vector_init (v, GTS_POINT (vo), GTS_POINT (v2));

  udotv = gts_vector_scalar (u, v);
  denom = sqrt (gts_vector_scalar (u,u)*gts_vector_scalar (v,v) 
		- udotv*udotv);

  /* Note: I assume this is what they mean by using atan2 (). -Ray Jones */

  /* tan = denom/udotv = y/x (see man page for atan2) */
  return (fabs (atan2 (denom, udotv)));
}

static gdouble region_area (GtsVertex * v, GtsFace * f)
{
  /* cf. Section 3.3 of [Meyer et al 2002] */
  
  if (gts_triangle_area (GTS_TRIANGLE (f)) == 0.0) return (0.0);

  if (triangle_obtuse (v, f)) {
    if (angle_obtuse (v, f))
      return (gts_triangle_area (GTS_TRIANGLE (f))/2.0);
    else
      return (gts_triangle_area (GTS_TRIANGLE (f))/4.0);
  } else {
    GtsEdge * e = gts_triangle_edge_opposite (GTS_TRIANGLE (f), v);

    return ((cotan (GTS_SEGMENT (e)->v1, v, GTS_SEGMENT (e)->v2)* 
             gts_point_distance2 (GTS_POINT (v), 
				  GTS_POINT (GTS_SEGMENT (e)->v2)) +
             cotan (GTS_SEGMENT (e)->v2, v, GTS_SEGMENT (e)->v1)* 
             gts_point_distance2 (GTS_POINT (v), 
                                  GTS_POINT (GTS_SEGMENT (e)->v1)))
            /8.0);
  }
}

/** 
 * gts_vertex_mean_curvature_normal:
 * @v: a #GtsVertex.  
 * @s: a #GtsSurface.
 * @Kh: the Mean Curvature Normal at @v.
 *
 * Computes the Discrete Mean Curvature Normal approximation at @v.
 * The mean curvature at @v is half the magnitude of the vector @Kh.
 *
 * Note: the normal computed is not unit length, and may point either
 * into or out of the surface, depending on the curvature at @v.  It
 * is the responsibility of the caller of the function to use the mean
 * curvature normal appropriately.
 *
 * This approximation is from the paper:
 * Discrete Differential-Geometry Operators for Triangulated 2-Manifolds
 * Mark Meyer, Mathieu Desbrun, Peter Schroder, Alan H. Barr
 * VisMath '02, Berlin (Germany) 
 * http://www-grail.usc.edu/pubs.html
 *
 * Returns: %TRUE if the operator could be evaluated, %FALSE if the
 * evaluation failed for some reason (@v is boundary or is the
 * endpoint of a non-manifold edge.)
 */
gboolean gts_vertex_mean_curvature_normal (GtsVertex * v, GtsSurface * s, 
                                           GtsVector Kh)
{
  GSList * faces, * edges, * i;
  gdouble area = 0.0;

  g_return_val_if_fail (v != NULL, FALSE);
  g_return_val_if_fail (s != NULL, FALSE);

  /* this operator is not defined for boundary edges */
  if (gts_vertex_is_boundary (v, s)) return (FALSE);
    
  faces = gts_vertex_faces (v, s, NULL);
  g_return_val_if_fail (faces != NULL, FALSE);

  edges = gts_vertex_fan_oriented (v, s);
  if (edges == NULL) {
    g_slist_free (faces);
    return (FALSE);
  }

  i = faces;
  while (i) {
    GtsFace * f = i->data;

    area += region_area (v, f);
    i = i->next;
  } 
  g_slist_free (faces);

  Kh[0] = Kh[1] = Kh[2] = 0.0;

  i = edges;
  while (i) {
    GtsEdge * e = i->data;
    GtsVertex * v1 = GTS_SEGMENT (e)->v1;
    GtsVertex * v2 = GTS_SEGMENT (e)->v2;
    gdouble temp;

    temp = cotan (v1, v, v2);
    Kh[0] += temp*(GTS_POINT (v2)->x - GTS_POINT (v)->x);
    Kh[1] += temp*(GTS_POINT (v2)->y - GTS_POINT (v)->y);
    Kh[2] += temp*(GTS_POINT (v2)->z - GTS_POINT (v)->z);

    temp = cotan (v2, v, v1);
    Kh[0] += temp*(GTS_POINT (v1)->x - GTS_POINT (v)->x);
    Kh[1] += temp*(GTS_POINT (v1)->y - GTS_POINT (v)->y);
    Kh[2] += temp*(GTS_POINT (v1)->z - GTS_POINT (v)->z);

    i = i->next;
  }
  g_slist_free (edges);

  if (area > 0.0) {
    Kh[0] /= 2*area;
    Kh[1] /= 2*area;
    Kh[2] /= 2*area;
  } else {
    return (FALSE);
  }
 
  return TRUE;
}

/** 
 * gts_vertex_gaussian_curvature:
 * @v: a #GtsVertex.  
 * @s: a #GtsSurface.
 * @Kg: the Discrete Gaussian Curvature approximation at @v.
 *
 * Computes the Discrete Gaussian Curvature approximation at @v.
 *
 * This approximation is from the paper:
 * Discrete Differential-Geometry Operators for Triangulated 2-Manifolds
 * Mark Meyer, Mathieu Desbrun, Peter Schroder, Alan H. Barr
 * VisMath '02, Berlin (Germany) 
 * http://www-grail.usc.edu/pubs.html
 *
 * Returns: %TRUE if the operator could be evaluated, %FALSE if the
 * evaluation failed for some reason (@v is boundary or is the
 * endpoint of a non-manifold edge.)
 */
gboolean gts_vertex_gaussian_curvature (GtsVertex * v, GtsSurface * s, 
                                        gdouble * Kg)
{
  GSList * faces, * edges, * i;
  gdouble area = 0.0;
  gdouble angle_sum = 0.0;

  g_return_val_if_fail (v != NULL, FALSE);
  g_return_val_if_fail (s != NULL, FALSE);
  g_return_val_if_fail (Kg != NULL, FALSE);

  /* this operator is not defined for boundary edges */
  if (gts_vertex_is_boundary (v, s)) return (FALSE);
    
  faces = gts_vertex_faces (v, s, NULL);
  g_return_val_if_fail (faces != NULL, FALSE);

  edges = gts_vertex_fan_oriented (v, s);
  if (edges == NULL) {
    g_slist_free (faces);
    return (FALSE);
  }

  i = faces;
  while (i) {
    GtsFace * f = i->data;

    area += region_area (v, f);
    i = i->next;
  } 
  g_slist_free (faces);

  i = edges;
  while (i) {
    GtsEdge * e = i->data;
    GtsVertex * v1 = GTS_SEGMENT (e)->v1;
    GtsVertex * v2 = GTS_SEGMENT (e)->v2;

    angle_sum += angle_from_cotan (v, v1, v2);
    i = i->next;
  }
  g_slist_free (edges);

  *Kg = (2.0*M_PI - angle_sum)/area;
 
  return TRUE;
}

/** 
 * gts_vertex_principal_curvatures:
 * @Kh: mean curvature.
 * @Kg: Gaussian curvature.
 * @K1: first principal curvature.
 * @K2: second principal curvature.
 *
 * Computes the principal curvatures at a point given the mean and
 * Gaussian curvatures at that point.  
 *
 * The mean curvature can be computed as one-half the magnitude of the
 * vector computed by gts_vertex_mean_curvature_normal().
 *
 * The Gaussian curvature can be computed with
 * gts_vertex_gaussian_curvature().
 */
void gts_vertex_principal_curvatures (gdouble Kh, gdouble Kg, 
				      gdouble * K1, gdouble * K2)
{
  gdouble temp = Kh*Kh - Kg;

  g_return_if_fail (K1 != NULL);
  g_return_if_fail (K2 != NULL);

  if (temp < 0.0) temp = 0.0;
  temp = sqrt (temp);
  *K1 = Kh + temp;
  *K2 = Kh - temp;
}

/* from Maple */
static void linsolve (gdouble m11, gdouble m12, gdouble b1,
		      gdouble m21, gdouble m22, gdouble b2,
		      gdouble * x1, gdouble * x2)
{
  gdouble temp;

  temp = 1.0 / (m21*m12 - m11*m22);
  *x1 = (m12*b2 - m22*b1)*temp;
  *x2 = (m11*b2 - m21*b1)*temp;
}
                
/* from Maple - largest eigenvector of [a b; b c] */
static void eigenvector (gdouble a, gdouble b, gdouble c,
			 GtsVector e)
{
  if (b == 0.0) {
    e[0] = 0.0;
  } else {
    e[0] = -(c - a - sqrt (c*c - 2*a*c + a*a + 4*b*b))/(2*b);
  }
  e[1] = 1.0;
  e[2] = 0.0;
}

/** 
 * gts_vertex_principal_directions:
 * @v: a #GtsVertex.  
 * @s: a #GtsSurface.
 * @Kh: mean curvature normal (a #GtsVector).
 * @Kg: Gaussian curvature (a gdouble).
 * @e1: first principal curvature direction (direction of largest curvature).
 * @e2: second principal curvature direction.
 *
 * Computes the principal curvature directions at a point given @Kh
 * and @Kg, the mean curvature normal and Gaussian curvatures at that
 * point, computed with gts_vertex_mean_curvature_normal() and
 * gts_vertex_gaussian_curvature(), respectively. 
 *
 * Note that this computation is very approximate and tends to be
 * unstable.  Smoothing of the surface or the principal directions may
 * be necessary to achieve reasonable results.  
 */
void gts_vertex_principal_directions (GtsVertex * v, GtsSurface * s,
                                      GtsVector Kh, gdouble Kg,
				      GtsVector e1, GtsVector e2)
{
  GtsVector N;
  gdouble normKh;
  GSList * i, * j;
  GtsVector basis1, basis2, d, eig;
  gdouble ve2, vdotN;
  gdouble aterm_da, bterm_da, cterm_da, const_da;
  gdouble aterm_db, bterm_db, cterm_db, const_db;
  gdouble a, b, c;
  gdouble K1, K2;
  gdouble *weights, *kappas, *d1s, *d2s;
  gint edge_count;
  gdouble err_e1, err_e2;
  int e;

  /* compute unit normal */
  normKh = sqrt (gts_vector_scalar (Kh, Kh));

  if (normKh > 0.0) {
    N[0] = Kh[0] / normKh;
    N[1] = Kh[1] / normKh;
    N[2] = Kh[2] / normKh;
  } else {
    /* This vertex is a point of zero mean curvature (flat or saddle
     * point).  Compute a normal by averaging the adjacent triangles
     */
    N[0] = N[1] = N[2] = 0.0;
    i = gts_vertex_faces (v, s, NULL);
    while (i) {
      gdouble x, y, z;
      gts_triangle_normal (GTS_TRIANGLE ((GtsFace *) i->data),
                           &x, &y, &z);
      N[0] += x;
      N[1] += y;
      N[2] += z;

      i = i->next;
    }
    g_return_if_fail (gts_vector_norm (N) > 0.0);
    gts_vector_normalize (N);
  }
    

  /* construct a basis from N: */
  /* set basis1 to any component not the largest of N */
  basis1[0] =  basis1[1] =  basis1[2] = 0.0;
  if (fabs (N[0]) > fabs (N[1]))
    basis1[1] = 1.0;
  else
    basis1[0] = 1.0;
    
  /* make basis2 orthogonal to N */
  gts_vector_cross (basis2, N, basis1);
  gts_vector_normalize (basis2);

  /* make basis1 orthogonal to N and basis2 */
  gts_vector_cross (basis1, N, basis2);
  gts_vector_normalize (basis1);
  
  aterm_da = bterm_da = cterm_da = const_da = 0.0;
  aterm_db = bterm_db = cterm_db = const_db = 0.0;

  weights = g_malloc (sizeof (gdouble)*g_slist_length (v->segments));
  kappas = g_malloc (sizeof (gdouble)*g_slist_length (v->segments));
  d1s = g_malloc (sizeof (gdouble)*g_slist_length (v->segments));
  d2s = g_malloc (sizeof (gdouble)*g_slist_length (v->segments));
  edge_count = 0;

  i = v->segments;
  while (i) {
    GtsEdge * e;
    GtsFace * f1, * f2;
    gdouble weight, kappa, d1, d2;
    GtsVector vec_edge;

    if (! GTS_IS_EDGE (i->data)) {
      i = i->next;
      continue;
    }

    e = i->data;

    /* since this vertex passed the tests in
     * gts_vertex_mean_curvature_normal(), this should be true. */
    g_assert (gts_edge_face_number (e, s) == 2);

    /* identify the two triangles bordering e in s */
    f1 = f2 = NULL;
    j = e->triangles;
    while (j) {
      if ((! GTS_IS_FACE (j->data)) || 
          (! gts_face_has_parent_surface (GTS_FACE (j->data), s))) {
        j = j->next;
        continue;
      }
      if (f1 == NULL)
        f1 = GTS_FACE (j->data);
      else {
        f2 = GTS_FACE (j->data);
        break;
      }
      j = j->next;
    }
    g_assert (f2 != NULL);

    /* We are solving for the values of the curvature tensor 
     *     B = [ a b ; b c ].  
     * The computations here are from section 5 of [Meyer et al 2002].  
     *
     * The first step is to calculate the linear equations governing
     * the values of (a,b,c).  These can be computed by setting the
     * derivatives of the error E to zero (section 5.3).
     * 
     * Since a + c = norm(Kh), we only compute the linear equations
     * for dE/da and dE/db.  (NB: [Meyer et al 2002] has the
     * equation a + b = norm(Kh), but I'm almost positive this is
     * incorrect.)
     *
     * Note that the w_ij (defined in section 5.2) are all scaled by
     * (1/8*A_mixed).  We drop this uniform scale factor because the
     * solution of the linear equations doesn't rely on it.
     *
     * The terms of the linear equations are xterm_dy with x in
     * {a,b,c} and y in {a,b}.  There are also const_dy terms that are
     * the constant factors in the equations.  
     */

    /* find the vector from v along edge e */
    gts_vector_init (vec_edge, GTS_POINT (v), 
                     GTS_POINT ((GTS_SEGMENT (e)->v1 == v) ? 
                                GTS_SEGMENT (e)->v2 : GTS_SEGMENT (e)->v1));
    ve2 = gts_vector_scalar (vec_edge, vec_edge);
    vdotN = gts_vector_scalar (vec_edge, N);

    /* section 5.2 - There is a typo in the computation of kappa.  The
     * edges should be x_j-x_i.
     */
    kappa = 2.0 * vdotN / ve2;

    /* section 5.2 */

    /* I don't like performing a minimization where some of the
     * weights can be negative (as can be the case if f1 or f2 are
     * obtuse).  To ensure all-positive weights, we check for
     * obtuseness and use values similar to those in region_area(). */
    weight = 0.0;
    if (! triangle_obtuse(v, f1)) {
      weight += ve2 * 
        cotan (gts_triangle_vertex_opposite (GTS_TRIANGLE (f1), e), 
               GTS_SEGMENT (e)->v1, GTS_SEGMENT (e)->v2) / 8.0;
    } else {
      if (angle_obtuse (v, f1)) {
        weight += ve2 * gts_triangle_area (GTS_TRIANGLE (f1)) / 4.0;
      } else {
        weight += ve2 * gts_triangle_area (GTS_TRIANGLE (f1)) / 8.0;
      }
    }

    if (! triangle_obtuse(v, f2)) {
      weight += ve2 * 
        cotan (gts_triangle_vertex_opposite (GTS_TRIANGLE (f2), e), 
               GTS_SEGMENT (e)->v1, GTS_SEGMENT (e)->v2) / 8.0;
    } else {
      if (angle_obtuse (v, f2)) {
        weight += ve2 * gts_triangle_area (GTS_TRIANGLE (f2)) / 4.0;
      } else {
        weight += ve2 * gts_triangle_area (GTS_TRIANGLE (f2)) / 8.0;
      }
    }

    /* projection of edge perpendicular to N (section 5.3) */
    d[0] = vec_edge[0] - vdotN * N[0];
    d[1] = vec_edge[1] - vdotN * N[1];
    d[2] = vec_edge[2] - vdotN * N[2];
    gts_vector_normalize (d);
    
    /* not explicit in the paper, but necessary.  Move d to 2D basis. */
    d1 = gts_vector_scalar (d, basis1);
    d2 = gts_vector_scalar (d, basis2);

    /* store off the curvature, direction of edge, and weights for later use */
    weights[edge_count] = weight;
    kappas[edge_count] = kappa;
    d1s[edge_count] = d1;
    d2s[edge_count] = d2;
    edge_count++;

    /* Finally, update the linear equations */
    aterm_da += weight * d1 * d1 * d1 * d1;
    bterm_da += weight * d1 * d1 * 2 * d1 * d2;
    cterm_da += weight * d1 * d1 * d2 * d2;
    const_da += weight * d1 * d1 * (- kappa);

    aterm_db += weight * d1 * d2 * d1 * d1;
    bterm_db += weight * d1 * d2 * 2 * d1 * d2;
    cterm_db += weight * d1 * d2 * d2 * d2;
    const_db += weight * d1 * d2 * (- kappa);

    i = i->next;
  }

  /* now use the identity (Section 5.3) a + c = |Kh| = 2 * kappa_h */
  aterm_da -= cterm_da;
  const_da += cterm_da * normKh;

  aterm_db -= cterm_db;
  const_db += cterm_db * normKh;
  
  /* check for solvability of the linear system */
  if (((aterm_da * bterm_db - aterm_db * bterm_da) != 0.0) &&
      ((const_da != 0.0) || (const_db != 0.0))) {
    linsolve (aterm_da, bterm_da, -const_da,
              aterm_db, bterm_db, -const_db,
              &a, &b);

    c = normKh - a;

    eigenvector (a, b, c, eig);
  } else {
    /* region of v is planar */
    eig[0] = 1.0;
    eig[1] = 0.0;
  }

  /* Although the eigenvectors of B are good estimates of the
   * principal directions, it seems that which one is attached to
   * which curvature direction is a bit arbitrary.  This may be a bug
   * in my implementation, or just a side-effect of the inaccuracy of
   * B due to the discrete nature of the sampling.
   *
   * To overcome this behavior, we'll evaluate which assignment best
   * matches the given eigenvectors by comparing the curvature
   * estimates computed above and the curvatures calculated from the
   * discrete differential operators.  */

  gts_vertex_principal_curvatures (0.5 * normKh, Kg, &K1, &K2);
  
  err_e1 = err_e2 = 0.0;
  /* loop through the values previously saved */
  for (e = 0; e < edge_count; e++) {
    gdouble weight, kappa, d1, d2;
    gdouble temp1, temp2;
    gdouble delta;

    weight = weights[e];
    kappa = kappas[e];
    d1 = d1s[e];
    d2 = d2s[e];

    temp1 = fabs (eig[0] * d1 + eig[1] * d2);
    temp1 = temp1 * temp1;
    temp2 = fabs (eig[1] * d1 - eig[0] * d2);
    temp2 = temp2 * temp2;

    /* err_e1 is for K1 associated with e1 */
    delta = K1 * temp1 + K2 * temp2 - kappa;
    err_e1 += weight * delta * delta;

    /* err_e2 is for K1 associated with e2 */
    delta = K2 * temp1 + K1 * temp2 - kappa;
    err_e2 += weight * delta * delta;
  }
  g_free (weights);
  g_free (kappas);
  g_free (d1s);
  g_free (d2s);

  /* rotate eig by a right angle if that would decrease the error */
  if (err_e2 < err_e1) {
    gdouble temp = eig[0];

    eig[0] = eig[1];
    eig[1] = -temp;
  }

  e1[0] = eig[0] * basis1[0] + eig[1] * basis2[0];
  e1[1] = eig[0] * basis1[1] + eig[1] * basis2[1];
  e1[2] = eig[0] * basis1[2] + eig[1] * basis2[2];
  gts_vector_normalize (e1);

  /* make N,e1,e2 a right handed coordinate sytem */
  gts_vector_cross (e2, N, e1);
  gts_vector_normalize (e2);
}
