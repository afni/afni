#include "gts.h"

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

static GtsVertex * edge_collapse (GtsSurface * s,
				  GtsEdge * e,
				  GtsVertex * (*midvertex) (GtsEdge *,
							    GtsVertexClass *))
{
  GtsVertex  * v1 = GTS_SEGMENT (e)->v1, * v2 = GTS_SEGMENT (e)->v2, * mid;
  GtsSplit * vs;
  GtsObject * o1, * o2;

  /* if the edge is degenerate (i.e. v1 == v2), destroy and return */
  if (v1 == v2) {
    gts_object_destroy (GTS_OBJECT (e));
    return NULL;
  }

  mid = (*midvertex) (e, s->vertex_class);
  o1 = GTS_OBJECT (v1);
  o2 = GTS_OBJECT (v2);
  vs = gts_split_new (gts_split_class (), mid, o1, o2);
  gts_split_collapse (vs, s->edge_class, NULL);

  return mid;
}

int main (int argc, char * argv[])
{
  GtsSurface * surface = gts_surface_new (gts_surface_class (),
					  gts_face_class (),
					  gts_edge_class (),
					  gts_vertex_class ());
		     
  GtsVertex * v1 = gts_vertex_new (gts_vertex_class (),
				   0.48, 1.068, 0.116);
  GtsVertex * v2 = gts_vertex_new (gts_vertex_class (),
				   0.492, 1.068, 0.118);
  GtsVertex * v3 = gts_vertex_new (gts_vertex_class (),
				   0.492, 1.056, 0.118);
  GtsVertex * v4 = gts_vertex_new (gts_vertex_class (),
				   0.516, 1.044, 0.118);
  GtsVertex * v5 = gts_vertex_new (gts_vertex_class (),
				   0.492, 1.04057, 0.118);
  GtsVertex * v6 = gts_vertex_new (gts_vertex_class (),
				   0.492, 1.02, 0.118);
  GtsVertex * v7 = gts_vertex_new (gts_vertex_class (),
				   0.504, 1.044, 0.118);
  GtsVertex * v8 = gts_vertex_new (gts_vertex_class (),
				   0.504, 1.056, 0.117);
  GtsVertex * v9 = gts_vertex_new (gts_vertex_class (),
				   0.48, 1.056, 0.117);
  GtsVertex * v10 = gts_vertex_new (gts_vertex_class (),
                                    0.48, 1.032, 0.117);

  GtsEdge * e1 = gts_edge_new (gts_edge_class (), v1, v2);
  GtsEdge * e2 = gts_edge_new (gts_edge_class (), v1, v3);
  GtsEdge * e3 = gts_edge_new (gts_edge_class (), v3, v2);
  GtsEdge * e4 = gts_edge_new (gts_edge_class (), v4, v5);
  GtsEdge * e5 = gts_edge_new (gts_edge_class (), v5, v6);
  GtsEdge * e6 = gts_edge_new (gts_edge_class (), v6, v4);
  GtsEdge * e7 = gts_edge_new (gts_edge_class (), v5, v3);
  GtsEdge * e8 = gts_edge_new (gts_edge_class (), v7, v5);
  GtsEdge * e9 = gts_edge_new (gts_edge_class (), v7, v3);
  GtsEdge * e10 = gts_edge_new (gts_edge_class (), v3, v8);
  GtsEdge * e11 = gts_edge_new (gts_edge_class (), v2, v8);
  GtsEdge * e12 = gts_edge_new (gts_edge_class (), v7, v8);
  GtsEdge * e13 = gts_edge_new (gts_edge_class (), v7, v4);
  GtsEdge * e14 = gts_edge_new (gts_edge_class (), v9, v3);
  GtsEdge * e15 = gts_edge_new (gts_edge_class (), v9, v1);
  GtsEdge * e16 = gts_edge_new (gts_edge_class (), v10, v5);
  GtsEdge * e17 = gts_edge_new (gts_edge_class (), v9, v5);
  GtsEdge * e18 = gts_edge_new (gts_edge_class (), v10, v9);
  GtsEdge * e19 = gts_edge_new (gts_edge_class (), v10, v6);

  GtsFace * f1 = gts_face_new (gts_face_class (),
			       e1, e2, e3);
  GtsFace * f2 = gts_face_new (gts_face_class (),
			       e4, e5, e6);
  GtsFace * f3 = gts_face_new (gts_face_class (),
			       e7, e8, e9);
  GtsFace * f4 = gts_face_new (gts_face_class (),
			       e10, e11, e3);
  GtsFace * f5 = gts_face_new (gts_face_class (),
			       e10, e9, e12);
  GtsFace * f6 = gts_face_new (gts_face_class (),
			       e8, e4, e13);
  GtsFace * f7 = gts_face_new (gts_face_class (),
			       e14, e2, e15);
  GtsFace * f8 = gts_face_new (gts_face_class (),
			       e16, e17, e18);
  GtsFace * f9 = gts_face_new (gts_face_class (),
			       e7, e14, e17);
  GtsFace * f10 = gts_face_new (gts_face_class (),
                                e16, e19, e5);
 
  GtsVertex * v;

  gts_surface_add_face (surface, f1);
  gts_surface_add_face (surface, f2);
  gts_surface_add_face (surface, f3);
  gts_surface_add_face (surface, f4);
  gts_surface_add_face (surface, f5);
  gts_surface_add_face (surface, f6);
  gts_surface_add_face (surface, f7);
  gts_surface_add_face (surface, f8);
  gts_surface_add_face (surface, f9);
  gts_surface_add_face (surface, f10);

  gts_surface_print_stats (surface, stderr);
  fprintf (stderr, "volume: %g\n", gts_surface_volume (surface));

#if 1
  v = edge_collapse (surface, e7, gts_volume_optimized_vertex);
#else
  v = gts_volume_optimized_vertex (e7, gts_vertex_class ());
  fprintf (stderr, "(%g, %g, %g) edge cost: %g cost: %g\n", 
	   GTS_POINT (v)->x, GTS_POINT (v)->y, GTS_POINT (v)->z,
	   edge_volume_cost (e7, v),
	   gts_volume_optimized_cost (e7));
#endif

  gts_surface_print_stats (surface, stderr);
  fprintf (stderr, "volume: %g\n", gts_surface_volume (surface));

  gts_surface_write (surface, stdout);

  return 0;
}
