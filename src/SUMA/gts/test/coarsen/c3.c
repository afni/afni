#include "gts.h"

static GtsVertex * edge_collapse (GtsSurface * s,
				  GtsEdge * e,
				  GtsCoarsenFunc coarsen_func,
				  gpointer coarsen_data)
{
  GtsVertex  * v1 = GTS_SEGMENT (e)->v1, * v2 = GTS_SEGMENT (e)->v2, * mid;
  GtsSplit * vs;
  GtsObject * o1, * o2;

  /* if the edge is degenerate (i.e. v1 == v2), destroy and return */
  if (v1 == v2) {
    gts_object_destroy (GTS_OBJECT (e));
    return NULL;
  }

  mid = (*coarsen_func) (e, s->vertex_class, coarsen_data);
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
				   11640, 15867.5, 1080);
  GtsVertex * v2 = gts_vertex_new (gts_vertex_class (),
				   11700, 15720, 1070);
  GtsVertex * v3 = gts_vertex_new (gts_vertex_class (),
				   11616, 15728.8, 1084);
  GtsVertex * v4 = gts_vertex_new (gts_vertex_class (),
				   11580, 15840, 1090);
  GtsVertex * v5 = gts_vertex_new (gts_vertex_class (),
				   11460, 15840, 1100);
  GtsVertex * v6 = gts_vertex_new (gts_vertex_class (),
				   11520, 15840, 1100);
  GtsVertex * v7 = gts_vertex_new (gts_vertex_class (),
				   11520, 15780, 1100);
  GtsVertex * v8 = gts_vertex_new (gts_vertex_class (),
				   11700, 15660, 1070);
  GtsVertex * v9 = gts_vertex_new (gts_vertex_class (),
				   11672.4, 15600.1, 1074.6);
  GtsVertex * v10 = gts_vertex_new (gts_vertex_class (),
                                    11520, 15751.7, 1100);
  GtsVertex * v11 = gts_vertex_new (gts_vertex_class (),
                                    11569.5, 15646.1, 1091.75);
  GtsVertex * v;

  GtsEdge * e1 = gts_edge_new (gts_edge_class (), v1, v2);
  GtsEdge * e2 = gts_edge_new (gts_edge_class (), v1, v3);
  GtsEdge * e3 = gts_edge_new (gts_edge_class (), v3, v2);
  GtsEdge * e4 = gts_edge_new (gts_edge_class (), v4, v1);
  GtsEdge * e5 = gts_edge_new (gts_edge_class (), v3, v4);
  GtsEdge * e6 = gts_edge_new (gts_edge_class (), v5, v6);
  GtsEdge * e7 = gts_edge_new (gts_edge_class (), v5, v7);
  GtsEdge * e8 = gts_edge_new (gts_edge_class (), v6, v7);
  GtsEdge * e9 = gts_edge_new (gts_edge_class (), v8, v2);
  GtsEdge * e10 = gts_edge_new (gts_edge_class (), v8, v3);
  GtsEdge * e11 = gts_edge_new (gts_edge_class (), v6, v4);
  GtsEdge * e12 = gts_edge_new (gts_edge_class (), v6, v3);
  GtsEdge * e13 = gts_edge_new (gts_edge_class (), v8, v9);
  GtsEdge * e14 = gts_edge_new (gts_edge_class (), v3, v9);
  GtsEdge * e15 = gts_edge_new (gts_edge_class (), v7, v10);
  GtsEdge * e16 = gts_edge_new (gts_edge_class (), v10, v5);
  GtsEdge * e17 = gts_edge_new (gts_edge_class (), v11, v3);
  GtsEdge * e18 = gts_edge_new (gts_edge_class (), v9, v11);
  GtsEdge * e19 = gts_edge_new (gts_edge_class (), v10, v11);
  GtsEdge * e20 = gts_edge_new (gts_edge_class (), v11, v7);
  GtsEdge * e21 = gts_edge_new (gts_edge_class (), v7, v3);

  GtsFace * f1 = gts_face_new (gts_face_class (),
			       e1, e2, e3);
  GtsFace * f2 = gts_face_new (gts_face_class (),
			       e4, e5, e2);
  GtsFace * f3 = gts_face_new (gts_face_class (),
			       e6, e7, e8);
  GtsFace * f4 = gts_face_new (gts_face_class (),
			       e9, e3, e10);
  GtsFace * f5 = gts_face_new (gts_face_class (),
			       e11, e12, e5);
  GtsFace * f6 = gts_face_new (gts_face_class (),
			       e13, e10, e14);
  GtsFace * f7 = gts_face_new (gts_face_class (),
			       e15, e7, e16);
  GtsFace * f8 = gts_face_new (gts_face_class (),
			       e17, e18, e14);
  GtsFace * f9 = gts_face_new (gts_face_class (),
			       e19, e20, e15);
  GtsFace * f10 = gts_face_new (gts_face_class (),
                                e21, e12, e8);
  GtsFace * f11 = gts_face_new (gts_face_class (),
                                e21, e20, e17);

  GtsVolumeOptimizedParams params = { 0.5, 0.5, 0. };
  
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
  gts_surface_add_face (surface, f11);

  gts_surface_print_stats (surface, stderr);
  fprintf (stderr, "volume: %g\n", gts_surface_volume (surface));

#if 0
  v = edge_collapse (surface, e21, 
		     (GtsCoarsenFunc) gts_volume_optimized_vertex, 
		     &params);
#else
  v = gts_volume_optimized_vertex (e21, gts_vertex_class (), &params);
fprintf (stderr, "v: %.10g %.10g %.10g\n",
	 GTS_POINT (v)->x, GTS_POINT (v)->y, GTS_POINT (v)->z);
  fprintf (stderr, "before: check for folds...\n");
  gts_edge_collapse_creates_fold (e21, v, 0.999695413509);
  v = edge_collapse (surface, e21, 
		     (GtsCoarsenFunc) gts_volume_optimized_vertex, 
		     &params);
  fprintf (stderr, "after: check for folds...\n");
  {
    GSList * i = v->segments;
    while (i) {
      GtsEdge * e = i->data;
      gts_triangles_are_folded (e->triangles,  
				GTS_SEGMENT (e)->v1,
				GTS_SEGMENT (e)->v2,
				0.999695413509);
      i = i->next;
    }
  }
#endif

  gts_surface_print_stats (surface, stderr);
  fprintf (stderr, "volume: %g\n", gts_surface_volume (surface));

  gts_surface_write (surface, stdout);

  return 0;
}
