#include "gts.h"

int main (int argc, char * argv[])
{
  GtsSurface * surface = gts_surface_new (gts_surface_class (),
                                          gts_face_class (),
                                          gts_edge_class (),
                                          gts_vertex_class ());

  GtsVertex * v1 = gts_vertex_new (gts_vertex_class (), 0, 0, 0);
  GtsVertex * v2 = gts_vertex_new (gts_vertex_class (), 0, -0.5, 1);
  GtsVertex * v3 = gts_vertex_new (gts_vertex_class (), 0, 0.5, 1);
  GtsVertex * v4 = gts_vertex_new (gts_vertex_class (), 1, 0, 0);
  GtsVertex * v5 = gts_vertex_new (gts_vertex_class (), 0, -0.5, -1);
  GtsVertex * v6 = gts_vertex_new (gts_vertex_class (), 1, -0.5, -1);
  GtsVertex * v7 = gts_vertex_new (gts_vertex_class (), 1, 0.5, -1);
  GtsVertex * v8 = gts_vertex_new (gts_vertex_class (), 1, -0.5, 1);
  GtsVertex * v9 = gts_vertex_new (gts_vertex_class (), 1, 0.5, 1);
  GtsVertex * v10 = gts_vertex_new (gts_vertex_class (), 0, 0.5, -1);

  GtsEdge * e1 = gts_edge_new (gts_edge_class (), v1, v2);
  GtsEdge * e2 = gts_edge_new (gts_edge_class (), v1, v3);
  GtsEdge * e3 = gts_edge_new (gts_edge_class (), v2, v3);
  GtsEdge * e4 = gts_edge_new (gts_edge_class (), v4, v5);
  GtsEdge * e5 = gts_edge_new (gts_edge_class (), v4, v6);
  GtsEdge * e6 = gts_edge_new (gts_edge_class (), v5, v6);
  GtsEdge * e7 = gts_edge_new (gts_edge_class (), v1, v4);
  GtsEdge * e8 = gts_edge_new (gts_edge_class (), v1, v5);
  GtsEdge * e9 = gts_edge_new (gts_edge_class (), v6, v7);
  GtsEdge * e10 = gts_edge_new (gts_edge_class (), v4, v7);
  GtsEdge * e11 = gts_edge_new (gts_edge_class (), v1, v7);
  GtsEdge * e12 = gts_edge_new (gts_edge_class (), v8, v9);
  GtsEdge * e13 = gts_edge_new (gts_edge_class (), v4, v9);
  GtsEdge * e14 = gts_edge_new (gts_edge_class (), v4, v8);
  GtsEdge * e15 = gts_edge_new (gts_edge_class (), v5, v10);
  GtsEdge * e16 = gts_edge_new (gts_edge_class (), v1, v10);
  GtsEdge * e17 = gts_edge_new (gts_edge_class (), v10, v7);
  GtsEdge * e18 = gts_edge_new (gts_edge_class (), v1, v8);
  GtsEdge * e19 = gts_edge_new (gts_edge_class (), v2, v8);
  GtsEdge * e20 = gts_edge_new (gts_edge_class (), v4, v3);
  GtsEdge * e21 = gts_edge_new (gts_edge_class (), v3, v9);

  GtsFace * f1 = gts_face_new (gts_face_class (),
                                e1, e2, e3);
  GtsFace * f2 = gts_face_new (gts_face_class (),
                                e4, e5, e6);
  GtsFace * f3 = gts_face_new (gts_face_class (),
                                e7, e4, e8);
  GtsFace * f4 = gts_face_new (gts_face_class (),
                                e9, e5, e10);
  GtsFace * f5 = gts_face_new (gts_face_class (),
                                e11, e10, e7);
  GtsFace * f6 = gts_face_new (gts_face_class (),
                                e12, e13, e14);
  GtsFace * f7 = gts_face_new (gts_face_class (),
                                e15, e16, e8);
  GtsFace * f8 = gts_face_new (gts_face_class (),
                                e17, e11, e16);
  GtsFace * f9 = gts_face_new (gts_face_class (),
                                e18, e14, e7);
  GtsFace * f10 = gts_face_new (gts_face_class (),
                                e19, e18, e1);
  GtsFace * f11 = gts_face_new (gts_face_class (),
                                e20, e13, e21);
  GtsFace * f12 = gts_face_new (gts_face_class (),
                                e7, e20, e2);
  GtsVertex * v;
  GtsVolumeOptimizedParams params = { 0.5, 0.5, 0.0 };
  GtsSplit * vs;

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
  gts_surface_add_face (surface, f12);

  g_assert (gts_edge_collapse_is_valid (e7));
  v = gts_volume_optimized_vertex (e7, gts_vertex_class (), &params);
  vs = gts_split_new (gts_split_class (), v, 
		      GTS_OBJECT (GTS_SEGMENT (e7)->v1),
		      GTS_OBJECT (GTS_SEGMENT (e7)->v2));
  gts_split_collapse (vs, gts_edge_class (), NULL);

  gts_surface_write (surface, stdout);

  return 0;
}
