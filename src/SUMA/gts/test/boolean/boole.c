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

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "gts.h"

static void prepend_triangle_bbox (GtsTriangle * t, GSList ** bboxes)
{
  *bboxes = g_slist_prepend (*bboxes, 
			     gts_bbox_triangle (gts_bbox_class (), t));
}

static void check_edge (GtsEdge * e, gpointer * data)
{
  GtsSurface * s1 = data[0];
  GtsSurface * s2 = data[1];
  guint * nf = data[2];

  if (gts_edge_is_boundary (e, s1) &&
      !gts_edge_is_boundary (e, s2)) {
    GtsPoint * p1 = GTS_POINT (GTS_SEGMENT (e)->v1);
    GtsPoint * p2 = GTS_POINT (GTS_SEGMENT (e)->v2);

    printf ("VECT 1 2 0 2 0 %g %g %g %g %g %g\n",
	    p1->x, p1->y, p1->z,
	    p2->x, p2->y, p2->z);
    (*nf)++;
  }
}

int main (int argc, char * argv[])
{
  GtsSurface * s1, * s2;
  GtsSurface * s1out2, * s1in2, * s2out1, * s2in1;
  GtsSurfaceInter * si;
  GNode * tree1, * tree2;
  gboolean is_open1, is_open2;
  GSList * bboxes;
  FILE * fptr;
  GtsFile * fp;
  guint n;
  gboolean closed = TRUE;

  if (argc != 3) {
    fprintf (stderr, 
	     "%s: test boolean operations between surfaces\n"
	     "usage: %s FILE1 FILE2\n",
	     argv[0], argv[0]);
    return 1;
  }

  /* open first file */
  if ((fptr = fopen (argv[1], "rt")) == NULL) {
    fprintf (stderr, "%s: can not open file `%s'\n", argv[0], argv[2]);
    return 1;
  }
  /* reads in first surface file */
  s1 = gts_surface_new (gts_surface_class (),
			GTS_FACE_CLASS (gts_nface_class ()),
			GTS_EDGE_CLASS (gts_nedge_class ()),
			GTS_VERTEX_CLASS (gts_nvertex_class ()));
  fp = gts_file_new (fptr);
  if (gts_surface_read (s1, fp)) {
    fprintf (stderr, "boole: `%s' is not a valid GTS surface file\n", 
	     argv[1]);
    fprintf (stderr, "%s:%d:%d: %s\n", argv[1], fp->line, fp->pos, fp->error);
    return 1;
  }
  gts_file_destroy (fp);
  fclose (fptr);

 /* open second file */
  if ((fptr = fopen (argv[2], "rt")) == NULL) {
    fprintf (stderr, "%s: can not open file `%s'\n", argv[0], argv[3]);
    return 1;
  }
  /* reads in second surface file */
  s2 = gts_surface_new (gts_surface_class (),
			GTS_FACE_CLASS (gts_nface_class ()),
			GTS_EDGE_CLASS (gts_nedge_class ()),
			GTS_VERTEX_CLASS (gts_nvertex_class ()));
  fp = gts_file_new (fptr);
  if (gts_surface_read (s2, fp)) {
    fprintf (stderr, "boole: `%s' is not a valid GTS surface file\n", 
	     argv[2]);
    fprintf (stderr, "%s:%d:%d: %s\n", argv[2], fp->line, fp->pos, fp->error);
    return 1;
  }
  gts_file_destroy (fp);
  fclose (fptr);

  /* display summary information about both surfaces */
#if 0
  gts_surface_print_stats (s1, stderr);
  gts_surface_print_stats (s2, stderr);
#endif

  /* check surfaces */
  g_assert (gts_surface_is_orientable (s1));
  g_assert (gts_surface_is_orientable (s2));
  g_assert (!gts_surface_is_self_intersecting (s1));
  g_assert (!gts_surface_is_self_intersecting (s2));

  /* build bounding boxes for first surface */
  bboxes = NULL;
  gts_surface_foreach_face (s1, (GtsFunc) prepend_triangle_bbox, &bboxes);
  /* build bounding box tree for first surface */
  tree1 = gts_bb_tree_new (bboxes);
  /* free list of bboxes */
  g_slist_free (bboxes);
  is_open1 = gts_surface_volume (s1) < 0. ? TRUE : FALSE;

  /* build bounding boxes for second surface */
  bboxes = NULL;
  gts_surface_foreach_face (s2, (GtsFunc) prepend_triangle_bbox, &bboxes);
  /* build bounding box tree for second surface */
  tree2 = gts_bb_tree_new (bboxes);
  /* free list of bboxes */
  g_slist_free (bboxes);
  is_open2 = gts_surface_volume (s2) < 0. ? TRUE : FALSE;

  si = gts_surface_inter_new (gts_surface_inter_class (), 
			      s1, s2, tree1, tree2, is_open1, is_open2);
  g_assert (gts_surface_inter_check (si, &closed));

  s1out2 = gts_surface_new (gts_surface_class (),
			    gts_face_class (),
			    gts_edge_class (),
			    gts_vertex_class ());
  s1in2 = gts_surface_new (gts_surface_class (),
			   gts_face_class (),
			   gts_edge_class (),
			   gts_vertex_class ());
  s2out1 = gts_surface_new (gts_surface_class (),
			    gts_face_class (),
			    gts_edge_class (),
			    gts_vertex_class ());
  s2in1 = gts_surface_new (gts_surface_class (),
			   gts_face_class (),
			   gts_edge_class (),
			   gts_vertex_class ());
  if (closed) {
    GtsSurfaceStats st1out2, st1in2, st2out1, st2in1;
    gpointer data[3];
    guint nf = 0;
    gdouble a, ain, aout;

    gts_surface_inter_boolean (si, s1out2, GTS_1_OUT_2);
    gts_surface_inter_boolean (si, s1in2, GTS_1_IN_2);  
    gts_surface_inter_boolean (si, s2out1, GTS_2_OUT_1);
    gts_surface_inter_boolean (si, s2in1, GTS_2_IN_1);

    gts_surface_stats (s1out2, &st1out2);
    fprintf (stderr, "----------- 1 out 2 -----------\n");
    gts_surface_print_stats (s1out2, stderr);
    g_assert (st1out2.n_incompatible_faces == 0 &&
    	      st1out2.n_non_manifold_edges == 0);
    
    gts_surface_stats (s1in2, &st1in2);
    fprintf (stderr, "----------- 1 in  2 -----------\n");
    gts_surface_print_stats (s1in2, stderr);
    g_assert (st1in2.n_incompatible_faces == 0 &&
    	      st1in2.n_non_manifold_edges == 0 &&
    	      st1in2.n_boundary_edges == st1out2.n_boundary_edges);
    a = gts_surface_area (s1);
    aout = gts_surface_area (s1out2);
    ain = gts_surface_area (s1in2);
    if (a > 0.) {
      a = fabs (a - aout - ain)/a;
      g_assert (a < 1e-9);
    }

    gts_surface_stats (s2out1, &st2out1);
    fprintf (stderr, "----------- 2 out 1 -----------\n");
    gts_surface_print_stats (s2out1, stderr);
    g_assert (st2out1.n_incompatible_faces == 0 &&
    	      st2out1.n_non_manifold_edges == 0);
    
    gts_surface_stats (s2in1, &st2in1);
    fprintf (stderr, "----------- 2 in  1 -----------\n");
    gts_surface_print_stats (s2in1, stderr);
    g_assert (st2in1.n_incompatible_faces == 0 &&
    	      st2in1.n_non_manifold_edges == 0 &&
   	      st2in1.n_boundary_edges == st2out1.n_boundary_edges);
    a = gts_surface_area (s2);
    aout = gts_surface_area (s2out1);
    ain = gts_surface_area (s2in1);
    if (a > 0.) {
      a = fabs (a - aout - ain)/a;
      g_assert (a < 1e-9);
    }
    
    n = g_slist_length (si->edges);
    g_assert (n == st1in2.n_boundary_edges &&
    	      n == st2in1.n_boundary_edges);

    data[0] = s1out2;
    data[1] = s1in2;
    data[2] = &nf;
    printf ("(geometry \"s1 failed\" = LIST {\n");
    gts_surface_foreach_edge (s1out2, (GtsFunc) check_edge, data);
    printf ("})\n");

    data[0] = s2out1;
    data[1] = s2in1;
    data[2] = &nf;
    printf ("(geometry \"s2 failed\" = LIST {\n");
    gts_surface_foreach_edge (s2out1, (GtsFunc) check_edge, data);
    printf ("})\n");

    g_assert (nf == 0);
  }
  else {
    fprintf (stderr, "----------- 1 out 2 -----------\n");
    gts_surface_print_stats (s1out2, stderr);
    
    fprintf (stderr, "----------- 1 in  2 -----------\n");
    gts_surface_print_stats (s1in2, stderr);
    
    fprintf (stderr, "----------- 2 out 1 -----------\n");
    gts_surface_print_stats (s2out1, stderr);
    
    fprintf (stderr, "----------- 2 in  1 -----------\n");
    gts_surface_print_stats (s2in1, stderr);
  }

  printf ("(geometry \"1out2\" { =\n");
  gts_surface_write_oogl (s1out2, stdout);
  printf ("})\n"
	  "(normalization \"1out2\" none)\n");
  
  printf ("(geometry \"1in2\" { =\n");
  gts_surface_write_oogl (s1in2, stdout);
  printf ("})\n"
	  "(normalization \"1in2\" none)\n");
  
  printf ("(geometry \"2out1\" { =\n");
  gts_surface_write_oogl (s2out1, stdout);
  printf ("})\n"
	  "(normalization \"2out1\" none)\n");
  
  printf ("(geometry \"2in1\" { =\n");
  gts_surface_write_oogl (s2in1, stdout);
  printf ("})\n"
	  "(normalization \"2in1\" none)\n");
  
  /* destroy surfaces and intersection */
  gts_object_destroy (GTS_OBJECT (s1));
  gts_object_destroy (GTS_OBJECT (s2));
  gts_object_destroy (GTS_OBJECT (si));

  /* destroy bounding box trees (including bounding boxes) */
  gts_bb_tree_destroy (tree1, TRUE);
  gts_bb_tree_destroy (tree2, TRUE);  

  return 0;
}
