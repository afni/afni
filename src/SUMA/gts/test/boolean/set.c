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
#include "gts.h"

static void prepend_triangle_bbox (GtsTriangle * t, GSList ** bboxes)
{
  *bboxes = g_slist_prepend (*bboxes, 
			     gts_bbox_triangle (gts_bbox_class (), t));
}

static gboolean segment_is_ok (GtsSegment * s)
{
  g_return_val_if_fail (s != NULL, FALSE);
  g_return_val_if_fail (s->v1 != s->v2, FALSE);
  g_assert (GTS_OBJECT (s)->reserved == NULL);
  return TRUE;
}

static gboolean vertex_is_ok (GtsVertex * v)
{
  g_return_val_if_fail (v != NULL, FALSE);
  g_return_val_if_fail (GTS_OBJECT (v)->reserved == NULL, FALSE);
  g_assert (GTS_OBJECT (v)->reserved == NULL);
  return TRUE;
}

static gboolean triangle_is_ok (GtsTriangle * t)
{
  g_return_val_if_fail (t != NULL, FALSE);
  g_return_val_if_fail (t->e1 != NULL, FALSE);
  g_return_val_if_fail (t->e2 != NULL, FALSE);
  g_return_val_if_fail (t->e3 != NULL, FALSE);
  g_return_val_if_fail (t->e1 != t->e2 && t->e1 != t->e3 && t->e2 != t->e3, 
			FALSE);
  g_assert (gts_segments_touch (GTS_SEGMENT (t->e1), 
				GTS_SEGMENT (t->e2)));
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
  return TRUE;
}

static void write_segment (GtsSegment * s, FILE * fp)
{
  fprintf (fp, "# %p %p:%s->%p:%s\n"
	   "VECT 1 2 0 2 0 %g %g %g %g %g %g\n",
	   s,
	   s->v1,
	   GTS_NVERTEX (s->v1)->name,
	   s->v2,
	   GTS_NVERTEX (s->v2)->name,
	   GTS_POINT (s->v1)->x, 
	   GTS_POINT (s->v1)->y, 
	   GTS_POINT (s->v1)->z, 
	   GTS_POINT (s->v2)->x, 
	   GTS_POINT (s->v2)->y, 
	   GTS_POINT (s->v2)->z);
}

int main (int argc, char * argv[])
{
  GtsSurface * s1, * s2;
  GtsSurfaceInter * si;
  GNode * tree1, * tree2;
  gboolean is_open1, is_open2;
  GSList * bboxes;
  FILE * fptr;
  GtsFile * fp;

  if (argc != 3) {
    fprintf (stderr, 
	     "%s: check set operations between surfaces\n"
	     "usage: %s FILE1 FILE2\n",
	     argv[0], argv[0]);
    return 1;
  }

  /* open first file */
  if ((fptr = fopen (argv[1], "rt")) == NULL) {
    fprintf (stderr, "%s: can not open file `%s'\n", argv[0], argv[1]);
    return 1;
  }
  /* reads in first surface file */
  s1 = gts_surface_new (gts_surface_class (),
			GTS_FACE_CLASS (gts_nface_class ()),
			GTS_EDGE_CLASS (gts_nedge_class ()),
			GTS_VERTEX_CLASS (gts_nvertex_class ()));
  fp = gts_file_new (fptr);
  if (gts_surface_read (s1, fp)) {
    fprintf (stderr, "set: `%s' is not a valid GTS surface file\n", 
	     argv[1]);
    fprintf (stderr, "%s:%d:%d: %s\n", argv[1], fp->line, fp->pos, fp->error);
    return 1;
  }
  gts_file_destroy (fp);
  fclose (fptr);

 /* open second file */
  if ((fptr = fopen (argv[2], "rt")) == NULL) {
    fprintf (stderr, "%s: can not open file `%s'\n", argv[0], argv[2]);
    return 1;
  }
  /* reads in second surface file */
  s2 = gts_surface_new (gts_surface_class (),
			GTS_FACE_CLASS (gts_nface_class ()),
			GTS_EDGE_CLASS (gts_nedge_class ()),
			GTS_VERTEX_CLASS (gts_nvertex_class ()));
  fp = gts_file_new (fptr);
  if (gts_surface_read (s2, fp)) {
    fprintf (stderr, "set: `%s' is not a valid GTS surface file\n", 
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

#if 1
  {
    GtsSurface * s = gts_surface_new (gts_surface_class (),
				      gts_face_class (),
				      gts_edge_class (),
				      gts_vertex_class ());
    gboolean closed = TRUE;

    si = gts_surface_inter_new (gts_surface_inter_class (), 
				s1, s2, tree1, tree2, is_open1, is_open2);

    gts_surface_merge (s, si->s1);
    gts_surface_merge (s, si->s2);
    gts_surface_print_stats (s, stderr);
#if 1
    printf ("(geometry \"inter\" { = LIST {\n");
    g_slist_foreach (si->edges, (GFunc) write_segment, stdout);
    printf ("}})\n"
	    "(normalization \"inter\" none)\n");
    printf ("(geometry \"s1\" { =\n");
    gts_surface_write_oogl (si->s1, stdout);
    printf ("})\n"
	    "(normalization \"s1\" none)\n");
    printf ("(geometry \"s2\" { =\n");
    gts_surface_write_oogl (si->s2, stdout);
    printf ("})\n"
	    "(normalization \"s2\" none)\n");
#else
#if 1
    {
      GtsGraph * g;

      g = gts_segments_graph_new (gts_graph_class (), si->edges);
      gts_graph_write_dot (g, stdout);
      gts_object_destroy (GTS_OBJECT (g));
    }
#else
    {
      GtsGraph * g = gts_surface_graph_new (gts_graph_class (), s);

      gts_graph_write_dot (g, stdout);
      gts_object_destroy (GTS_OBJECT (g));
    }
#endif
#endif
    gts_object_destroy (GTS_OBJECT (s));
#if 1
    gts_surface_foreach_face (s1, (GtsFunc) triangle_is_ok, NULL);
    gts_surface_foreach_edge (s1, (GtsFunc) segment_is_ok, NULL);
    gts_surface_foreach_edge (s1, (GtsFunc) vertex_is_ok, NULL);

    gts_surface_foreach_face (s2, (GtsFunc) triangle_is_ok, NULL);
    gts_surface_foreach_edge (s2, (GtsFunc) segment_is_ok, NULL);
    gts_surface_foreach_edge (s2, (GtsFunc) vertex_is_ok, NULL);

    gts_surface_foreach_face (si->s1, (GtsFunc) triangle_is_ok, NULL);
    gts_surface_foreach_edge (si->s1, (GtsFunc) segment_is_ok, NULL);
    gts_surface_foreach_edge (si->s1, (GtsFunc) vertex_is_ok, NULL);

    gts_surface_foreach_face (si->s2, (GtsFunc) triangle_is_ok, NULL);
    gts_surface_foreach_edge (si->s2, (GtsFunc) segment_is_ok, NULL);
    gts_surface_foreach_edge (si->s2, (GtsFunc) vertex_is_ok, NULL);
#endif
    g_assert (gts_surface_inter_check (si, &closed));
  }
#else
  si = gts_surface_inter_new (gts_surface_inter_class (), 
			      s1, s2, tree1, tree2);
#endif

  /* destroy surfaces and intersection */
  gts_object_destroy (GTS_OBJECT (s1));
  gts_object_destroy (GTS_OBJECT (s2));
  gts_object_destroy (GTS_OBJECT (si));

  /* destroy bounding box trees (including bounding boxes) */
  gts_bb_tree_destroy (tree1, TRUE);
  gts_bb_tree_destroy (tree2, TRUE);  

  /* free GTS memory (for memory profiling) */
  gts_finalize ();

  return 0;
}
