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
#include <math.h>
#include "config.h"
#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#endif /* HAVE_GETOPT_H */
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include "gts.h"

#define HEAP_INSERT_EDGE(h, e) (GTS_OBJECT (e)->reserved = gts_eheap_insert (h, e))
#define HEAP_REMOVE_EDGE(h, e) (gts_eheap_remove (h, GTS_OBJECT (e)->reserved),\
                                GTS_OBJECT (e)->reserved = NULL)

static gdouble triangles_angle (GtsPoint * p1, GtsPoint * p2, 
				GtsPoint * p3, GtsPoint * p4)
{ 
  gdouble x1, y1, z1, x2, y2, z2;
  gdouble nx1, ny1, nz1, nx2, ny2, nz2;
  gdouble pvx, pvy, pvz;
  gdouble theta;

  x1 = p2->x - p1->x;
  y1 = p2->y - p1->y;
  z1 = p2->z - p1->z;

  x2 = p3->x - p1->x;
  y2 = p3->y - p1->y;
  z2 = p3->z - p1->z;

  nx1 = y1*z2 - z1*y2;
  ny1 = z1*x2 - x1*z2;
  nz1 = x1*y2 - y1*x2;

  x1 = p1->x - p2->x;
  y1 = p1->y - p2->y;
  z1 = p1->z - p2->z;

  x2 = p4->x - p2->x;
  y2 = p4->y - p2->y;
  z2 = p4->z - p2->z;

  nx2 = y1*z2 - z1*y2;
  ny2 = z1*x2 - x1*z2;
  nz2 = x1*y2 - y1*x2;

  pvx = ny1*nz2 - nz1*ny2;
  pvy = nz1*nx2 - nx1*nz2;
  pvz = nx1*ny2 - ny1*nx2;

  theta = atan2 (sqrt (pvx*pvx + pvy*pvy + pvz*pvz), 
		 nx1*nx2 + ny1*ny2 + nz1*nz2) - M_PI;
  return theta < - M_PI ? theta + 2.*M_PI : theta;
  
}

static gdouble edge_swap_cost (GtsEdge * e)
{
  GSList * i;
  GtsTriangle * t1 = NULL, * t2 = NULL;
  GtsVertex * v1, * v2, * v3, * v4;
  GtsEdge * e1, * e2, * e3, * e4;
  gdouble ab, aa;

  i = e->triangles;
  while (i) {
    if (GTS_IS_FACE (i->data)) {
      if (!t1) t1 = i->data;
      else if (!t2) t2 = i->data;
      else return G_MAXDOUBLE;
    }
    i = i->next;
  }
  if (!t1 || !t2)
    return G_MAXDOUBLE;

  gts_triangle_vertices_edges (t1, e, &v1, &v2, &v3, &e, &e3, &e4);
  gts_triangle_vertices_edges (t2, e, &v2, &v1, &v4, &e, &e1, &e2);

  ab = triangles_angle (GTS_POINT (v1), GTS_POINT (v2), 
			GTS_POINT (v3), GTS_POINT (v4));
  aa = triangles_angle (GTS_POINT (v3), GTS_POINT (v4), 
			GTS_POINT (v2), GTS_POINT (v1));
  return fabs (ab) - fabs (aa);
}

static void edge_swap (GtsEdge * e, GtsSurface * s, GtsEHeap * heap)
{
  GSList * i;
  GtsTriangle * t1 = NULL, * t2 = NULL;
  GtsVertex * v1, * v2, * v3, * v4;
  GtsEdge * e1, * e2, * e3, * e4;

  i = e->triangles;
  while (i) {
    if (GTS_IS_FACE (i->data)) {
      if (!t1) t1 = i->data;
      else if (!t2) t2 = i->data;
      else g_assert_not_reached ();
    }
    i = i->next;
  }
  g_assert (t1 && t2);

  gts_triangle_vertices_edges (t1, e, &v1, &v2, &v3, &e, &e3, &e4);
  gts_triangle_vertices_edges (t2, e, &v2, &v1, &v4, &e, &e1, &e2);

  gts_object_destroy (GTS_OBJECT (e));
  e = gts_edge_new (s->edge_class, v3, v4);
  gts_surface_add_face (s, gts_face_new (s->face_class, e, e4, e1));
  gts_surface_add_face (s, gts_face_new (s->face_class, e, e2, e3));
  
  HEAP_INSERT_EDGE (heap, e);
  HEAP_REMOVE_EDGE (heap, e1);
  HEAP_INSERT_EDGE (heap, e1);
  HEAP_REMOVE_EDGE (heap, e2);
  HEAP_INSERT_EDGE (heap, e2);
  HEAP_REMOVE_EDGE (heap, e3);
  HEAP_INSERT_EDGE (heap, e3);
  HEAP_REMOVE_EDGE (heap, e4);
  HEAP_INSERT_EDGE (heap, e4);
}

static void create_heap_optimize (GtsEdge * e, GtsEHeap * heap)
{
  HEAP_INSERT_EDGE (heap, e);
}

static void surface_optimize (GtsSurface * surface,
			      gdouble max_cost)
{
  GtsEHeap * heap;
  GtsEdge * e;
  gdouble top_cost;

  heap = gts_eheap_new ((GtsKeyFunc) edge_swap_cost, NULL);
  gts_eheap_freeze (heap);
  gts_surface_foreach_edge (surface, (GtsFunc) create_heap_optimize, heap);
  gts_eheap_thaw (heap);

  gts_allow_floating_edges = TRUE;
  while ((e = gts_eheap_remove_top (heap, &top_cost)) &&
	 top_cost < max_cost)
    edge_swap (e, surface, heap);
  gts_allow_floating_edges = FALSE;

  if (e) GTS_OBJECT (e)->reserved = NULL;
  gts_eheap_foreach (heap, (GFunc) gts_object_reset_reserved, NULL);

  gts_eheap_destroy (heap);
}

static void angle_stats (GtsEdge * e, GtsRange * angle)
{
  GSList * i;
  GtsTriangle * t1 = NULL, * t2 = NULL;

  i = e->triangles;
  while (i) {
    if (GTS_IS_FACE (i->data)) {
      if (!t1) t1 = i->data;
      else if (!t2) t2 = i->data;
      else return;
    }
    i = i->next;
  }
  if (!t1 || !t2)
    return;

  gts_range_add_value (angle, fabs (gts_triangles_angle (t1, t2)));
}

int main (int argc, char * argv[])
{
  GtsSurface * s;
  gboolean verbose = FALSE;
  gdouble threshold;
  int c = 0;
  GtsFile * fp;
  GtsRange angle;

  /* parse options using getopt */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hv", 
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hv"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'v': /* verbose */
      verbose = TRUE;
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: optimize [OPTION] THRESHOLD < FILE\n"
	     "\n"
	     "  -v      --verbose  print statistics about the surface\n"
	     "  -h      --help     display this help and exit\n"
	     "\n"
	     "Report bugs to %s\n",
	     GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `optimize --help' for more information.\n");
      return 1; /* failure */
    }
  }

  if (optind >= argc) { /* missing threshold */
    fprintf (stderr, 
	     "optimize: missing THRESHOLD\n"
	     "Try `optimize --help' for more information.\n");
    return 1; /* failure */
  }
  threshold = atof (argv[optind]);

  if (threshold < 0.0) { /* threshold must be positive */
     fprintf (stderr, 
	     "optimize: THRESHOLD must be >= 0.0\n"
	     "Try `optimize --help' for more information.\n");
    return 1; /* failure */
  }

  /* read surface in */
  s = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());
  fp = gts_file_new (stdin);
  if (gts_surface_read (s, fp)) {
    fputs ("optimize: file on standard input is not a valid GTS file\n", 
	   stderr);
    fprintf (stderr, "stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
    return 1; /* failure */
  }

  /* if verbose on print stats */
  if (verbose) {
    gts_surface_print_stats (s, stderr);
    gts_range_init (&angle);
    gts_surface_foreach_edge (s, (GtsFunc) angle_stats, &angle);
    gts_range_update (&angle);
    fputs ("#   angle : ", stderr);
    gts_range_print (&angle, stderr);
    fputc ('\n', stderr);
  }

  surface_optimize (s, -threshold);

  /* if verbose on print stats */
  if (verbose) {
    gts_surface_print_stats (s, stderr);
    gts_range_init (&angle);
    gts_surface_foreach_edge (s, (GtsFunc) angle_stats, &angle);
    gts_range_update (&angle);
    fputs ("#   angle : ", stderr);
    gts_range_print (&angle, stderr);
    fputc ('\n', stderr);
  }

  /* write surface */
  gts_surface_write (s, stdout);

  return 0; /* success */
}
