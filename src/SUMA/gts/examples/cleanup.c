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
#include "config.h"
#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#endif /* HAVE_GETOPT_H */
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include "gts.h"

static void build_list (gpointer data, GSList ** list)
{
  /* always use O(1) g_slist_prepend instead of O(n) g_slist_append */
  *list = g_slist_prepend (*list, data);
}

static void build_list1 (gpointer data, GList ** list)
{
  /* always use O(1) g_list_prepend instead of O(n) g_list_append */
  *list = g_list_prepend (*list, data);
}

static void vertex_cleanup (GtsVertex * v)
{
  gts_vertex_is_contact (v, TRUE);
}

static void edge_cleanup (GtsSurface * surface)
{
  GSList * edges = NULL;
  GSList * i;

  g_return_if_fail (surface != NULL);

  /* build list of edges */
  gts_surface_foreach_edge (surface, (GtsFunc) build_list, &edges);

  /* remove degenerate and duplicate edges.
     Note: we could use gts_edges_merge() to remove the duplicates and then
     remove the degenerate edges but it is more efficient to do everything 
     at once (and it's more pedagogical too ...) */

  /* We want to control manually the destruction of edges */
  gts_allow_floating_edges = TRUE;

  i = edges;
  while (i) {
    GtsEdge * e = i->data;
    GtsEdge * duplicate;
    if (GTS_SEGMENT (e)->v1 == GTS_SEGMENT (e)->v2) /* edge is degenerate */
      /* destroy e */
      gts_object_destroy (GTS_OBJECT (e));
    else if ((duplicate = gts_edge_is_duplicate (e))) {
      /* replace e with its duplicate */
      gts_edge_replace (e, duplicate);
      /* destroy e */
      gts_object_destroy (GTS_OBJECT (e));
    }
    i = i->next;
  }
  
  /* don't forget to reset to default */
  gts_allow_floating_edges = FALSE;

  /* free list of edges */
  g_slist_free (edges);
}

static void triangle_cleanup (GtsSurface * s)
{
  GSList * triangles = NULL;
  GSList * i;

  g_return_if_fail (s != NULL);

  /* build list of triangles */
  gts_surface_foreach_face (s, (GtsFunc) build_list, &triangles);
  
  /* remove duplicate triangles */
  i = triangles;
  while (i) {
    GtsTriangle * t = i->data;
    if (gts_triangle_is_duplicate (t))
      /* destroy t, its edges (if not used by any other triangle)
	 and its corners (if not used by any other edge) */
      gts_object_destroy (GTS_OBJECT (t));
    i = i->next;
  }
  
  /* free list of triangles */
  g_slist_free (triangles);
}

/* cleanup - using a given threshold merge vertices which are too close.
   Eliminate degenerate and duplicate edges.
   Eliminate duplicate triangles . */
int main (int argc, char * argv[])
{
  GtsSurface * s, * m;
  GList * vertices = NULL;
  gboolean verbose = FALSE, sever = FALSE;
  gdouble threshold;
  int c = 0;
  GtsFile * fp;

  s = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());

  /* parse options using getopt */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"merge", required_argument, NULL, 'm'},
      {"sever", no_argument, NULL, 's'},
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hvsm:", 
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hvsm:"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 's': /* sever */
      sever = TRUE;
      break;
    case 'm': { /* merge */
      FILE * fptr = fopen (optarg, "rt");
      GtsFile * fp;

      if (fptr == NULL) {
	fprintf (stderr, "cleanup: cannot open file `%s' for merging\n", 
		 optarg);
	return 1; /* failure */
      }
      m = gts_surface_new (gts_surface_class (),
			   gts_face_class (),
			   gts_edge_class (),
			   gts_vertex_class ());
      fp = gts_file_new (fptr);
      if (gts_surface_read (m, fp)) {
	fprintf (stderr, "cleanup: file `%s' is not a valid GTS file\n", 
		 optarg);
	fprintf (stderr, "%s:%d:%d: %s\n", 
		 optarg, fp->line, fp->pos, fp->error);
	return 1; /* failure */
      }
      gts_file_destroy (fp);
      fclose (fptr);
      gts_surface_merge (s, m);
      gts_object_destroy (GTS_OBJECT (m));      
      break;
    }
    case 'v': /* verbose */
      verbose = TRUE;
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: cleanup [OPTION] THRESHOLD < FILE\n"
	     "Merge vertices of the GTS surface FILE if they are closer than THRESHOLD,\n"
	     "eliminate degenerate, duplicate edges and duplicate triangles.\n"
	     "\n"
	     "  -s      --sever    sever \"contact\" vertices\n"
	     "  -m FILE --merge    merge surface FILE\n"
	     "  -v      --verbose  print statistics about the surface\n"
	     "  -h      --help     display this help and exit\n"
	     "\n"
	     "Report bugs to %s\n",
	     GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `cleanup --help' for more information.\n");
      return 1; /* failure */
    }
  }

  if (optind >= argc) { /* missing threshold */
    fprintf (stderr, 
	     "cleanup: missing THRESHOLD\n"
	     "Try `cleanup --help' for more information.\n");
    return 1; /* failure */
  }

  threshold = atof (argv[optind]);

  if (threshold < 0.0) { /* threshold must be positive */
     fprintf (stderr, 
	     "cleanup: THRESHOLD must be >= 0.0\n"
	     "Try `cleanup --help' for more information.\n");
    return 1; /* failure */
  }

  /* read surface in */
  m = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());
  fp = gts_file_new (stdin);
  if (gts_surface_read (m, fp)) {
    fputs ("cleanup: file on standard input is not a valid GTS file\n", 
	   stderr);
    fprintf (stderr, "stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
    return 1; /* failure */
  }
  gts_surface_merge (s, m);
  gts_object_destroy (GTS_OBJECT (m));

  /* if verbose on print stats */
  if (verbose) 
    gts_surface_print_stats (s, stderr);
 
  /* merge vertices which are close enough */
  /* build list of vertices */
  gts_surface_foreach_vertex (s, (GtsFunc) build_list1, &vertices);
  /* merge vertices: we MUST update the variable vertices because this function
     modifies the list (i.e. removes the merged vertices). */
  vertices = gts_vertices_merge (vertices, threshold);

  /* free the list */
  g_list_free (vertices);

  /* eliminate degenerate and duplicate edges */
  edge_cleanup (s);
  /* eliminate duplicate triangles */
  triangle_cleanup (s);

  if (sever)
    gts_surface_foreach_vertex (s, (GtsFunc) vertex_cleanup, NULL);

  /* if verbose on print stats */
  if (verbose) {
    GtsBBox * bb = gts_bbox_surface (gts_bbox_class (), s);
    gts_surface_print_stats (s, stderr);
    fprintf (stderr, "# Bounding box: [%g,%g,%g] [%g,%g,%g]\n",
	     bb->x1, bb->y1, bb->z1,
	     bb->x2, bb->y2, bb->z2);	     
  }

  /* write surface */
  gts_surface_write (s, stdout);

  return 0; /* success */
}
