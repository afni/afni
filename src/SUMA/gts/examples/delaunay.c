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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#endif /* HAVE_GETOPT_H */
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include "gts.h"

/* the file format is the classic GTS file format but only the vertex and 
 * edge sections are read. */
static guint read_list (GPtrArray * vertices,
			GtsFifo * constraints,
			GtsEdgeClass * edge_class,
			FILE * fptr)
{
  guint nv, ne, nt, i;
  guint line = 1;

  g_return_val_if_fail (vertices != NULL, 1);
  g_return_val_if_fail (constraints != NULL, 1);
  g_return_val_if_fail (edge_class != NULL, 1);
  g_return_val_if_fail (fptr != NULL, 1);

  if (fscanf (fptr, "%u %u %u", &nv, &ne, &nt) != 3)
    return line;
  line++;

  g_ptr_array_set_size (vertices, nv);
  i = 1;
  while (i <= nv) {
    gdouble x, y, z;

    if (fscanf (fptr, "%lf %lf %lf", &x, &y, &z) != 3)
      return line;
    line++;

    g_ptr_array_index (vertices, i++ - 1) = 
      gts_vertex_new (gts_vertex_class (), x, y, z);
  }
  
  i = 1;
  while (i <= ne) {
    guint iv1, iv2;

    if (fscanf (fptr, "%u %u", &iv1, &iv2) != 2 ||
	iv1 <= 0 || iv1 > nv || iv2 <= 0 || iv2 > nv)
      return line;
    line++;

    gts_fifo_push (constraints, 
		   gts_edge_new (edge_class,
				 g_ptr_array_index (vertices, iv1 - 1),
				 g_ptr_array_index (vertices, iv2 - 1)));
    i++;
  }

  return 0;
}

static gdouble triangle_cost (GtsTriangle * t, gpointer * data)
{
  gdouble * min_quality = data[0];
  gdouble * max_area = data[1];
  gdouble quality = gts_triangle_quality (t);
  gdouble area = gts_triangle_area (t);
  
  if (quality < *min_quality || area > *max_area)
    return quality;
  return 0.;
}

static gboolean triangle_is_hole (GtsTriangle * t)
{
  GtsEdge * e1, * e2, * e3;
  GtsVertex * v1, * v2, * v3;

  gts_triangle_vertices_edges (t, NULL, &v1, &v2, &v3, &e1, &e2, &e3);

  if ((GTS_IS_CONSTRAINT (e1) && GTS_SEGMENT (e1)->v1 != v1) ||
      (GTS_IS_CONSTRAINT (e2) && GTS_SEGMENT (e2)->v1 != v2) ||
      (GTS_IS_CONSTRAINT (e3) && GTS_SEGMENT (e3)->v1 != v3))
    return TRUE;
  return FALSE;
}

static guint delaunay_remove_holes (GtsSurface * surface)
{
  g_return_val_if_fail (surface != NULL, 0);

  return gts_surface_foreach_face_remove (surface, 
					  (GtsFunc) triangle_is_hole, NULL);
}

static void gts_constraint_split (GtsConstraint * c, 
				  GtsSurface * s,
				  GtsFifo * fifo)
{
  GSList * i;
  GtsVertex * v1, * v2;
  GtsEdge * e;

  g_return_if_fail (c != NULL);
  g_return_if_fail (s != NULL);

  v1 = GTS_SEGMENT (c)->v1;
  v2 = GTS_SEGMENT (c)->v2;
  e = GTS_EDGE (c);

  i = e->triangles;
  while (i) {
    GtsFace * f = i->data;
    if (GTS_IS_FACE (f) && gts_face_has_parent_surface (f, s)) {
      GtsVertex * v = gts_triangle_vertex_opposite (GTS_TRIANGLE (f), e);
      if (gts_point_orientation (GTS_POINT (v1), 
				 GTS_POINT (v2), 
				 GTS_POINT (v)) == 0.) {
	GSList * j = e->triangles;
	GtsFace * f1 = NULL;
	GtsEdge * e1, * e2;

	/* replaces edges with constraints */
	gts_triangle_vertices_edges (GTS_TRIANGLE (f), e,
				     &v1, &v2, &v, &e, &e1, &e2);
	if (!GTS_IS_CONSTRAINT (e1)) {
	  GtsEdge * ne1 = 
	    gts_edge_new (GTS_EDGE_CLASS (GTS_OBJECT (c)->klass), v2, v);
	  gts_edge_replace (e1, ne1);
	  gts_object_destroy (GTS_OBJECT (e1));
	  e1 = ne1;
	  if (fifo) gts_fifo_push (fifo, e1);
	}
	if (!GTS_IS_CONSTRAINT (e2)) {
	  GtsEdge * ne2 = 
	    gts_edge_new (GTS_EDGE_CLASS (GTS_OBJECT (c)->klass), v, v1);
	  gts_edge_replace (e2, ne2);
	  gts_object_destroy (GTS_OBJECT (e2));
	  e2 = ne2;
	  if (fifo) gts_fifo_push (fifo, e2);
	}

	/* look for face opposite */
	while (j && !f1) {
	  if (GTS_IS_FACE (j->data) && 
	      gts_face_has_parent_surface (j->data, s))
	    f1 = j->data;
	  j = j->next;
	}
	if (f1) { /* c is not a boundary of s */
	  GtsEdge * e3, * e4, * e5;
	  GtsVertex * v3;
	  gts_triangle_vertices_edges (GTS_TRIANGLE (f1), e,
				       &v1, &v2, &v3, &e, &e3, &e4);
	  e5 = gts_edge_new (s->edge_class, v, v3);
	  gts_surface_add_face (s, gts_face_new (s->face_class, e5, e2, e3));
	  gts_surface_add_face (s, gts_face_new (s->face_class, e5, e4, e1));
	  gts_object_destroy (GTS_OBJECT (f1));
	}
	gts_object_destroy (GTS_OBJECT (f));
	return;
      }
    }
    i = i->next;
  }
}

static void add_constraint (GtsConstraint * c, GtsSurface * s)
{
  g_assert (gts_delaunay_add_constraint (s, c) == NULL);
}

static void split_constraint (GtsConstraint * c, gpointer * data)
{
  GtsSurface * s = data[0];
  GtsFifo * fifo = data[1];

  gts_constraint_split (c, s, fifo);
}

static void shuffle_array (GPtrArray * a)
{
  guint i;

  for (i = 0; i < a->len; i++) {
    guint j = (gdouble) rand ()*(a->len - 1)/(gdouble) RAND_MAX;
    guint k = (gdouble) rand ()*(a->len - 1)/(gdouble) RAND_MAX;
    gpointer tmp;

    if (j >= a->len) j = a->len - 1;
    if (k >= a->len) k = a->len - 1;

    tmp = g_ptr_array_index (a, j);
    g_ptr_array_index (a, j) = g_ptr_array_index (a, k);
    g_ptr_array_index (a, k) = tmp;
  }
}

int main (int argc, char * argv[])
{
  GPtrArray * vertices;
  GtsFifo * edges;
  guint i, line;
  GtsTriangle * t;
  GtsVertex * v1, * v2, * v3;
  GtsSurface * surface;
  gboolean keep_hull = TRUE;
  gboolean verbose = FALSE;
  gboolean add_constraints = TRUE;
  gboolean remove_holes = FALSE;
  gboolean check_delaunay = FALSE;
  gboolean conform = FALSE;
  gboolean refine = FALSE;
  gboolean split_constraints = FALSE;
  gboolean randomize = FALSE;
  gboolean remove_duplicates = FALSE;
  gint steiner_max = -1;
  gdouble quality = 0., area = G_MAXDOUBLE;
  int c = 0, status = 0;
  const char * fname = NULL;
  GTimer * timer;

  /* parse options using getopt */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"duplicates", no_argument, NULL, 'd'},
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'},
      {"randomize", no_argument, NULL, 'r'},
      {"hull", no_argument, NULL, 'b'},
      {"noconst", no_argument, NULL, 'e'},
      {"holes", no_argument, NULL, 'H'},
      {"split", no_argument, NULL, 'S'},
      {"check", no_argument, NULL, 'c'},
      {"files", required_argument, NULL, 'f'},
      {"conform", no_argument, NULL, 'o'},
      {"steiner", required_argument, NULL, 's'},
      {"quality", required_argument, NULL, 'q'},
      {"area", required_argument, NULL, 'a'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hvbecf:os:q:a:HSrd",
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hvbecf:os:q:a:HSrd"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'd': /* duplicates */
      remove_duplicates = TRUE;
      break;
    case 'b': /* do not keep convex hull */
      keep_hull = FALSE;
      break;
    case 'e': /* do not add constrained edges */
      add_constraints = FALSE;
      break;
    case 'H': /* remove holes */
      remove_holes = TRUE;
      break;
    case 'S': /* split constraints */
      split_constraints = TRUE;
      break;
    case 'r': /* randomize */
      randomize = TRUE;
      break;
    case 'c': /* check Delaunay property */
      check_delaunay = TRUE;
      break;
    case 'f': /* generates files */
      fname = optarg;
      break;      
    case 'v': /* verbose */
      verbose = TRUE;
      break;
    case 'o': /* conform */
      conform = TRUE;
      break;
    case 's': /* steiner */
      steiner_max = atoi (optarg);
      break;
    case 'q': /* quality */
      conform = TRUE;
      refine = TRUE;
      quality = atof (optarg);
      break;
    case 'a': /* area */
      conform = TRUE;
      refine = TRUE;
      area = atof (optarg);
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: delaunay [OPTION] < file.gts\n"
	     "Construct the constrained Delaunay triangulation of the input\n"
	     "\n"
	     "  -b       --hull         do not keep convex hull\n"
	     "  -e       --noconst      do not add constrained edges\n"
	     "  -S       --split        split constraints (experimental)\n"
	     "  -H       --holes        remove holes from the triangulation\n"
	     "  -d       --duplicates   remove duplicate vertices\n"
	     "  -r       --randomize    shuffle input vertex list\n"
	     "  -c       --check        check Delaunay property\n"
	     "  -f FNAME --files=FNAME  generate evolution files\n"
	     "  -o       --conform      generate conforming triangulation\n"
	     "  -s N     --steiner=N    maximum number of Steiner points for\n"
	     "                          conforming triangulation (default is no limit)\n"
	     "  -q Q     --quality=Q    Set the minimum acceptable face quality\n"
	     "  -a A     --area=A       Set the maximum acceptable face area\n"
	     "  -v       --verbose      print statistics about the triangulation\n"
	     "  -h       --help         display this help and exit\n"
	     "\n"
	     "Reports bugs to %s\n",
	     GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `delaunay --help' for more information.\n");
      return 1; /* failure */
    }
  }

  /* read file => two lists: vertices and constraints */

  edges = gts_fifo_new ();
  vertices = g_ptr_array_new ();
  if (add_constraints) /* the edge class is a GtsConstraintClass */
    line = read_list (vertices, edges, 
		      GTS_EDGE_CLASS (gts_constraint_class ()),
		      stdin);
  else /* the edge class is a "normal" edge: GtsEdgeClass */
    line = read_list (vertices, edges, 
		      gts_edge_class (), 
		      stdin);

  if (line > 0) {
    fprintf (stderr, "delaunay: error in input file at line %u\n", line);
    return 1;
  }

  timer = g_timer_new ();
  g_timer_start (timer);

  if (randomize)
    shuffle_array (vertices);

  /* create triangle enclosing all the vertices */
  {
    GSList * list = NULL;
    for (i = 0; i < vertices->len; i++)
      list = g_slist_prepend (list, g_ptr_array_index (vertices, i));
    t = gts_triangle_enclosing (gts_triangle_class (), list, 100.);
    g_slist_free (list);
  }
  gts_triangle_vertices (t, &v1, &v2, &v3);

  /* create surface with one face: the enclosing triangle */
  surface = gts_surface_new (gts_surface_class (),
			     gts_face_class (),
			     gts_edge_class (),
			     gts_vertex_class ());
  gts_surface_add_face (surface, gts_face_new (gts_face_class (),
					       t->e1, t->e2, t->e3));

  /* add vertices */
  for (i = 0; i < vertices->len; i++) {
    GtsVertex * v1 = g_ptr_array_index (vertices, i);
    GtsVertex * v = gts_delaunay_add_vertex (surface, v1, NULL);

    g_assert (v != v1);
    if (v != NULL) {
      if (!remove_duplicates) {
	fprintf (stderr, "delaunay: duplicate vertex (%g,%g) in input file\n",
		 GTS_POINT (v)->x, GTS_POINT (v)->y);
	return 1; /* Failure */
      }
      else
	gts_vertex_replace (v1, v);
    }
    if (fname) {
      static guint nf = 1;
      char s[80];
      FILE * fp;

      g_snprintf (s, 80, "%s.%u", fname, nf++);
      fp = fopen (s, "wt");
      gts_surface_write_oogl (surface, fp);
      fclose (fp);

      if (check_delaunay && gts_delaunay_check (surface)) {
	fprintf (stderr, "delaunay: triangulation is not Delaunay\n");
	return 1;
      }
    }
  }
  g_ptr_array_free (vertices, TRUE);

  /* add remaining constraints */
  if (add_constraints)
    gts_fifo_foreach (edges, (GtsFunc) add_constraint, surface);

  /* destroy enclosing triangle */
  gts_allow_floating_vertices = TRUE;
  gts_object_destroy (GTS_OBJECT (v1));
  gts_object_destroy (GTS_OBJECT (v2));
  gts_object_destroy (GTS_OBJECT (v3));
  gts_allow_floating_vertices = FALSE;

  if (!keep_hull)
    gts_delaunay_remove_hull (surface);

  if (remove_holes)
    delaunay_remove_holes (surface);

  if (split_constraints) {
    gpointer data[2];

    data[0] = surface;
    data[1] = edges;
    gts_fifo_foreach (edges, (GtsFunc) split_constraint, data);
  }

  if (conform) {
    guint encroached_number = 
      gts_delaunay_conform (surface, 
			    steiner_max,
			    (GtsEncroachFunc) gts_vertex_encroaches_edge,
			    NULL);
    if (encroached_number == 0 && refine) {
      guint unrefined_number;
      gpointer data[2];
      
      data[0] = &quality;
      data[1] = &area;
      unrefined_number = 
	gts_delaunay_refine (surface, 
			     steiner_max,
			     (GtsEncroachFunc) gts_vertex_encroaches_edge,
			     NULL,
			     (GtsKeyFunc) triangle_cost,
			     data);
      if (verbose && unrefined_number > 0)
	fprintf (stderr, 
		 "delaunay: ran out of Steiner points (max: %d) during refinement\n"
		 "%d unrefined faces left\n",
		 steiner_max, unrefined_number);
    }
    else if (verbose && encroached_number > 0)
      fprintf (stderr, 
	       "delaunay: ran out of Steiner points (max: %d) during conforming\n"
	       "Delaunay triangulation: %d encroached constraints left\n",
	       steiner_max, encroached_number);
  }
  g_timer_stop (timer);

  if (verbose) {
    gts_surface_print_stats (surface, stderr);
    fprintf (stderr, "# Triangulation time: %g s speed: %.0f vertex/s\n", 
	  g_timer_elapsed (timer, NULL),
	  gts_surface_vertex_number (surface)/g_timer_elapsed (timer, NULL));
  }

  if (check_delaunay && gts_delaunay_check (surface)) {
    fprintf (stderr, "delaunay: triangulation is not Delaunay\n");
    status = 1; /* failure */
  }

  /* write triangulation */
  gts_surface_write (surface, stdout);

  return status;
}
