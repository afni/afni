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

typedef struct _CartesianGrid CartesianGrid;

struct _CartesianGrid {
  GtsVertex *** vertices;
  guint nx, ny;
  gdouble xmin, xmax, ymin, ymax;
};

static void ** malloc2D (guint nx, guint ny, gulong size)
{
  void ** m = g_malloc (nx*sizeof (void *));
  guint i;

  for (i = 0; i < nx; i++)
    m[i] = g_malloc0 (ny*size);

  return m;
}

static void free2D (void ** m, guint nx)
{
  guint i;

  g_return_if_fail (m != NULL);

  for (i = 0; i < nx; i++)
    g_free (m[i]);
  g_free (m);
}

static CartesianGrid * cartesian_grid_new (guint nx, guint ny)
{
  CartesianGrid * grid;

  grid = g_malloc (sizeof (CartesianGrid));
  grid->vertices = (GtsVertex ***) malloc2D (nx, ny, sizeof (GtsVertex *));
  grid->nx = nx;
  grid->ny = ny;
  grid->xmin = G_MAXDOUBLE;
  grid->xmax = - G_MAXDOUBLE;
  grid->ymin = G_MAXDOUBLE;
  grid->ymax = - G_MAXDOUBLE;

  return grid;
}

static void cartesian_grid_destroy (CartesianGrid * g,
				    gboolean destroy_vertices)
{
  g_return_if_fail (g != NULL);

  if (destroy_vertices) {
    guint i, j;

    gts_allow_floating_vertices = TRUE;
    for (i = 0; i < g->nx; i++)
      for (j = 0; j < g->ny; j++)
	if (g->vertices[i][j])
	  gts_object_destroy (GTS_OBJECT (g->vertices[i][j]));
    gts_allow_floating_vertices = FALSE;
  }

  free2D ((void **) g->vertices, g->nx);
  g_free (g);
}

static CartesianGrid * cartesian_grid_read (GtsVertexClass * klass,
					    guint * line)
{
  CartesianGrid * grid;
  guint nx, ny, line_number = 1, i, j;

  g_return_val_if_fail (klass != NULL, NULL);
  
  if (scanf ("%u %u", &nx, &ny) != 2) {
    if (line)
      *line = line_number;
    return NULL;
  }
  line_number++;

  grid = cartesian_grid_new (nx, ny);
  for (i = 0; i < nx; i++)
    for (j = 0; j < ny; j++) {
      gdouble x, y, z;
      if (scanf ("%lf %lf %lf", &x, &y, &z) != 3) {
	cartesian_grid_destroy (grid, TRUE);
	if (line)
	  *line = line_number;
	return NULL;
      }
      if (z != 0.)
	grid->vertices[i][j] = gts_vertex_new (klass, x, y, z);
      if (x > grid->xmax) grid->xmax = x;
      if (x < grid->xmin) grid->xmin = x;
      if (y > grid->ymax) grid->ymax = y;
      if (y < grid->ymin) grid->ymin = y;
      line_number++;
    }

  return grid;
}

static GtsEdge * new_edge (GtsVertex * v1, GtsVertex * v2)
{
  GtsSegment * s = gts_vertices_are_connected (v1, v2);
  return s == NULL ? 
    gts_edge_new (GTS_EDGE_CLASS (gts_constraint_class ()), v1, v2) :
    GTS_EDGE (s);
}

static void cartesian_grid_triangulate (CartesianGrid * g,
					GtsSurface * s)
{
  gint i, j;
  GtsVertex *** v;

  g_return_if_fail (g != NULL);
  g_return_if_fail (s != NULL);

  v = g->vertices;
  for (i = 0; i < g->nx - 1; i++)
    for (j = 0; j < g->ny - 1; j++)
      if (v[i][j]) {
	if (v[i][j+1]) {
	  if (v[i+1][j+1]) {
	    GtsEdge * e1 = new_edge (v[i][j+1], v[i][j]);
	    GtsEdge * e2 = 
	      gts_edge_new (GTS_EDGE_CLASS (gts_constraint_class ()), 
			    v[i][j], v[i+1][j+1]);
	    GtsEdge * e3 = new_edge (v[i+1][j+1], v[i][j+1]);
	    gts_surface_add_face (s, gts_face_new (s->face_class, e1, e2, e3));
	    if (v[i+1][j]) {
	      e1 = new_edge (v[i+1][j], v[i+1][j+1]);
	      e3 = new_edge (v[i][j], v[i+1][j]);
	      gts_surface_add_face (s, 
				    gts_face_new (s->face_class, e1, e2, e3));
	    }
	  }
	  else if (v[i+1][j]) {
	    GtsEdge * e1 = new_edge (v[i][j+1], v[i][j]);
	    GtsEdge * e2 = new_edge (v[i][j], v[i+1][j]);
	    GtsEdge * e3 = 
	      gts_edge_new (GTS_EDGE_CLASS (gts_constraint_class ()), 
			    v[i+1][j], v[i][j+1]);
	    gts_surface_add_face (s, gts_face_new (s->face_class, e1, e2, e3));
	  }
	}
	else if (v[i+1][j] && v[i+1][j+1]) {
	  GtsEdge * e1 = new_edge (v[i][j], v[i+1][j]);
	  GtsEdge * e2 = new_edge (v[i+1][j], v[i+1][j+1]);
	  GtsEdge * e3 = 
	    gts_edge_new (GTS_EDGE_CLASS (gts_constraint_class ()),
			  v[i+1][j+1], v[i][j]);
	  gts_surface_add_face (s, gts_face_new (s->face_class, e1, e2, e3));
	}
      }
      else if (v[i][j+1] && v[i+1][j+1] && v[i+1][j]) {
	GtsEdge * e1 = new_edge (v[i+1][j], v[i+1][j+1]);
	GtsEdge * e2 = new_edge (v[i+1][j+1], v[i][j+1]);
	GtsEdge * e3 = 
	  gts_edge_new (GTS_EDGE_CLASS (gts_constraint_class ()),
			v[i][j+1], v[i+1][j]);
	gts_surface_add_face (s, gts_face_new (s->face_class, e1, e2, e3));
      }
}

static gboolean triangle_is_hole (GtsTriangle * t)
{
  GtsEdge * e1, * e2, * e3;
  GtsVertex * v1, * v2, * v3;

  gts_triangle_vertices_edges (t, NULL, &v1, &v2, &v3, &e1, &e2, &e3);

  if ((GTS_IS_CONSTRAINT (e1) && GTS_SEGMENT (e1)->v2 != v1) ||
      (GTS_IS_CONSTRAINT (e2) && GTS_SEGMENT (e2)->v2 != v2) ||
      (GTS_IS_CONSTRAINT (e3) && GTS_SEGMENT (e3)->v2 != v3))
    return TRUE;
  return FALSE;
}

static void mark_as_hole (GtsFace * f, GtsSurface * s)
{
  GtsEdge * e1, * e2, * e3;

  if (GTS_OBJECT (f)->reserved == f)
    return;

  GTS_OBJECT (f)->reserved = f;
  e1 = GTS_TRIANGLE (f)->e1;
  e2 = GTS_TRIANGLE (f)->e2;
  e3 = GTS_TRIANGLE (f)->e3;

  if (!GTS_IS_CONSTRAINT (e1)) {
    GSList * i = e1->triangles;
    while (i) {
      GtsFace * f1 = i->data;
      if (f1 != f && GTS_IS_FACE (f1) && gts_face_has_parent_surface (f1, s))
	mark_as_hole (f1, s);
      i = i->next;
    }
  }
  if (!GTS_IS_CONSTRAINT (e2)) {
    GSList * i = e2->triangles;
    while (i) {
      GtsFace * f1 = i->data;
      if (f1 != f && GTS_IS_FACE (f1) && gts_face_has_parent_surface (f1, s))
	mark_as_hole (f1, s);
      i = i->next;
    }
  }
  if (!GTS_IS_CONSTRAINT (e3)) {
    GSList * i = e3->triangles;
    while (i) {
      GtsFace * f1 = i->data;
      if (f1 != f && GTS_IS_FACE (f1) && gts_face_has_parent_surface (f1, s))
	mark_as_hole (f1, s);
      i = i->next;
    }
  }
}

static void edge_mark_as_hole (GtsEdge * e, GtsSurface * s)
{
  GSList * i = e->triangles;

  while (i) {
    GtsFace * f = i->data;
    if (GTS_IS_FACE (f) && 
	gts_face_has_parent_surface (f, s) &&
	triangle_is_hole (GTS_TRIANGLE (f)))
      mark_as_hole (f, s);
    i = i->next;
  }
}

static gboolean face_is_marked (GtsObject * o)
{
  if (o->reserved == o || gts_triangle_is_duplicate (GTS_TRIANGLE (o)))
    return TRUE;
  return FALSE;
}

static void build_constraint_list (GtsEdge * e, gpointer * data)
{
  GtsSurface * s = data[0];
  GSList ** constraints = data[1];
  
  if (gts_edge_is_boundary (e, s))
    *constraints = g_slist_prepend (*constraints, e);
}

static GtsSurface * cartesian_grid_triangulate_holes (CartesianGrid * grid,
						      GtsSurface * s)
{
  GtsVertex * v1, * v2, * v3, * v4;
  GtsEdge * e1, * e2, * e3, * e4, * e5;
  gdouble w, h;
  GtsSurface * box;
  GSList * constraints = NULL, * vertices = NULL, * i;
  gpointer data[2];

  g_return_val_if_fail (grid != NULL, NULL);
  g_return_val_if_fail (s != NULL, NULL);

  /* build enclosing box */
  w = grid->xmax - grid->xmin;
  h = grid->ymax - grid->ymin;
  v1 = gts_vertex_new (s->vertex_class, grid->xmin - w, grid->ymin - h, 0.);
  v2 = gts_vertex_new (s->vertex_class, grid->xmax + w, grid->ymin - h, 0.);
  v3 = gts_vertex_new (s->vertex_class, grid->xmax + w, grid->ymax + h, 0.);
  v4 = gts_vertex_new (s->vertex_class, grid->xmin - w, grid->ymax + h, 0.);

  e1 = gts_edge_new (s->edge_class, v1, v2);
  e2 = gts_edge_new (s->edge_class, v2, v3);
  e3 = gts_edge_new (s->edge_class, v3, v4);
  e4 = gts_edge_new (s->edge_class, v4, v1);
  e5 = gts_edge_new (s->edge_class, v1, v3);

  box = gts_surface_new (GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass), 
			 s->face_class, 
			 s->edge_class, 
			 s->vertex_class);
  gts_surface_add_face (box, gts_face_new (s->face_class, e1, e2, e5));
  gts_surface_add_face (box, gts_face_new (s->face_class, e3, e4, e5));

  /* build vertex and constraint list from the boundaries of the input
     surface s */
  data[0] = s;
  data[1] = &constraints;
  gts_surface_foreach_edge (s, (GtsFunc) build_constraint_list, data);
  vertices = gts_vertices_from_segments (constraints);

  /* triangulate holes */
  i = vertices;
  while (i) {
    g_assert (!gts_delaunay_add_vertex (box, i->data, NULL));
    i = i->next;
  }
  g_slist_free (vertices);

  i = constraints;
  while (i) {
    g_assert (!gts_delaunay_add_constraint (box, i->data));
    i = i->next;
  }

  /* destroy corners of the enclosing box */
  gts_allow_floating_vertices = TRUE;
  gts_object_destroy (GTS_OBJECT (v1));
  gts_object_destroy (GTS_OBJECT (v2));
  gts_object_destroy (GTS_OBJECT (v3));
  gts_object_destroy (GTS_OBJECT (v4));
  gts_allow_floating_vertices = FALSE;

  /* remove parts of the mesh which are not holes */
  i = constraints;
  while (i) {
    edge_mark_as_hole (i->data, box);
    i = i->next;
  }
  g_slist_free (constraints);

  /* remove marked and duplicate faces */
  gts_surface_foreach_face_remove (box, (GtsFunc) face_is_marked, NULL);

  /* box now contains only the triangulated holes. */
  return box;
}

int main (int argc, char * argv[])
{
  gboolean verbose = FALSE;
  gboolean triangulate_holes = TRUE;
  gboolean write_holes = FALSE;
  int c = 0;
  CartesianGrid * grid;
  guint line;
  GtsSurface * s;
  GTimer * timer;

  /* parse options using getopt */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"holes", no_argument, NULL, 'H'},
      {"keep", no_argument, NULL, 'k'},
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hvHk", 
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hvHk"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'H': /* holes */
      write_holes = TRUE;
      break;
    case 'k': /* keep */
      triangulate_holes = FALSE;
      break;
    case 'v': /* verbose */
      verbose = TRUE;
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: cartesian [OPTION] < FILE\n"
	     "Triangulates vertices of a regular cartesian mesh\n"
	     "(possibly containing holes)\n"
	     "\n"
	     "  -k    --keep     keep holes\n"
	     "  -H    --holes    write holes only\n"
	     "  -v    --verbose  print statistics about the surface\n"
	     "  -h    --help     display this help and exit\n"
	       "\n"
	     "Reports bugs to %s\n",
	       GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `cartesian --help' for more information.\n");
      return 1; /* failure */
    }
  }
  
  grid = cartesian_grid_read (gts_vertex_class (), &line);
  if (grid == NULL) {
    fprintf (stderr, 
	   "cartesian: file on standard input is not a valid cartesian grid\n"
	   "error at line %u\n", line);
    return 1;
  }

  timer = g_timer_new ();
  g_timer_start (timer);
  s = gts_surface_new (gts_surface_class (), 
		       gts_face_class (), 
		       gts_edge_class (), 
		       gts_vertex_class ());
  cartesian_grid_triangulate (grid, s);
  if (triangulate_holes) {    
    GtsSurface * holes = cartesian_grid_triangulate_holes (grid, s);
    if (write_holes) {
      gts_object_destroy (GTS_OBJECT (s));
      s = holes;
    }
    else
      gts_surface_merge (s, holes);
  }
  g_timer_stop (timer);
  
  if (verbose) {
    gts_surface_print_stats (s, stderr);
    fprintf (stderr, "# Triangulation time: %g s speed: %.0f vertex/s\n", 
	     g_timer_elapsed (timer, NULL),
	     gts_surface_vertex_number (s)/g_timer_elapsed (timer, NULL));
  }

  gts_surface_write (s, stdout);

  return 0; /* success */
}
