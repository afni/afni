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

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <pgm.h>
#include "config.h"
#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#endif /* HAVE_GETOPT_H */
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include "gts.h"

typedef enum { NUMBER, COST } StopOptions;

static gboolean stop_number (gdouble cost, guint number, guint * max)
{
  if (number >= *max)
    return TRUE;
  return FALSE;
}

static gboolean stop_number_log_cost (gdouble cost, guint number, guint * max)
{
  if (number >= *max)
    return TRUE;
  fprintf (stderr, "%d %g\n", number, cost);
  return FALSE;
}

static gboolean stop_number_verbose (gdouble cost, guint number, guint * max)
{
  static guint nmax = 0, nold = 0;
  static GTimer * timer = NULL, * total_timer = NULL;

  if (timer == NULL) {
    nmax = nold = number;
    timer = g_timer_new ();
    total_timer = g_timer_new ();
    g_timer_start (total_timer);
  }

  if (number != nold && number % 121 == 0 &&
      number > nmax && nmax <= *max) {
    gdouble total_elapsed = g_timer_elapsed (total_timer, NULL);
    gdouble remaining;
    gdouble hours, mins, secs;
    gdouble hours1, mins1, secs1;

    g_timer_stop (timer);

    hours = floor (total_elapsed/3600.);
    mins = floor ((total_elapsed - 3600.*hours)/60.);
    secs = floor (total_elapsed - 3600.*hours - 60.*mins);

    remaining = total_elapsed*((*max - nmax)/(gdouble) (number - nmax) - 1.);
    hours1 = floor (remaining/3600.);
    mins1 = floor ((remaining - 3600.*hours1)/60.);
    secs1 = floor (remaining - 3600.*hours1 - 60.*mins1);

    fprintf (stderr, 
	     "\rVertices: %8u %3.0f%% %6.0f vertices/s "
	     "Elapsed: %02.0f:%02.0f:%02.0f "
	     "Remaining: %02.0f:%02.0f:%02.0f ",
	     number, 
	     100.*(number - nmax)/(*max - nmax),
	     (number - nold)/g_timer_elapsed (timer, NULL),
	     hours, mins, secs,
	     hours1, mins1, secs1);
    fflush (stderr);

    nold = number;
    g_timer_start (timer);
  }
  if (number >= *max) {
    g_timer_destroy (timer);
    g_timer_destroy (total_timer);
    return TRUE;
  }
  return FALSE;
}

static gboolean stop_cost (gdouble cost, guint number, gdouble * min)
{
  if (cost < *min)
    return TRUE;
  return FALSE;
}

static gboolean stop_cost_verbose (gdouble cost, guint number, gdouble * min)
{
  if (number % 121 == 0) {
    fprintf (stderr, "\rVertices: %10u Cost: %10.2f ", number, cost);
    fflush (stderr);
  }
  if (cost < *min)
    return TRUE;
  return FALSE;
}

/* ListFace: Header */

typedef struct _ListFace         ListFace;

struct _ListFace {
  /*< private >*/
  GtsListFace parent;

  /*< public >*/
  GtsEHeap * heap;
  GtsEHeapPair * pair;
  GtsVertex * best;
  GtsVector p;
};

typedef struct _ListFaceClass    ListFaceClass;

struct _ListFaceClass {
  /*< private >*/
  GtsFaceClass parent_class;

  /*< public >*/
  void (* cost_init) (ListFace *);
  gdouble (* cost) (ListFace *, GtsPoint *);
};

#define LIST_FACE(obj)            GTS_OBJECT_CAST (obj,\
					         ListFace,\
					         list_face_class ())
#define LIST_FACE_CLASS(klass)    GTS_OBJECT_CLASS_CAST (klass,\
						 ListFaceClass,\
						 list_face_class())
#define IS_LIST_FACE(obj)         (gts_object_is_from_class (obj,\
						 list_face_class ()))

static ListFaceClass * list_face_class (void);

/* ListFace: Object */

static void list_face_destroy (GtsObject * object)
{
  ListFace * f = LIST_FACE (object);

  if (f->heap) {
    gts_eheap_remove (f->heap, f->pair);
    f->heap = NULL;
  }

  (* GTS_OBJECT_CLASS (list_face_class ())->parent_class->destroy) 
    (object);
}

static void triangle_plane (GtsTriangle * f, GtsVector p)
{
  GtsPoint * v1, * v2, * v3;
  gdouble x1, x2, y1, y2, det;

  v1 = GTS_POINT (GTS_SEGMENT (f->e1)->v1);
  v2 = GTS_POINT (GTS_SEGMENT (f->e1)->v2);
  v3 = GTS_POINT (gts_triangle_vertex (f));

  x1 = v2->x - v1->x;
  y1 = v2->y - v1->y;
  x2 = v3->x - v1->x;
  y2 = v3->y - v1->y;
  det = x1*y2 - x2*y1;
  g_assert (det != 0.);

  p[0] = (y2*(v2->z - v1->z) - y1*(v3->z - v1->z))/det;
  p[1] = (-x2*(v2->z - v1->z) + x1*(v3->z - v1->z))/det;
  p[2] = ((- v1->x*y2 + v1->y*x2)*(v2->z - v1->z) +
	  (- v1->y*x1 + v1->x*y1)*(v3->z - v1->z))/det + v1->z;
}

static void list_face_cost_init (ListFace * f)
{
  triangle_plane (GTS_TRIANGLE (f), f->p);
}

static gdouble list_face_cost (ListFace * f, GtsPoint * p)
{
  return fabs (p->x*f->p[0] + p->y*f->p[1] + f->p[2] - p->z);  
}

static void list_face_class_init (ListFaceClass * klass)
{
  GTS_OBJECT_CLASS (klass)->destroy = list_face_destroy;
  klass->cost_init = list_face_cost_init;
  klass->cost = list_face_cost;
}

static ListFaceClass * list_face_class (void)
{
  static ListFaceClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo list_face_info = {
      "ListFace",
      sizeof (ListFace),
      sizeof (ListFaceClass),
      (GtsObjectClassInitFunc) list_face_class_init,
      (GtsObjectInitFunc) NULL,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_list_face_class ()),
				  &list_face_info);
  }

  return klass;
}

static void list_face_update (ListFace * f, GtsEHeap * heap)
{
  if (IS_LIST_FACE (f)) {
    GSList * i = GTS_LIST_FACE (f)->points;
    
    if (i) {
      gdouble maxerr = 0.;
      ListFaceClass * klass = LIST_FACE_CLASS (GTS_OBJECT (f)->klass);

      (* klass->cost_init) (f);
      f->best = NULL;
      while (i) {
	GtsPoint * v = i->data;
	gdouble e = (* klass->cost) (f, v);
	
	if (e > maxerr) {
	  maxerr = e;
	  f->best = GTS_VERTEX (v); 
	}
	i = i->next;
      }
      if (f->heap) {
	gts_eheap_remove (f->heap, f->pair);
	f->heap = NULL;
      }
      if (maxerr > 0.) {
	f->pair = gts_eheap_insert_with_key (heap, f, - maxerr);
	f->heap = heap;
      }
    }
  }
}

static void list_face_clear_heap (ListFace * f)
{
  f->heap = NULL;
}

static void surface_hf_refine (GtsSurface * s,
			       GtsStopFunc stop_func,
			       gpointer stop_data)
{
  GtsEHeap * heap;
  gdouble top_cost;
  guint nv = 4;
  GtsListFace * f;

  g_return_if_fail (s != NULL);
  g_return_if_fail (stop_func != NULL);

  heap = gts_eheap_new (NULL, NULL);
  gts_surface_foreach_face (s, (GtsFunc) list_face_update, heap);
  while ((f = gts_eheap_remove_top (heap, &top_cost)) &&
	 !(*stop_func) (- top_cost, nv, stop_data)) {
    GtsVertex * v = LIST_FACE (f)->best;
    GSList * t, * i;

    LIST_FACE (f)->heap = NULL;
    gts_delaunay_add_vertex_to_face (s, v, GTS_FACE (f));
    i = t = gts_vertex_triangles (v, NULL);
    while (i) {
      list_face_update (i->data, heap);
      i = i->next;
    }
    g_slist_free (t);
    nv++;
  }

  if (f)
    LIST_FACE (f)->heap = NULL;

  gts_eheap_foreach (heap, (GFunc) list_face_clear_heap, NULL);
  gts_eheap_destroy (heap);
}

static void prepend (GtsListFace * f, 
		     gray ** g, guint i, guint j)
{
  f->points = g_slist_prepend (f->points, 
			       gts_vertex_new (gts_vertex_class (), 
					       i, j, g[j][i]));
}

static void destroy_unused (GtsListFace * f)
{
  g_slist_foreach (f->points, (GFunc) gts_object_destroy, NULL);
  g_slist_free (f->points);
  f->points = NULL;
}

static GtsSurface * happrox (gray ** g, gint width, gint height,
			     GtsStopFunc stop_func,
			     gpointer stop_data)
{
  GtsSurface * s = gts_surface_new (gts_surface_class (),
				    GTS_FACE_CLASS (list_face_class ()),
				    gts_edge_class (),
				    gts_vertex_class ());
  GtsVertex * v1 = gts_vertex_new (s->vertex_class, 0., 0., g[0][0]);
  GtsVertex * v2 = gts_vertex_new (s->vertex_class, 
				   0., height - 1, g[height - 1][0]);
  GtsVertex * v3 = gts_vertex_new (s->vertex_class,
				   width - 1, 0., g[0][width - 1]);
  GtsVertex * v4 = gts_vertex_new (s->vertex_class,
				   width - 1, height - 1, 
				   g[height - 1][width - 1]);
  guint i, j;
  GSList * corners = NULL;
  GtsTriangle * t;
  GtsVertex * w1, * w2, * w3;
  GtsListFace * f;

  /* creates enclosing triangle */
  corners = g_slist_prepend (corners, v1);
  corners = g_slist_prepend (corners, v2);
  corners = g_slist_prepend (corners, v3);
  corners = g_slist_prepend (corners, v4);
  t = gts_triangle_enclosing (gts_triangle_class (), corners, 100.);
  g_slist_free (corners);
  gts_triangle_vertices (t, &w1, &w2, &w3);

  f = GTS_LIST_FACE (gts_face_new (s->face_class, t->e1, t->e2, t->e3));
  gts_surface_add_face (s, GTS_FACE (f));

  /* add PGM vertices (corners excepted) to point list of f */
  for (i = 1; i < width - 1; i++) {
    for (j = 1; j < height - 1; j++)
      prepend (f, g, i, j);
    prepend (f, g, i, 0);
    prepend (f, g, i, height - 1);
  }
  for (j = 1; j < height - 1; j++) {
    prepend (f, g, 0, j);
    prepend (f, g, width - 1, j);
  }
  pgm_freearray (g, height);

  /* add four corners to initial triangulation */
  g_assert (gts_delaunay_add_vertex_to_face (s, v1, GTS_FACE (f)) == NULL);
  f = GTS_LIST_FACE (gts_point_locate (GTS_POINT (v2), s, NULL));
  g_assert (gts_delaunay_add_vertex_to_face (s, v2, GTS_FACE (f)) == NULL);
  f = GTS_LIST_FACE (gts_point_locate (GTS_POINT (v3), s, NULL));
  g_assert (gts_delaunay_add_vertex_to_face (s, v3, GTS_FACE (f)) == NULL);
  f = GTS_LIST_FACE (gts_point_locate (GTS_POINT (v4), s, NULL));
  g_assert (gts_delaunay_add_vertex_to_face (s, v4, GTS_FACE (f)) == NULL);

  /* refine surface */
  surface_hf_refine (s, stop_func, stop_data);

  /* destroy unused vertices */
  gts_surface_foreach_face (s, (GtsFunc) destroy_unused, NULL);
  
  /* destroy enclosing triangle */
  gts_allow_floating_vertices = TRUE;
  gts_object_destroy (GTS_OBJECT (w1));
  gts_object_destroy (GTS_OBJECT (w2));
  gts_object_destroy (GTS_OBJECT (w3));
  gts_allow_floating_vertices = FALSE;

  return s;
}

int main (int argc, char * argv[])
{
  int c = 0;
  gboolean verbose = FALSE;
  GtsSurface * s;
  gint width, height;
  gray ** g, maxgray;
  GtsStopFunc stop_func = NULL;
  gpointer stop_data = NULL;
  StopOptions stop = NUMBER;
  guint number = 0;
  gdouble cmin = 0.0;
  gboolean logcost = FALSE;

  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"log", no_argument, NULL, 'l'},
      {"number", required_argument, NULL, 'n'},
      {"cost", required_argument, NULL, 'c'},
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hvn:c:l",
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hvn:c:l"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'l': /* log cost */
      logcost = TRUE;
      break;
    case 'n': /* stop by number */
      stop = NUMBER;
      number = atoi (optarg);
      break;
    case 'c': /* stop by cost */
      stop = COST;
      cmin = - atof (optarg);
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: happrox [OPTION]... < input.pgm > output.gts\n"
	     "Returns a simplified triangulation of a set of points using\n"
             "algorithm III of Garland and Heckbert (1995).\n"
	     "\n"
	     "  -n N, --number=N    stop the refinement process if the number of\n"
	     "                      vertices is larger than N\n"
	     "  -c C, --cost=C      stop the refinement process if the cost of insertion\n"
             "                      of a vertex is smaller than C\n"
             "  -l    --log         log evolution of cost\n" 
	     "  -v,   --verbose     display surface statistics\n"
	     "  -h,   --help        display this help and exit\n"
	     "\n"
	     "Report bugs to %s\n",
	     GTS_MAINTAINER);
      return 0;
      break;
    case 'v':
      verbose = TRUE;
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `happrox --help' for more information.\n");
      return 1;
    }
  }

  switch (stop) {
  case NUMBER:
    if (verbose)
      stop_func = (GtsStopFunc) stop_number_verbose;
    else
      stop_func = (GtsStopFunc) stop_number;
    if (logcost)
      stop_func = (GtsStopFunc) stop_number_log_cost;
    stop_data = &number;
    break;
  case COST:
    if (verbose)
      stop_func = (GtsStopFunc) stop_cost_verbose;
    else
      stop_func = (GtsStopFunc) stop_cost; 
    stop_data = &cmin;
    break;
  default:
    g_assert_not_reached ();
  }
  
  g = pgm_readpgm (stdin, &width, &height, &maxgray);
  if (verbose)
    fprintf (stderr, "width: %d height: %d maxgray: %d\n",
	     width, height, maxgray);
  
  s = happrox (g, width, height, stop_func, stop_data);

  if (verbose) {
    fputc ('\n', stderr);
    gts_surface_print_stats (s, stderr);
  }
  gts_surface_write (s, stdout);

  return 0;
}
