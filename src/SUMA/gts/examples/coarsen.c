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

#ifndef PI
#define PI 3.14159265359
#endif

typedef enum { NUMBER, COST } StopOptions;
typedef enum { COST_LENGTH, COST_OPTIMIZED, COST_ANGLE } CostOptions;
typedef enum { MIDVERTEX, OPTIMIZED } MidvertexOptions;

static gboolean stop_number_verbose (gdouble cost, guint number, guint * min)
{
  static guint nmax = 0, nold = 0;
  static GTimer * timer = NULL, * total_timer = NULL;

  g_return_val_if_fail (min != NULL, TRUE);

  if (timer == NULL) {
    nmax = nold = number;
    timer = g_timer_new ();
    total_timer = g_timer_new ();
    g_timer_start (total_timer);
  }

  if (number != nold && number % 121 == 0 &&
      number < nmax && nmax > *min) {
    gdouble total_elapsed = g_timer_elapsed (total_timer, NULL);
    gdouble remaining;
    gdouble hours, mins, secs;
    gdouble hours1, mins1, secs1;

    g_timer_stop (timer);

    hours = floor (total_elapsed/3600.);
    mins = floor ((total_elapsed - 3600.*hours)/60.);
    secs = floor (total_elapsed - 3600.*hours - 60.*mins);

    remaining = total_elapsed*((nmax - *min)/(gdouble) (nmax - number) - 1.);
    hours1 = floor (remaining/3600.);
    mins1 = floor ((remaining - 3600.*hours1)/60.);
    secs1 = floor (remaining - 3600.*hours1 - 60.*mins1);

    fprintf (stderr, 
	     "\rEdges: %10u %3.0f%% %6.0f edges/s "
	     "Elapsed: %02.0f:%02.0f:%02.0f "
	     "Remaining: %02.0f:%02.0f:%02.0f ",
	     number, 
	     100.*(nmax - number)/(nmax - *min),
	     (nold - number)/g_timer_elapsed (timer, NULL),
	     hours, mins, secs,
	     hours1, mins1, secs1);
    fflush (stderr);

    nold = number;
    g_timer_start (timer);
  }
  if (number < *min) {
    g_timer_destroy (timer);
    g_timer_destroy (total_timer);
    return TRUE;
  }
  return FALSE;
}

static gboolean stop_cost_verbose (gdouble cost, guint number, gdouble * max)
{
  g_return_val_if_fail (max != NULL, TRUE);

  if (number % 121 == 0) {
    fprintf (stderr, "\rEdges: %10u Cost: %10g ", number, cost);
    fflush (stderr);
  }
  if (cost > *max)
    return TRUE;
  return FALSE;
}

static gboolean stop_log_cost (gdouble cost, guint number)
{
  fprintf (stderr, "%d %g\n", number, cost);
  return FALSE;
}

static gdouble cost_angle (GtsEdge * e)
{
  if (e->triangles && e->triangles->next)
    return fabs (gts_triangles_angle (e->triangles->data,
				      e->triangles->next->data));
  return G_MAXDOUBLE;
}

/* coarsen - produce a coarsened version of the input */
int main (int argc, char * argv[])
{
  GtsSurface * s;
  GtsPSurface * ps = NULL;
  gboolean verbose = FALSE;
  gboolean progressive = FALSE;
  gboolean log_cost = FALSE;
  guint number = 0;
  gdouble cmax = 0.0;
  StopOptions stop = NUMBER;
  CostOptions cost = COST_OPTIMIZED;
  MidvertexOptions mid = OPTIMIZED;
  GtsKeyFunc cost_func = NULL;
  GtsCoarsenFunc coarsen_func = NULL;
  GtsStopFunc stop_func = NULL;
  gpointer stop_data = NULL;
  int c = 0;
  GtsFile * fp;
  gdouble fold = PI/180.;
  GtsVolumeOptimizedParams params = { 0.5, 0.5, 0. };
  gpointer coarsen_data = NULL, cost_data = NULL;

  /* parse options using getopt */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"angle", no_argument, NULL, 'a'},
      {"progressive", no_argument, NULL, 'p'},
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'},
      {"number", required_argument, NULL, 'n'},
      {"length", no_argument, NULL, 'l'},
      {"midvertex", no_argument, NULL, 'm'},
      {"cost", required_argument, NULL, 'c'},
      {"fold", required_argument, NULL, 'f'},
      {"vweight", required_argument, NULL, 'w'},
      {"bweight", required_argument, NULL, 'b'},
      {"sweight", required_argument, NULL, 's'},
      {"log", no_argument, NULL, 'L'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hvmc:n:lpf:w:b:s:La",
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hvmc:n:lpf:w:b:s:La"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'a': /* angle */
      cost = COST_ANGLE;
      break;
    case 'L': /* log */
      log_cost = TRUE;
      break;
    case 'p': /* write progressive surface */
      progressive = TRUE;
      break;
    case 'n': /* stop by number */
      stop = NUMBER;
      number = atoi (optarg);
      break;
    case 'c': /* stop by cost */
      stop = COST;
      cmax = atof (optarg);
      break;
    case 'v': /* verbose */
      verbose = TRUE;
      break;
    case 'm': /* midvertex */
      mid = MIDVERTEX;
      break;
    case 'l': /* cost is length */
      cost = COST_LENGTH;
      break;
    case 'f': /* fold angle */
      fold = atof (optarg)*PI/180.;
      break;
    case 'w': /* volume optimized weight */
      params.volume_weight = atof (optarg);
      break;
    case 'b': /* boundary optimized weight */
      params.boundary_weight = atof (optarg);
      break;
    case 's': /* shape optimized weight */
      params.shape_weight = atof (optarg);
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: coarsen [OPTION] < file.gts\n"
	     "Construct a coarsened version of the input.\n"
	     "\n"
	     "  -n N, --number=N    stop the coarsening process if the number of\n"
	     "                      edges was to fall below N\n"
	     "  -c C, --cost=C      stop the coarsening process if the cost of collapsing\n"
	     "                      an edge is larger than C\n"
	     "  -m    --midvertex   use midvertex as replacement vertex\n"
	     "                      default is volume optimized point\n"
	     "  -l    --length      use length^2 as cost function\n"
	     "                      default is optimized point cost\n"
	     "  -f F, --fold=F      set maximum fold angle to F degrees\n"
	     "                      default is one degree\n"
	     "  -w W, --vweight=W   set weight used for volume optimization\n"
	     "                      default is 0.5\n"
	     "  -b W, --bweight=W   set weight used for boundary optimization\n"
	     "                      default is 0.5\n"
	     "  -s W, --sweight=W   set weight used for shape optimization\n"
	     "                      default is 0.0\n"
	     "  -p    --progressive write progressive surface file\n"
	     "  -L    --log         logs the evolution of the cost\n"
	     "  -v    --verbose     print statistics about the surface\n"
	     "  -h    --help        display this help and exit\n"
	     "\n"
	     "Reports bugs to %s\n",
	     GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `coarsen --help' for more information.\n");
      return 1; /* failure */
    }
  }
  
  /* read surface in */
  s = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());
  fp = gts_file_new (stdin);
  if (gts_surface_read (s, fp)) {
    fputs ("coarsen: the file on standard input is not a valid GTS file\n", 
	   stderr);
    fprintf (stderr, "stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
    return 1; /* failure */
  }

  /* if verbose on print stats */
  if (verbose) {
    gts_surface_print_stats (s, stderr);
    fprintf (stderr, "# volume: %g area: %g\n", 
	     gts_surface_volume (s), gts_surface_area (s));
  }

  /* select the right coarsening process */
  switch (cost) {
  case COST_OPTIMIZED: 
    cost_func = (GtsKeyFunc) gts_volume_optimized_cost; 
    cost_data = &params;
    break;
  case COST_LENGTH:
    cost_func = NULL; break;
  case COST_ANGLE:
    cost_func = (GtsKeyFunc) cost_angle; break;
  default:
    g_assert_not_reached ();
  }
  switch (mid) {
  case MIDVERTEX:
    coarsen_func = NULL; 
    break;
  case OPTIMIZED:
    coarsen_func = (GtsCoarsenFunc) gts_volume_optimized_vertex; 
    coarsen_data = &params;
    break;
  default:
    g_assert_not_reached ();
  }
  if (log_cost)
    stop_func = (GtsStopFunc) stop_log_cost;
  else {
    switch (stop) {
    case NUMBER:
      if (verbose)
	stop_func = (GtsStopFunc) stop_number_verbose;
      else
	stop_func = (GtsStopFunc) gts_coarsen_stop_number; 
      stop_data = &number;
      break;
    case COST:
      if (verbose)
	stop_func = (GtsStopFunc) stop_cost_verbose;
      else
	stop_func = (GtsStopFunc) gts_coarsen_stop_cost; 
      stop_data = &cmax;
      break;
    default:
      g_assert_not_reached ();
    }
  }
  if (progressive)
    ps = gts_psurface_new (gts_psurface_class (),
			   s, gts_split_class (),
			   cost_func, cost_data,
			   coarsen_func, coarsen_data,
			   stop_func, stop_data, 
			   fold);
  else
    gts_surface_coarsen (s, 
			 cost_func, cost_data, 
			 coarsen_func, coarsen_data, 
			 stop_func, stop_data, fold);

  /* if verbose on print stats */
  if (verbose) {
    fputc ('\n', stderr);
    gts_surface_print_stats (s, stderr);
    fprintf (stderr, "# volume: %g area: %g\n", 
	     gts_surface_volume (s), gts_surface_area (s));
  }

  /* write resulting surface to standard output */
  if (progressive)
    gts_psurface_write (ps, stdout);
  else
    gts_surface_write (s, stdout);

  return 0; /* success */
}
