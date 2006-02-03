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
#include <stdio.h>
#include <stdlib.h>
#include "config.h"
#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#endif /* HAVE_GETOPT_H */
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include "gts.h"

#ifndef PI
#  define PI 3.14159265359
#endif

static void sphere (gdouble ** f, GtsCartesianGrid g, guint k, gpointer data)
{
  gdouble x, y, z = g.z;
  guint i, j;

  for (i = 0, x = g.x; i < g.nx; i++, x += g.dx)
    for (j = 0, y = g.y; j < g.ny; j++, y += g.dy)
      f[i][j] = x*x + y*y + z*z;
}

/* The Clebsch diagonal surface: a smooth cubic surface admitting the
 * symmetry of the tetrahedron (courtesy Johannes Beigel).
 */
static void clebsch (gdouble ** f, GtsCartesianGrid g, guint k, gpointer data)
{
  gdouble x, y, z = g.z;
  guint i, j;
  gdouble w2 = sqrt (2.0);

  for (i = 0, x = g.x; i < g.nx; i++, x += g.dx)
    for (j = 0, y = g.y; j < g.ny; j++, y += g.dy) {
      /* tetrahedral coordinates */
      gdouble p = 1. - z - w2*x;
      gdouble q = 1. - z + w2*x;
      gdouble r = 1. + z + w2*y;
      gdouble s = 1. + z - w2*y;
      /* symmetric polynomials */
      gdouble c1 = p + q + r - s;
      gdouble c2 = p*p*p + q*q*q + r*r*r - s*s*s;
      
      f[i][j] = c2 - c1*c1*c1;
    }
}

/* The Barth decic: a degree 10 surface with 345 ordinary double points.
 * (courtesy Johannes Beigel again).
 */
static void barth (gdouble ** f, GtsCartesianGrid g, guint k, gpointer data)
{
  gdouble x, y, z = g.z;
  guint i, j;
  gdouble  t = (1. + sqrt(5.))/2.;

  for (i = 0, x = g.x; i < g.nx; i++, x += g.dx)
    for (j = 0, y = g.y; j < g.ny; j++, y += g.dy) {
      gdouble t4 = t*t*t*t;
      gdouble p1 = x*x - t4*y*y;
      gdouble p2 = y*y - t4*z*z;
      gdouble p3 = z*z - t4*x*x;
      gdouble p4 = x*x*x*x + y*y*y*y + z*z*z*z 
	- 2.*x*x*y*y - 2.*y*y*z*z - 2.*x*x*z*z;
      gdouble tmp = x*x + y*y + z*z - 1.;
      gdouble q1 = tmp*tmp;
      gdouble tmp1 = x*x + y*y + z*z - (2. - t);
      gdouble q2 = tmp1*tmp1;
      
      f[i][j] = 8.*p1*p2*p3*p4 + (3. + 5.*t)*q1*q2;
    }
}

/* Returns isosurface f(x, y, z) = iso with f(x, y, z) = x*x + y*y + z*z. */
int main (int argc, char * argv[])
{
  int c = 0;
  GtsCartesianGrid g;
  GtsSurface * surface;
  gdouble iso;
  gboolean verbose = FALSE, tetra = FALSE, dual = FALSE;
  GtsIsoCartesianFunc func = sphere;

  /* parse options using getopt */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"dual", no_argument, NULL, 'd'},
      {"tetra", no_argument, NULL, 't'},
      {"barth", no_argument, NULL, 'b'},
      {"clebsch", no_argument, NULL, 'c'},
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hvbctd",
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hvbctd"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'd': /* dual */
      dual = TRUE;
      break;
    case 't': /* tetra */
      tetra = TRUE;
      break;
    case 'b': /* Barth function */
      func = barth;
      break;
    case 'c': /* Clebsch function */
      func = clebsch;
      break;
    case 'v': /* verbose */
      verbose = TRUE;
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: iso [OPTION] NX NY NZ VAL > file.gts\n"
	     "Compute the isosurface of various functions (default is a sphere).\n"
	     "The size of the cartesian mesh is given by NX, NY and NZ and the isosurface value by VAL.\n"
	     "\n"
	     "  -b    --barth    use the Barth decic function\n"
	     "  -c    --clebsch  use the Clebsch function\n"
	     "  -t    --tetra    use marching tetrahedra (default is marching cubes)\n"
             "  -d    --dual     use \"dual\" marching tetrahedra\n"
	     "  -v    --verbose  print statistics about the surface\n"
	     "  -h    --help     display this help and exit\n"
	     "\n"
	     "Reports bugs to %s\n",
	     GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `iso --help' for more information.\n");
      return 1; /* failure */
    }
  }

  if (optind >= argc) { /* missing NX */
    fprintf (stderr, 
	     "iso: missing NX\n"
	     "Try `iso --help' for more information.\n");
    return 1; /* failure */
  }
  g.nx = atoi (argv[optind]);
  
  if (optind + 1 >= argc) { /* missing NY */
    fprintf (stderr, 
	     "iso: missing NY\n"
	     "Try `iso --help' for more information.\n");
    return 1; /* failure */
  }
  g.ny = atoi (argv[optind + 1]);

  if (optind + 2 >= argc) { /* missing NZ */
    fprintf (stderr, 
	     "iso: missing NZ\n"
	     "Try `iso --help' for more information.\n");
    return 1; /* failure */
  }
  g.nz = atoi (argv[optind + 2]);

  if (optind + 3 >= argc) { /* missing VAL */
    fprintf (stderr, 
	     "iso: missing VAL\n"
	     "Try `iso --help' for more information.\n");
    return 1; /* failure */
  }
  iso = atof (argv[optind + 3]);

  /* interval is [-10:10][-10:10][-10:10] */
  g.x = -10.0; g.dx = 20./(gdouble) (g.nx - 1);
  g.y = -10.0; g.dy = 20./(gdouble) (g.ny - 1);
  g.z = -10.0; g.dz = 20./(gdouble) (g.nz - 1);

  surface = gts_surface_new (gts_surface_class (),
			     gts_face_class (),
			     gts_edge_class (),
			     gts_vertex_class ());
  if (tetra)
    gts_isosurface_tetra (surface, g, func, NULL, iso);
  else if (dual)
    gts_isosurface_tetra_bcl (surface, g, func, NULL, iso);
  else
    gts_isosurface_cartesian (surface, g, func, NULL, iso);

  if (verbose)
    gts_surface_print_stats (surface, stderr);
  gts_surface_write (surface, stdout);

  return 0;
}
